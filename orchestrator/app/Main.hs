module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, displayException, try)
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Orchestrator.AI.Client (AiConfig (..))
import Orchestrator.Database.Connection (createPool, migrateDatabase)
import Orchestrator.Discord.Bot (mkDiscordConfig, startBot)
import Orchestrator.GitHub.Client (GitHubConfig (..), defaultApiBase)
import Orchestrator.Pipeline (PipelineEnv (..), runDiscovery, runDraftGeneration)
import System.Envy (FromEnv (..), decodeEnv, env, envMaybe)
import System.Exit (exitFailure)
import System.IO (BufferMode (..), hSetBuffering, stdout)

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Top-level configuration assembled from environment variables.
data Config = Config
  { -- | PostgreSQL connection string (e.g. @postgresql://user:pass\@host:5432/db@).
    cfgDbUrl :: !String,
    -- | Google Gemini API key.
    cfgGeminiKey :: !Text,
    -- | Gemini model name (default: @gemini-2.5-flash@).
    cfgGeminiModel :: !Text,
    -- | GitHub Personal Access Token with @contents:write@ and @actions:write@.
    cfgGhToken :: !Text,
    -- | GitHub organisation or user that owns the target repository.
    cfgGhOwner :: !Text,
    -- | GitHub repository name (e.g. @daily-haskell@).
    cfgGhRepo :: !Text,
    -- | Branch to commit posts to (default: @main@).
    cfgGhBranch :: !Text,
    -- | Path inside the repo where Hugo posts live (default: @content/posts@).
    cfgGhPostsPath :: !Text,
    -- | Filename of the GitHub Actions workflow to dispatch for deploys (default: @deploy.yml@).
    cfgGhWorkflowId :: !Text,
    -- | Discord bot token (without the @Bot @ prefix).
    cfgDcBotToken :: !Text,
    -- | Discord guild (server) ID where the review channel lives.
    cfgDcGuildId :: !Int,
    -- | Discord channel ID where draft review messages are posted.
    cfgDcChannelId :: !Int,
    -- | How often to run the discovery step, in seconds (default: 86400 = 1 day).
    cfgDiscoveryIntervalSecs :: !Int,
    -- | How often to run the draft-generation step, in seconds (default: 43200 = 12 hours).
    cfgDraftIntervalSecs :: !Int
  }

instance FromEnv Config where
  fromEnv _ =
    Config
      <$> env "DATABASE_URL"
      <*> env "GEMINI_API_KEY"
      <*> (fromMaybe "gemini-2.5-flash" <$> envMaybe "GEMINI_MODEL")
      <*> env "GITHUB_TOKEN"
      <*> env "GITHUB_REPO_OWNER"
      <*> env "GITHUB_REPO_NAME"
      <*> (fromMaybe "main" <$> envMaybe "GITHUB_BRANCH")
      <*> (fromMaybe "content/posts" <$> envMaybe "GITHUB_POSTS_PATH")
      <*> (fromMaybe "deploy.yml" <$> envMaybe "GITHUB_WORKFLOW_ID")
      <*> env "DISCORD_BOT_TOKEN"
      <*> env "DISCORD_GUILD_ID"
      <*> env "DISCORD_CHANNEL_ID"
      <*> (fromMaybe 86400 <$> envMaybe "DISCOVERY_INTERVAL_SECS")
      <*> (fromMaybe 43200 <$> envMaybe "DRAFT_INTERVAL_SECS")

-- ---------------------------------------------------------------------------
-- Utilities
-- ---------------------------------------------------------------------------

-- | Sleep for @n@ seconds (converts to microseconds for 'threadDelay').
sleepSecs :: Int -> IO ()
sleepSecs n = threadDelay (n * 1_000_000)

-- | Run an action immediately, then repeat it on a fixed interval.
-- Exceptions are caught, logged with the given prefix, and the loop continues.
scheduledLoop :: String -> Int -> IO () -> IO ()
scheduledLoop prefix intervalSecs action = do
  result <- try action :: IO (Either SomeException ())
  case result of
    Left ex -> putStrLn $ prefix <> " Error: " <> displayException ex
    Right () -> pure ()
  putStrLn $ prefix <> " Sleeping for " <> show intervalSecs <> "s."
  sleepSecs intervalSecs
  scheduledLoop prefix intervalSecs action

-- | Build an 'AiConfig' from the top-level config and a shared HTTP manager.
mkAiConfig :: Config -> Manager -> AiConfig
mkAiConfig cfg manager =
  AiConfig
    { aiApiKey = cfgGeminiKey cfg,
      aiModel = cfgGeminiModel cfg,
      aiManager = manager
    }

-- | Build a 'GitHubConfig' from the top-level config and a shared HTTP manager.
mkGhConfig :: Config -> Manager -> GitHubConfig
mkGhConfig cfg manager =
  GitHubConfig
    { ghToken = cfgGhToken cfg,
      ghRepoOwner = cfgGhOwner cfg,
      ghRepoName = cfgGhRepo cfg,
      ghBranch = cfgGhBranch cfg,
      ghPostsPath = cfgGhPostsPath cfg,
      ghWorkflowId = cfgGhWorkflowId cfg,
      ghManager = manager,
      ghApiBase = defaultApiBase
    }

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "[Jarvis] Starting orchestrator..."

  cfg <- loadConfig
  manager <- newTlsManager

  let aiCfg = mkAiConfig cfg manager
      ghCfg = mkGhConfig cfg manager

  dcCfg <-
    mkDiscordConfig
      (cfgDcBotToken cfg)
      (fromIntegral (cfgDcGuildId cfg))
      (fromIntegral (cfgDcChannelId cfg))

  pool <- createPool (BC.pack (cfgDbUrl cfg))
  migrateDatabase pool
  putStrLn "[Jarvis] Database ready."

  let pipeEnv =
        PipelineEnv
          { pipeAiCfg = aiCfg,
            pipeGhCfg = ghCfg,
            pipeDcCfg = dcCfg,
            pipeDbPool = pool
          }

  _ <-
    forkIO $
      scheduledLoop
        "[Discovery]"
        (cfgDiscoveryIntervalSecs cfg)
        (runDiscovery pipeEnv)

  _ <-
    forkIO $
      scheduledLoop
        "[Drafts]"
        (cfgDraftIntervalSecs cfg)
        (runDraftGeneration pipeEnv)

  putStrLn "[Jarvis] All workers started. Discord bot running..."
  startBot dcCfg
  where
    loadConfig =
      decodeEnv >>= \case
        Left err -> putStrLn ("[Jarvis] Configuration error: " <> err) >> exitFailure
        Right c -> pure c

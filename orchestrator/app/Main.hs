{-# LANGUAGE RecursiveDo #-}

module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, displayException, try)
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Word (Word64)
import qualified Data.Text as T
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Orchestrator.AI.Client (AiConfig (..))
import Orchestrator.Database.Connection (createPool, migrateDatabase)
import Orchestrator.Discord.Bot (DiscordBotSettings (..), mkDiscordConfig, startBot)
import Orchestrator.GitHub.Client (GitHubConfig (..), defaultApiBase)
import Orchestrator.Pipeline (PipelineEnv (..), createSubject, disableSubject, handleApproveReview, handleCustomPostRequest, handleRejectReview, handleReviseRequest, listEnabledSubjects, retryFailedDrafts, runDiscovery, runDraftGeneration)
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
    -- | Gemini models in priority order (default: see 'defaultGeminiModels').
    -- On a rate-limit error the next model in the list is tried automatically.
    cfgGeminiModels :: ![Text],
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
    cfgDcGuildId :: !Word64,
    -- | Discord channel ID where draft review messages are posted.
    cfgDcChannelId :: !Word64,
    -- | Discord text channel ID used for slash commands and bot notices.
    cfgDcInteractionChannelId :: !Word64,
    -- | Discord user ID of the bot owner (only this user's interactions are processed).
    cfgDcOwnerId :: !Word64,
    -- | How often to run the discovery step, in seconds (default: 86400 = 1 day).
    cfgDiscoveryIntervalSecs :: !Int,
    -- | How often to run the draft-generation step, in seconds (default: 43200 = 12 hours).
    cfgDraftIntervalSecs :: !Int,
    -- | How often to retry drafts that failed to publish, in seconds (default: 3600 = 1 hour).
    cfgRetryIntervalSecs :: !Int
  }

instance FromEnv Config where
  fromEnv _ =
    Config
      <$> env "DATABASE_URL"
      <*> env "GEMINI_API_KEY"
      <*> (maybe defaultGeminiModels parseGeminiModels <$> envMaybe "GEMINI_MODELS")
      <*> env "GITHUB_TOKEN"
      <*> env "GITHUB_REPO_OWNER"
      <*> env "GITHUB_REPO_NAME"
      <*> (fromMaybe "main" <$> envMaybe "GITHUB_BRANCH")
      <*> (fromMaybe "content/posts" <$> envMaybe "GITHUB_POSTS_PATH")
      <*> (fromMaybe "deploy.yml" <$> envMaybe "GITHUB_WORKFLOW_ID")
      <*> env "DISCORD_BOT_TOKEN"
      <*> env "DISCORD_GUILD_ID"
      <*> env "DISCORD_CHANNEL_ID"
      <*> env "DISCORD_INTERACTION_CHANNEL_ID"
      <*> env "DISCORD_OWNER_ID"
      <*> (fromMaybe 86400 <$> envMaybe "DISCOVERY_INTERVAL_SECS")
      <*> (fromMaybe 43200 <$> envMaybe "DRAFT_INTERVAL_SECS")
      <*> (fromMaybe 3600 <$> envMaybe "RETRY_INTERVAL_SECS")

-- ---------------------------------------------------------------------------
-- Utilities
-- ---------------------------------------------------------------------------

-- | Ordered list of Gemini models to try, highest-priority first.
-- Used when 'GEMINI_MODELS' is not set in the environment.
defaultGeminiModels :: [Text]
defaultGeminiModels =
  [ "gemini-2.5-flash-lite",
    "gemini-2.5-flash"
  ]

-- | Parse a comma-separated list of model names from the 'GEMINI_MODELS'
-- environment variable, dropping any blank entries.
parseGeminiModels :: Text -> [Text]
parseGeminiModels = filter (not . T.null) . map T.strip . T.splitOn ","

-- | Sleep for @n@ seconds (converts to microseconds for 'threadDelay').
sleepSecs :: Int -> IO ()
sleepSecs n = threadDelay (n * 1_000_000)

-- | Sleep for @intervalSecs@, run an action, then repeat.
-- The initial sleep means the workers do not fire immediately on startup.
-- Exceptions are caught, logged with the given prefix, and the loop continues.
scheduledLoop :: String -> Int -> IO () -> IO ()
scheduledLoop prefix intervalSecs action = do
  putStrLn $ prefix <> " Sleeping for " <> show intervalSecs <> "s."
  sleepSecs intervalSecs
  result <- try action :: IO (Either SomeException ())
  case result of
    Left ex -> putStrLn $ prefix <> " Error: " <> displayException ex
    Right () -> pure ()
  scheduledLoop prefix intervalSecs action

-- | Build an 'AiConfig' from the top-level config and a shared HTTP manager.
mkAiConfig :: Config -> Manager -> AiConfig
mkAiConfig cfg manager =
  AiConfig
    { aiApiKey = cfgGeminiKey cfg,
      aiModels = cfgGeminiModels cfg,
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

  pool <- createPool (BC.pack (cfgDbUrl cfg))
  migrateDatabase pool
  putStrLn "[Jarvis] Database ready."

  -- RecursiveDo is needed here because 'dcCfg' depends on 'pipeEnv'
  -- (callbacks are bound as 'pipeEnv' closures) while 'pipeEnv' depends on
  -- 'dcCfg'.  The 'rec' block threads the knot via lazy evaluation of the
  -- 'let' binding, so both values are available by the time IO actions run.
  rec dcCfg <-
        mkDiscordConfig
          DiscordBotSettings
            { dbsBotToken = cfgDcBotToken cfg,
              dbsGuildId = fromIntegral (cfgDcGuildId cfg),
              dbsChannelId = fromIntegral (cfgDcChannelId cfg),
              dbsInteractionChannelId = fromIntegral (cfgDcInteractionChannelId cfg),
              dbsOwnerId = fromIntegral (cfgDcOwnerId cfg),
              dbsOnDiscoverCommand = runDiscovery pipeEnv,
              dbsOnDraftCommand = runDraftGeneration pipeEnv,
              dbsOnSubjectCommand = createSubject pipeEnv,
              dbsOnDisableSubjectCommand = disableSubject pipeEnv,
              dbsOnListSubjectsCommand = listEnabledSubjects pipeEnv,
              dbsOnApproveReview = handleApproveReview pipeEnv,
              dbsOnRejectReview = handleRejectReview pipeEnv,
              dbsOnReviseRequest = handleReviseRequest pipeEnv,
              dbsOnCustomPostRequest = handleCustomPostRequest pipeEnv
            }
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

  _ <-
    forkIO $
      scheduledLoop
        "[Retry]"
        (cfgRetryIntervalSecs cfg)
        (retryFailedDrafts pipeEnv)

  putStrLn "[Jarvis] All workers started. Discord bot running..."
  startBot dcCfg
  where
    loadConfig =
      decodeEnv >>= \case
        Left err -> putStrLn ("[Jarvis] Configuration error: " <> err) >> exitFailure
        Right c -> pure c

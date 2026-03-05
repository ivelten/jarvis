module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, try)
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist.Sql (entityKey, entityVal, update, (=.))
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Orchestrator.AI.Client
  ( AiConfig (..),
    DiscoveredContent (..),
    GeneratedDraft (..),
    generateDraft,
    reviseDraft,
    toSlug,
  )
import Orchestrator.Database.Connection (DbPool, createPool, migrateDatabase, runDb)
import Orchestrator.Database.Entities
import Orchestrator.Database.Models (ContentStatus (..))
import Orchestrator.Discord.Bot
  ( DiscordConfig (..),
    ReviewRequest (..),
    mkDiscordConfig,
    registerForReview,
    startBot,
  )
import Orchestrator.GitHub.Client (GitHubConfig (..), commitPost, defaultApiBase, triggerDeploy)
import Orchestrator.Posts.Generator (renderHugoPost)
import Orchestrator.Topics.Selector (ingestDiscoveredContent, pendingContent)
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
    -- | Gemini model name (default: @gemini-2.0-flash@).
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
    -- | How often to run the draft-generation step, in seconds (default: 3600 = 1 hour).
    cfgDraftIntervalSecs :: !Int
  }

instance FromEnv Config where
  fromEnv _ =
    Config
      <$> env "DATABASE_URL"
      <*> env "GEMINI_API_KEY"
      <*> (fromMaybe "gemini-2.0-flash" <$> envMaybe "GEMINI_MODEL")
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
      <*> (fromMaybe 3600 <$> envMaybe "DRAFT_INTERVAL_SECS")

-- ---------------------------------------------------------------------------
-- Independent pipeline steps
-- ---------------------------------------------------------------------------

-- | Ask Gemini to discover new content and persist it to the database.
-- Runs on its own schedule, independently of draft generation.
runDiscovery :: AiConfig -> DbPool -> IO ()
runDiscovery aiCfg pool = do
  putStrLn "[Discovery] Discovering content via Gemini..."
  runDb pool (ingestDiscoveredContent aiCfg)
  putStrLn "[Discovery] Done."

-- | Pick the next pending content item, generate a Markdown draft, and
-- register it for Discord review.  The approve/reject callbacks fire
-- as soon as the reviewer reacts or types an approval in the Discord thread.
runDraftGeneration :: AiConfig -> GitHubConfig -> DiscordConfig -> DbPool -> IO ()
runDraftGeneration aiCfg ghCfg dcCfg pool = do
  pending <- runDb pool pendingContent
  case pending of
    [] -> putStrLn "[Drafts] No pending content to process. Skipping."
    (item : _) -> processDraft (entityKey item) (entityVal item)
  where
    processDraft rcKey rc = do
      putStrLn $ "[Drafts] Generating draft for: " <> T.unpack (rawContentTitle rc)
      draft <- generateDraft aiCfg [rcToDiscovered rc]
      now <- getCurrentTime
      let title = gdTitle draft
          slug = toSlug title
          filename = slug <> ".md"
          revise currentBody feedback =
            gdBody <$> reviseDraft aiCfg title currentBody feedback
      -- Mark as drafted immediately so restarts don't re-pick this item.
      runDb pool $
        update
          rcKey
          [ RawContentStatus =. ContentDrafted,
            RawContentUpdatedAt =. now
          ]
      putStrLn "[Drafts] Draft sent to Discord for review."
      registerForReview
        dcCfg
        ReviewRequest
          { rrTitle = title,
            rrBody = gdBody draft,
            rrRevise = revise,
            rrApprove = onApprove filename title slug now (gdTags draft),
            rrReject = onReject rcKey
          }

    onApprove filename title slug now tags finalBody = do
      let mdContent = renderHugoPost title slug now tags finalBody
      putStrLn $ "[Drafts] Approved! Committing " <> T.unpack filename <> " to GitHub..."
      commitPost ghCfg filename mdContent
      putStrLn "[Drafts] Triggering deploy workflow..."
      triggerDeploy ghCfg
      putStrLn "[Drafts] Deployed."

    onReject rcKey reason = do
      rejectedAt <- getCurrentTime
      runDb pool $
        update
          rcKey
          [ RawContentStatus =. ContentRejected,
            RawContentUpdatedAt =. rejectedAt
          ]
      putStrLn $ "[Drafts] Draft rejected: " <> T.unpack reason

-- ---------------------------------------------------------------------------
-- Utilities
-- ---------------------------------------------------------------------------

-- | Convert a database 'RawContent' row into a 'DiscoveredContent' value
--   suitable for passing to 'generateDraft'.
rcToDiscovered :: RawContent -> DiscoveredContent
rcToDiscovered rc =
  DiscoveredContent
    { dcTitle = rawContentTitle rc,
      dcUrl = rawContentUrl rc,
      dcSummary = rawContentSummary rc,
      dcSubject = Nothing
    }

-- | Sleep for @n@ seconds (converts to microseconds for 'threadDelay').
sleepSecs :: Int -> IO ()
sleepSecs n = threadDelay (n * 1_000_000)

-- | Run an action immediately, then repeat it on a fixed interval.
-- Exceptions are caught, logged with the given prefix, and the loop continues.
scheduledLoop :: String -> Int -> IO () -> IO ()
scheduledLoop prefix intervalSecs action = do
  result <- try action :: IO (Either SomeException ())
  case result of
    Left ex -> putStrLn $ prefix <> " Error: " <> show ex
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

  _ <-
    forkIO $
      scheduledLoop
        "[Discovery]"
        (cfgDiscoveryIntervalSecs cfg)
        (runDiscovery aiCfg pool)

  _ <-
    forkIO $
      scheduledLoop
        "[Drafts]"
        (cfgDraftIntervalSecs cfg)
        (runDraftGeneration aiCfg ghCfg dcCfg pool)

  putStrLn "[Jarvis] All workers started. Discord bot running..."
  startBot dcCfg
  where
    loadConfig =
      decodeEnv >>= \case
        Left err -> putStrLn ("[Jarvis] Configuration error: " <> err) >> exitFailure
        Right c -> pure c

module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, try)
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist.Sql (entityVal)
import Network.HTTP.Client.TLS (newTlsManager)
import Orchestrator.AI.Client
  ( AiConfig (..),
    DiscoveredContent (..),
    GeneratedDraft (..),
    discoverContent,
    generateDraft,
  )
import Orchestrator.Database.Connection (DbPool, createPool, migrateDatabase, runDb)
import Orchestrator.Database.Entities (RawContent (..))
import Orchestrator.Discord.Bot
  ( DiscordConfig (..),
    ReviewResult (..),
    awaitReview,
    mkDiscordConfig,
    sendForReview,
    startBot,
  )
import Orchestrator.GitHub.Client (GitHubConfig (..), commitPost, triggerDeploy)
import Orchestrator.Posts.Generator (renderHugoPost)
import Orchestrator.Topics.Selector (ingestDiscoveredContent, pendingContent)
import System.Envy (FromEnv (..), decodeEnv, env, envMaybe)
import System.Exit (exitFailure)
import System.IO (BufferMode (..), hSetBuffering, stdout)

-- | Top-level configuration assembled from environment variables.
data Config = Config
  { cfgDbUrl :: !String,
    cfgGeminiKey :: !Text,
    cfgGeminiModel :: !Text,
    cfgGhToken :: !Text,
    cfgGhOwner :: !Text,
    cfgGhRepo :: !Text,
    cfgGhBranch :: !Text,
    cfgGhPostsPath :: !Text,
    cfgGhWorkflowId :: !Text,
    cfgDcBotToken :: !Text,
    cfgDcGuildId :: !Int,
    cfgDcChannelId :: !Int,
    -- | How often to run the pipeline, in seconds (default: 86400 = 1 day).
    cfgIntervalSecs :: !Int
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
      <*> (fromMaybe 86400 <$> envMaybe "PIPELINE_INTERVAL_SECS")

-- | Run one full cycle of the orchestration pipeline:
--
--   1. Ask Gemini to discover new content and persist it.
--   2. Pick an unreviewed item from the queue.
--   3. Generate a Markdown draft.
--   4. Send the draft to Discord for review.
--   5. On approval, commit the post and trigger a deploy.
runPipeline :: AiConfig -> GitHubConfig -> DiscordConfig -> DbPool -> IO ()
runPipeline aiCfg ghCfg dcCfg pool = do
  -- Step 1: discover & ingest
  putStrLn "[Pipeline] Discovering content..."
  runDb pool (ingestDiscoveredContent aiCfg)

  -- Step 2: pick a pending item
  pending <- runDb pool pendingContent
  case pending of
    [] -> putStrLn "[Pipeline] No pending content to process. Skipping."
    (item : _) -> do
      let rc = entityVal item

      -- Step 3: generate draft
      putStrLn $ "[Pipeline] Generating draft for: " <> T.unpack (rawContentTitle rc)
      draft <- generateDraft aiCfg [rcToDiscovered rc]
      now <- getCurrentTime

      let filename = toSlug (gdTitle draft) <> ".md"
          mdContent =
            renderHugoPost
              (gdTitle draft)
              (toSlug (gdTitle draft))
              now
              []
              (gdBody draft)

      -- Step 4: Discord review
      putStrLn "[Pipeline] Sending draft for review on Discord..."
      msgId <- sendForReview dcCfg (gdTitle draft) (gdBody draft)
      result <- awaitReview dcCfg msgId

      case result of
        Rejected reason ->
          putStrLn $ "[Pipeline] Draft rejected: " <> T.unpack reason
        Approved -> do
          -- Step 5: commit & deploy
          putStrLn "[Pipeline] Approved! Committing post to GitHub..."
          commitPost ghCfg filename mdContent
          putStrLn "[Pipeline] Triggering deploy workflow..."
          triggerDeploy ghCfg
          putStrLn "[Pipeline] Done."

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

-- | Convert a post title to a URL-safe slug for filenames.
toSlug :: Text -> Text
toSlug =
  T.intercalate "-"
    . filter (not . T.null)
    . T.splitOn " "
    . T.toLower

-- | Sleep for @n@ seconds (converts to microseconds for 'threadDelay').
sleepSecs :: Int -> IO ()
sleepSecs n = threadDelay (n * 1_000_000)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "[Jarvis] Starting orchestrator..."

  -- Load all configuration from environment variables.
  cfg <-
    decodeEnv >>= \case
      Left err -> do
        putStrLn $ "[Jarvis] Configuration error: " <> err
        exitFailure
      Right c -> pure c

  -- One shared TLS-capable HTTP manager for Gemini and GitHub calls.
  manager <- newTlsManager

  let aiCfg =
        AiConfig
          { aiApiKey = cfgGeminiKey cfg,
            aiModel = cfgGeminiModel cfg,
            aiManager = manager
          }
      ghCfg =
        GitHubConfig
          { ghToken = cfgGhToken cfg,
            ghRepoOwner = cfgGhOwner cfg,
            ghRepoName = cfgGhRepo cfg,
            ghBranch = cfgGhBranch cfg,
            ghPostsPath = cfgGhPostsPath cfg,
            ghWorkflowId = cfgGhWorkflowId cfg,
            ghManager = manager
          }

  -- Allocate the Discord bot config with its internal communication state.
  dcCfg <-
    mkDiscordConfig
      (cfgDcBotToken cfg)
      (fromIntegral (cfgDcGuildId cfg))
      (fromIntegral (cfgDcChannelId cfg))

  -- Set up the database pool and run schema migrations.
  pool <- createPool (BC.pack (cfgDbUrl cfg))
  migrateDatabase pool
  putStrLn "[Jarvis] Database ready."

  -- Start the Discord bot event loop in a background thread.
  _ <- forkIO $ startBot dcCfg
  putStrLn "[Jarvis] Discord bot started."

  -- Run the pipeline immediately, then on a repeating schedule.
  let loop = do
        result <-
          try (runPipeline aiCfg ghCfg dcCfg pool) ::
            IO (Either SomeException ())
        case result of
          Left ex -> putStrLn $ "[Jarvis] Pipeline error: " <> show ex
          Right () -> pure ()
        putStrLn $
          "[Jarvis] Sleeping for "
            <> show (cfgIntervalSecs cfg)
            <> " seconds until next run."
        sleepSecs (cfgIntervalSecs cfg)
        loop

  putStrLn "[Jarvis] Orchestrator running."
  loop

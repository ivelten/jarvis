module Main (main) where

import Orchestrator.Database.Connection (createPool, migrateDatabase)
import System.IO (BufferMode (..), hSetBuffering, stdout)

-- | Entry point. Reads configuration from the environment, sets up the
-- database pool, runs migrations, and starts the orchestration loop.
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  putStrLn "[Jarvis] Starting orchestrator..."

  -- TODO: load config from environment variables (using envy)
  -- cfg <- loadConfig

  -- TODO: create DB pool
  -- pool <- createPool (dbConnStr cfg)
  -- migrateDatabase pool

  -- TODO: start the Discord bot in its own thread
  -- forkIO $ startBot (dcConfig cfg)

  -- TODO: run the main orchestration loop
  -- runLoop cfg pool

  putStrLn "[Jarvis] Orchestrator running. Press Ctrl+C to stop."
  -- Block forever for now
  _ <- getLine
  pure ()

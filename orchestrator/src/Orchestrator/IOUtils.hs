-- | Lightweight IO utilities shared across the orchestrator.
module Orchestrator.IOUtils
  ( tryIO,
    tryLog,
  )
where

import Control.Exception (SomeException, displayException, try)

-- | Specialise 'try' to 'SomeException' so call sites don't need inline type
-- annotations.
tryIO :: IO a -> IO (Either SomeException a)
tryIO = try

-- | Run an IO action, catching any exception and logging it with the given
-- prefix.  Always continues after logging; use this for non-fatal steps.
tryLog :: String -> IO () -> IO ()
tryLog prefix action = do
  result <- tryIO action
  case result of
    Left ex -> putStrLn $ prefix <> ": " <> displayException ex
    Right () -> pure ()

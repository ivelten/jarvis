-- | Lightweight IO utilities shared across the orchestrator.
module Orchestrator.IOUtils
  ( tryIO,
    tryLog,
    logMsg,
  )
where

import Control.Exception (SomeException, displayException, try)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- | Specialise 'try' to 'SomeException' so call sites don't need inline type
-- annotations.
tryIO :: IO a -> IO (Either SomeException a)
tryIO = try

-- | Print a 'Text' message to stdout, followed by a newline.
-- Prefer this over 'putStrLn' to avoid 'T.unpack' at every call site.
logMsg :: Text -> IO ()
logMsg = TIO.putStrLn

-- | Run an IO action, catching any exception and logging it with the given
-- prefix.  Always continues after logging; use this for non-fatal steps.
tryLog :: Text -> IO () -> IO ()
tryLog prefix action = do
  result <- tryIO action
  case result of
    Left ex -> logMsg $ prefix <> ": " <> T.pack (displayException ex)
    Right () -> pure ()

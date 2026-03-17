-- | Typed application errors for the orchestrator.
--
-- All domain-specific exceptions are represented as constructors of 'AppError'
-- so that error-handling code can pattern-match on the error kind rather than
-- inspecting exception messages as strings.
module Orchestrator.Error
  ( AppError (..),
    tryAppError,
  )
where

import Control.Exception (Exception, try)
import Data.Text (Text, unpack)

-- | All typed errors that can be thrown by the orchestrator.
--
-- The 'Show' instance produces human-readable messages; since 'displayException'
-- defaults to 'show', all logging via @displayException@ will use these strings.
data AppError
  = -- | Gemini rate-limit (HTTP 429 \/ RESOURCE_EXHAUSTED).
    AiRateLimit Text
  | -- | Gemini returned a response with no extractable text.
    AiEmptyResponse Text
  | -- | Any other Gemini API or parse error.
    AiApiError Text
  | -- | GitHub Contents API commit failed.
    GitHubCommitError Int Text
  | -- | GitHub Actions workflow dispatch failed.
    GitHubDeployError Int Text

instance Show AppError where
  show (AiRateLimit msg) = "AI rate limit: " <> unpack msg
  show (AiEmptyResponse msg) = "AI empty response: " <> unpack msg
  show (AiApiError msg) = "AI API error: " <> unpack msg
  show (GitHubCommitError sc body) = "GitHub commit failed (" <> show sc <> "): " <> unpack body
  show (GitHubDeployError sc body) = "GitHub deploy failed (" <> show sc <> "): " <> unpack body

instance Exception AppError

-- | Specialise 'try' to 'AppError' so call sites don't need inline type
-- annotations.
tryAppError :: IO a -> IO (Either AppError a)
tryAppError = try

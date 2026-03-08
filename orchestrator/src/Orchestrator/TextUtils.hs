module Orchestrator.TextUtils
  ( truncateText,
    splitTitle,
    toSlug,
    emojiApprove,
    emojiDraft,
    emojiQueue,
    emojiReject,
    emojiRevise,
    emojiSearch,
    emojiStar,
    emojiWarning,
  )
where

import Data.Char (isAlphaNum, toLower)
import Data.Text (Text)
import qualified Data.Text as T

-- | Truncate text to at most @maxLen@ characters, breaking at the last word
-- boundary within the limit, stripping trailing punctuation, and appending
-- @'\\x2026'@ (…) when the text is actually truncated.
--
-- If the text fits within @maxLen@ it is returned unchanged.
truncateText :: Int -> Text -> Text
truncateText maxLen t
  | T.length t <= maxLen = t
  | otherwise =
      let window = T.take maxLen t
          trimmed = case T.breakOnEnd " " window of
            (pre, _) | not (T.null pre) -> T.dropWhileEnd (== ' ') pre
            _ -> window
          clean = T.dropWhileEnd (`elem` (",;:—-" :: String)) trimmed
       in clean <> "\x2026"

-- | Split the first Markdown H1 heading from the document body.
-- Returns @(title, body)@ where @title@ has the leading @#@ stripped.
-- Returns @(\"Untitled\", input)@ for empty input.
splitTitle :: Text -> (Text, Text)
splitTitle txt =
  case T.lines txt of
    (h : rest)
      | "#" `T.isPrefixOf` h -> (T.strip (T.dropWhile (== '#') h), T.unlines rest)
      | otherwise -> (T.strip h, T.unlines rest)
    [] -> ("Untitled", txt)

-- | Convert a title into a URL-safe slug: lowercase, non-alphanumeric
-- characters replaced with spaces, words joined with hyphens.
--
-- >>> toSlug "Hello, World!"
-- "hello-world"
toSlug :: Text -> Text
toSlug =
  T.intercalate "-"
    . filter (not . T.null)
    . T.splitOn " "
    . T.map (\c -> if isAlphaNum c || c == ' ' then toLower c else ' ')

-- Discord emoji constants --------------------------------------------------

-- | Unicode emoji constants for use in Discord messages and reactions.
emojiApprove, emojiDraft, emojiQueue, emojiReject, emojiRevise, emojiSearch, emojiStar, emojiWarning :: Text
emojiApprove = "\x2705" -- ✅
emojiReject = "\x274c" -- ❌
emojiDraft = "\x1f4dd" -- 📝
emojiQueue = "\x1f4ec" -- 📬
emojiRevise = "\x270f\xfe0f" -- ✏️
emojiSearch = "\x1f50d" -- 🔍
emojiStar = "\x2b50" -- ⭐
emojiWarning = "\x26a0\xfe0f" -- ⚠️

module Orchestrator.TextUtils
  ( truncateText,
    chunkText,
    splitTitle,
    toSlug,
  )
where

import Data.Char (isAlphaNum, toLower)
import Data.Maybe (fromMaybe)
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

-- | Split 'Text' into chunks of at most @maxLen@ characters.
-- Break priority (highest to lowest):
--
--   1. Last @\\n\\n@ paragraph boundary within the window
--   2. Last @\\n@ line boundary within the window
--   3. Last word boundary (space) within the window
--   4. Hard cut at @maxLen@ (only when no whitespace exists at all,
--      e.g. an extremely long URL or code token)
--
-- Code fences are kept coherent: when a split falls inside an open fence the
-- current chunk is closed with @```@ and the next chunk reopens it with the
-- same fence header.  Splitting right at the fence opener line (with no code
-- content in the chunk) is detected and recovered by forcing a split within the
-- code content that follows, so a bare opener is never emitted as a message.
chunkText :: Int -> Text -> [Text]
chunkText maxLen = filter (not . T.null) . map T.strip . go
  where
    go t
      | T.length t <= maxLen = [t]
      | otherwise =
          let window = T.take maxLen t
              remaining = T.drop maxLen t
              (chunk, leftover) = bestSplit window
           in case currentFenceOpener chunk of
                Nothing ->
                  chunk : go (leftover <> remaining)
                Just opener ->
                  let opLen = T.length opener + 1 -- opener + '\n'
                   in if opLen >= T.length chunk
                        then
                          let (altChunk, altLeft) =
                                bestSplitWithMinPrefix opLen (chunk <> leftover)
                           in case currentFenceOpener altChunk of
                                Nothing ->
                                  altChunk : go (altLeft <> remaining)
                                Just ro ->
                                  if T.length ro + 1 >= T.length altChunk
                                    then altChunk : go (altLeft <> remaining)
                                    else (altChunk <> "\n```") : go (ro <> "\n" <> altLeft <> remaining)
                        else
                          (chunk <> "\n```") : go (opener <> "\n" <> leftover <> remaining)

    -- \| Like 'bestSplit' but treats the first @minPfx@ characters of the
    -- window as a fixed prefix — the split is only searched for in the
    -- remainder, preventing the opener line from being chosen as the split
    -- point.
    bestSplitWithMinPrefix minPfx window =
      let prefix = T.take minPfx window
          rest = T.drop minPfx window
       in if T.null rest
            then (window, "")
            else
              let (c, l) = bestSplit rest
               in (prefix <> c, l)

    bestSplit window =
      repairOrphanedBracket $
        repairMarkdownLink $
          case breakLast "\n\n" window of
            Just p -> p
            Nothing -> case breakLast "\n" window of
              Just p -> p
              Nothing -> fromMaybe (window, "") (breakLastWord window)

    -- \| If the leftover starts with @[…@ that is NOT a Markdown link (i.e.
    -- no @](@ found within its first 200 characters), retreat the split to the
    -- last newline boundary inside the chunk.  This prevents a bare token such
    -- as @[JSON]@ — a continuation line inside a list item — from opening a
    -- new Discord message without its surrounding list context.
    repairOrphanedBracket :: (Text, Text) -> (Text, Text)
    repairOrphanedBracket p@(chunk, leftover) =
      let stripped = T.dropWhile (== ' ') leftover
       in if "[" `T.isPrefixOf` stripped
            && not ("]( " `T.isInfixOf` T.take 200 stripped)
            && not ("](" `T.isInfixOf` T.take 200 stripped)
            then case breakLast "\n" (T.stripEnd chunk) of
              Just (lineChunk, _)
                | not (T.null (T.stripEnd lineChunk)) ->
                    let lc = T.stripEnd lineChunk
                     in (lc, T.drop (T.length lc) (chunk <> leftover))
              _ -> p
            else p

    -- \| If the chunk ends with an unclosed Markdown link opener @[…@ (i.e. a
    -- @[@ with no subsequent @](@ before the end of the chunk), retreat the
    -- split to just before that @[@.  This prevents links in the Further
    -- Reading section — or anywhere else — from being torn across two
    -- Discord messages.
    repairMarkdownLink p@(chunk, leftover) =
      case T.breakOnEnd "[" chunk of
        ("", _) -> p -- no '[' in chunk, nothing to repair
        (beforeIncl, afterOpen)
          | "](" `T.isInfixOf` afterOpen -> p -- '[' is properly closed within the chunk
          | otherwise ->
              let safeChunk = T.stripEnd (T.dropEnd 1 beforeIncl)
               in if T.null safeChunk
                    then p -- can't retreat further; leave as-is to avoid an infinite loop
                    else (safeChunk, T.drop (T.length safeChunk) (chunk <> leftover))

    currentFenceOpener :: Text -> Maybe Text
    currentFenceOpener = stepLines Nothing . T.lines
      where
        stepLines state [] = state
        stepLines Nothing (l : ls)
          | "```" `T.isPrefixOf` T.strip l = stepLines (Just (T.strip l)) ls
          | otherwise = stepLines Nothing ls
        stepLines (Just opener) (l : ls)
          | T.strip l == "```" = stepLines Nothing ls
          | otherwise = stepLines (Just opener) ls

    breakLast sep t =
      let parts = T.splitOn sep t
       in if length parts <= 1
            then Nothing
            else Just (T.intercalate sep (init parts) <> sep, last parts)

    breakLastWord t =
      case T.breakOnEnd " " t of
        ("", _) -> Nothing
        (pre, suf) -> Just (pre, suf)

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

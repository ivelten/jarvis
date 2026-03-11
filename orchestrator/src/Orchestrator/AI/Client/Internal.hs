-- | Internal helpers for 'Orchestrator.AI.Client'.
--
-- These are exported for testing purposes only.
-- Do not import this module from production code — use 'Orchestrator.AI.Client' instead.
module Orchestrator.AI.Client.Internal
  ( -- * Domain types (also re-exported by 'Orchestrator.AI.Client')
    DiscoveredContent (..),
    ModelItem (..),

    -- * JSON extraction helpers
    extractText,
    extractFinishReason,
    extractTokenCount,
    extractGroundingChunks,

    -- * URL resolution helpers
    resolveRedirectUrl,
    mergeWithChunks,
    extractQParam,
    followRedirect,

    -- * Draft parsing helpers
    parseTagsLine,
    parseBilingualResponse,
  )
where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Network.HTTP.Client
  ( Manager,
    Request (..),
    httpLbs,
    parseRequest,
    responseHeaders,
  )

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | A piece of web content discovered by the AI.
data DiscoveredContent = DiscoveredContent
  { dcTitle :: !Text,
    dcUrl :: !Text,
    dcSummary :: !Text,
    -- | Subject name hints; each is matched against the Subject table at ingest time.
    dcSubjects :: ![Text]
  }
  deriving (Show, Eq, Generic)

-- | Raw model output from the discovery prompt (no URL — URLs come from
-- grounding chunks via 'mergeWithChunks').
data ModelItem = ModelItem
  { miTitle :: !Text,
    miSummary :: !Text,
    miSubjects :: ![Text]
  }
  deriving (Show, Eq, Generic)

instance FromJSON ModelItem where
  parseJSON = withObject "ModelItem" $ \o ->
    ModelItem
      <$> o .: "title"
      <*> o .: "summary"
      <*> o .:? "subjects" .!= []

instance FromJSON DiscoveredContent where
  parseJSON = withObject "DiscoveredContent" $ \o ->
    DiscoveredContent
      <$> o .: "title"
      <*> o .: "url"
      <*> o .: "summary"
      <*> o .:? "subjects" .!= []

instance ToJSON DiscoveredContent where
  toJSON DiscoveredContent {..} =
    object
      [ "title" .= dcTitle,
        "url" .= dcUrl,
        "summary" .= dcSummary,
        "subjects" .= dcSubjects
      ]

-- ---------------------------------------------------------------------------
-- JSON extraction helpers
-- ---------------------------------------------------------------------------

-- | Extract @usageMetadata.totalTokenCount@ from a Gemini response.
-- Returns 0 when the field is absent (e.g. in grounded-search responses).
extractTokenCount :: Value -> Int
extractTokenCount = fromMaybe 0 . parseMaybe go
  where
    go = withObject "GeminiResponse" $ \o -> do
      usage <- o .: "usageMetadata"
      usage .: "totalTokenCount"

-- | Extract the text payload from a Gemini response.
--
-- Concatenates all non-thought text parts from the first candidate.
-- Gemini 2.5 thinking models prepend internal-reasoning parts tagged with
-- @\"thought\": true@ before the actual response; those are skipped.
-- When the candidate has no @content@ (e.g. @finishReason@ is @RECITATION@
-- or @SAFETY@) the function returns 'Nothing'.
extractText :: Value -> Maybe Text
extractText = parseMaybe go
  where
    go = withObject "GeminiResponse" $ \o -> do
      candidates <- o .: "candidates"
      case candidates of
        [] -> fail "no candidates in Gemini response"
        (c : _) -> do
          content <- c .: "content"
          parts <- content .: "parts"
          let textParts = mapMaybe (parseMaybe responseTextPart) parts
          case textParts of
            [] -> fail "no text parts in Gemini candidate"
            ts -> pure (T.concat ts)
    responseTextPart = withObject "part" $ \p -> do
      isThought <- p .:? "thought" .!= False
      if isThought
        then fail "thought part"
        else p .: "text"

-- | Extract the @finishReason@ of the first candidate, if present.
extractFinishReason :: Value -> Maybe Text
extractFinishReason = parseMaybe go
  where
    go = withObject "GeminiResponse" $ \o -> do
      candidates <- o .: "candidates"
      case candidates of
        [] -> fail "no candidates"
        (c : _) -> c .: "finishReason"

-- | Extract (chunkTitle, realUri) pairs from Gemini's grounding metadata.
--
-- When the @google_search@ grounding tool is active, Gemini embeds ephemeral
-- @vertexaisearch.cloud.google.com\/grounding-api-redirect\/...@ URLs in the
-- generated text, but the real, permanent source URLs are available in
-- @candidates[0].groundingMetadata.groundingChunks[].web.uri@.  This
-- function extracts them so they can be substituted back into the parsed items.
extractGroundingChunks :: Value -> [(Text, Text)]
extractGroundingChunks = fromMaybe [] . parseMaybe go
  where
    go = withObject "GeminiResponse" $ \o -> do
      candidates <- o .: "candidates"
      case candidates of
        [] -> pure []
        (c : _) -> do
          meta <- c .: "groundingMetadata"
          chunks <- meta .: "groundingChunks"
          mapM parseChunk chunks
    parseChunk = withObject "GroundingChunk" $ \o -> do
      web <- o .: "web"
      uri <- web .: "uri"
      title <- web .:? "title" .!= uri
      pure (title, uri)

-- ---------------------------------------------------------------------------
-- URL resolution helpers
-- ---------------------------------------------------------------------------

-- | If a 'DiscoveredContent' item has an ephemeral redirect URL, attempt to
-- replace it with the matching real URL from the grounding chunk map.
-- Matching is done by case-insensitive substring containment against the
-- chunk titles.  If no match is found the original (redirect) URL is kept.
resolveRedirectUrl :: Map Text Text -> DiscoveredContent -> DiscoveredContent
resolveRedirectUrl chunkMap dc
  | not ("vertexaisearch" `T.isInfixOf` dcUrl dc) = dc
  | otherwise = dc {dcUrl = fromMaybe (dcUrl dc) (findMatch (dcTitle dc))}
  where
    findMatch modelTitle =
      listToMaybe
        [ uri
          | (chunkTitle, uri) <- Map.toList chunkMap,
            let a = T.toCaseFold chunkTitle
                b = T.toCaseFold modelTitle,
            a `T.isInfixOf` b || b `T.isInfixOf` a
        ]

-- | Pair each 'ModelItem' with the best-matching grounding chunk to obtain a
-- real, permanent source URL.  Items with no matching chunk are dropped.
--
-- Matching uses keyword overlap: at least one significant word (five or more
-- characters) from either title must appear as a substring of the other
-- (case-insensitive).
mergeWithChunks :: [(Text, Text)] -> [ModelItem] -> [DiscoveredContent]
mergeWithChunks chunks = mapMaybe matchItem
  where
    matchItem mi =
      case findChunk (miTitle mi) of
        Nothing -> Nothing
        Just (_, uri) ->
          Just $ DiscoveredContent (miTitle mi) uri (miSummary mi) (miSubjects mi)

    findChunk modelTitle =
      listToMaybe
        [ chunk
          | chunk@(chunkTitle, _) <- chunks,
            overlaps (T.toCaseFold modelTitle) (T.toCaseFold chunkTitle)
        ]

    overlaps a b =
      any (\w -> T.length w >= 5 && w `T.isInfixOf` b) (T.words a)
        || any (\w -> T.length w >= 5 && w `T.isInfixOf` a) (T.words b)

-- | Extract the value of the @q@ query-string parameter from a URL.
--
-- Google's newer redirect format embeds the real URL directly as the @q@
-- parameter (e.g. @vertexaisearch.google.com\/url?q=https:\/\/...@), so no
-- network request is needed at all.
extractQParam :: Text -> Maybe Text
extractQParam url =
  case T.breakOn "?q=" url of
    (_, rest) | not (T.null rest) -> Just $ T.takeWhile (/= '&') (T.drop 3 rest)
    _ ->
      case T.breakOn "&q=" url of
        (_, rest) | not (T.null rest) -> Just $ T.takeWhile (/= '&') (T.drop 3 rest)
        _ -> Nothing

-- | Follow a single HTTP redirect and return the resolved permanent URL.
--
-- First checks whether the real URL is embedded as a @q=@ query parameter
-- (the @vertexaisearch.google.com\/url?q=...@ format Google now uses) and
-- returns it immediately without making any network request.
--
-- Falls back to making a GET request with redirect-following disabled
-- (@redirectCount = 0@) and reading the @Location@ response header, which
-- handles the older @vertexaisearch.cloud.google.com\/grounding-api-redirect\/...@
-- format.
--
-- Non-redirect URLs (no @vertexaisearch@ substring) are returned unchanged.
-- If the server returns no @Location@ header the original URL is kept.
followRedirect :: Manager -> Text -> IO Text
followRedirect mgr url
  | not ("vertexaisearch" `T.isInfixOf` url) = pure url
  | Just real <- extractQParam url = pure real
  | otherwise = do
      req <- parseRequest (T.unpack url)
      resp <- httpLbs req {redirectCount = 0} mgr
      pure $ maybe url TE.decodeUtf8 (lookup "Location" (responseHeaders resp))

-- ---------------------------------------------------------------------------
-- Draft parsing helpers
-- ---------------------------------------------------------------------------

-- | Parse and remove a leading @TAGS: tag1, tag2, ...@ line from AI output.
-- Returns the list of trimmed tags (empty if no TAGS line) and the remaining
-- body text with leading whitespace stripped.
--
-- The Gemini draft prompt instructs the model to write exactly this line
-- before the H1 title so we can reliably extract structured tags without
-- a second API call.
parseTagsLine :: Text -> ([Text], Text)
parseTagsLine txt = case T.lines txt of
  (l : rest)
    | Just raw <- T.stripPrefix "TAGS:" l ->
        let tags = filter (not . T.null) . map T.strip . T.splitOn "," $ raw
         in (tags, T.strip (T.unlines rest))
  _ -> ([], txt)

-- | Parse the bilingual output format produced by the generate/revise prompts.
-- Strips the @TAGS:@ line, then splits the content on @---PTBR---@, yielding
-- the English body and the Brazilian Portuguese body.  The @---EN---@ header
-- line is removed from the English section.
--
-- Falls back gracefully: if the @---PTBR---@ separator is absent the entire
-- body is treated as English and the Portuguese body is empty.
parseBilingualResponse :: Text -> ([Text], Text, Text)
parseBilingualResponse txt =
  let (tags, rest) = parseTagsLine txt
      ls = T.lines (T.strip rest)
      (enLines, ptbrLines) = splitOnMarker "---PTBR---" ls
      cleanEn = dropWhile (\l -> T.strip l == "---EN---") enLines
   in (tags, T.strip (T.unlines cleanEn), T.strip (T.unlines ptbrLines))
  where
    splitOnMarker marker ls =
      let (before, after) = break (\l -> T.strip l == marker) ls
       in (before, drop 1 after)

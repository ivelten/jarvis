{-# LANGUAGE FlexibleInstances #-}

module Orchestrator.Database.Models
  ( ContentStatus (..),
    DraftStatus (..),
    CommentAuthor (..),
    TagList (..),
    InterestScore,
    mkInterestScore,
    unInterestScore,
  )
where

import Data.Text (Text, pack, stripPrefix, stripSuffix)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.Persist (PersistField (..), PersistValue (..))
import Database.Persist.Sql (PersistFieldSql (..), SqlType (..))

-- | Lifecycle status of a piece of raw content.
--
-- The typical progression is @ContentNew → ContentDrafted@; content
-- that is not worth writing about transitions to @ContentRejected@.
--
-- >>> toPersistValue ContentNew
-- PersistText "new"
data ContentStatus
  = -- | Content freshly discovered, not yet triaged.
    ContentNew
  | -- | Content discarded by AI or human reviewer.
    ContentRejected
  | -- | At least one PostDraft has been created from this content.
    ContentDrafted
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance PersistField ContentStatus where
  toPersistValue ContentNew = PersistText "new"
  toPersistValue ContentRejected = PersistText "rejected"
  toPersistValue ContentDrafted = PersistText "drafted"
  fromPersistValue pv = case pv of
    PersistText t -> parseContentStatus t
    PersistLiteral bs -> parseContentStatus (decodeUtf8 bs)
    _ -> Left $ "Expected text for ContentStatus, got: " <> pack (show pv)

-- | Parse a Text value into a ContentStatus, returning an error message on failure.
parseContentStatus :: Text -> Either Text ContentStatus
parseContentStatus "new" = Right ContentNew
parseContentStatus "rejected" = Right ContentRejected
parseContentStatus "drafted" = Right ContentDrafted
parseContentStatus t = Left $ "Unknown ContentStatus value: " <> t

instance PersistFieldSql ContentStatus where
  sqlType _ = SqlOther "content_status"

-- | Lifecycle status of a blog post draft.
--
-- The typical progression is
-- @DraftReviewing → DraftApproved → DraftPublished@.
--
-- >>> toPersistValue DraftApproved
-- PersistText "approved"
data DraftStatus
  = -- | Draft sent to Discord, awaiting human review.
    DraftReviewing
  | -- | Draft is human approved; ready to publish.
    DraftApproved
  | -- | Draft committed to Hugo repo and live on the blog.
    DraftPublished
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance PersistField DraftStatus where
  toPersistValue DraftReviewing = PersistText "reviewing"
  toPersistValue DraftApproved = PersistText "approved"
  toPersistValue DraftPublished = PersistText "published"
  fromPersistValue pv = case pv of
    PersistText t -> parseDraftStatus t
    PersistLiteral bs -> parseDraftStatus (decodeUtf8 bs)
    _ -> Left $ "Expected text for DraftStatus, got: " <> pack (show pv)

parseDraftStatus :: Text -> Either Text DraftStatus
parseDraftStatus "reviewing" = Right DraftReviewing
parseDraftStatus "approved" = Right DraftApproved
parseDraftStatus "published" = Right DraftPublished
parseDraftStatus t = Left $ "Unknown DraftStatus value: " <> t

instance PersistFieldSql DraftStatus where
  sqlType _ = SqlOther "draft_status"

-- | Identifies who authored a 'ReviewComment'.
--
-- >>> toPersistValue CommentAuthorJarvis
-- PersistText "jarvis"
data CommentAuthor
  = -- | The human blog owner
    CommentAuthorUser
  | -- | The Jarvis bot
    CommentAuthorJarvis
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance PersistField CommentAuthor where
  toPersistValue CommentAuthorUser = PersistText "user"
  toPersistValue CommentAuthorJarvis = PersistText "jarvis"
  fromPersistValue pv = case pv of
    PersistText t -> parseCommentAuthor t
    PersistLiteral bs -> parseCommentAuthor (decodeUtf8 bs)
    _ -> Left $ "Expected text for CommentAuthor, got: " <> pack (show pv)

-- | Parse a Text value into a CommentAuthor, returning an error message on failure.
parseCommentAuthor :: Text -> Either Text CommentAuthor
parseCommentAuthor "user" = Right CommentAuthorUser
parseCommentAuthor "jarvis" = Right CommentAuthorJarvis
parseCommentAuthor t = Left $ "Unknown CommentAuthor value: " <> t

instance PersistFieldSql CommentAuthor where
  sqlType _ = SqlOther "comment_author"

-- | A validated interest score in the inclusive range 1–5.
--
-- The constructor is not exported; use 'mkInterestScore' to build values.
--
-- >>> fmap unInterestScore (mkInterestScore 3)
-- Right 3
newtype InterestScore = InterestScore {unInterestScore :: Int}
  deriving (Show, Eq, Ord)

-- | Smart constructor for 'InterestScore'.
-- Returns 'Left' with an error message if the value is outside 1–5.
mkInterestScore :: Int -> Either Text InterestScore
mkInterestScore n
  | n >= 1 && n <= 5 = Right (InterestScore n)
  | otherwise = Left $ "InterestScore must be between 1 and 5, got: " <> pack (show n)

instance PersistField InterestScore where
  toPersistValue = toPersistValue . unInterestScore
  fromPersistValue pv = fromPersistValue pv >>= mkInterestScore

instance PersistFieldSql InterestScore where
  sqlType _ = SqlInt64

-- | A list of tags stored as a PostgreSQL @text[]@ array.
--
-- >>> unTagList (TagList ["haskell", "type-safety"])
-- ["haskell","type-safety"]
newtype TagList = TagList {unTagList :: [Text]}
  deriving (Show, Eq)

instance PersistField TagList where
  toPersistValue (TagList ts) =
    PersistLiteralEscaped . encodeUtf8 $
      "{" <> T.intercalate "," (map pgQuote ts) <> "}"
    where
      pgQuote t
        | T.null t || T.any (`elem` [',', '\\', '{', '}', ' ', '\n', '"']) t =
            "\"" <> T.replace "\"" "\\\"" (T.replace "\\" "\\\\" t) <> "\""
        | otherwise = t
  fromPersistValue (PersistArray pvs) = TagList <$> traverse fromPersistValue pvs
  fromPersistValue (PersistList pvs) = TagList <$> traverse fromPersistValue pvs
  fromPersistValue (PersistLiteral bs) = parsePgTextArray (decodeUtf8 bs)
  fromPersistValue (PersistLiteralEscaped bs) = parsePgTextArray (decodeUtf8 bs)
  fromPersistValue v = Left $ "Expected text[] for TagList, got: " <> pack (show v)

-- | Parse a PostgreSQL text array literal into a TagList, returning an error message on failure.
parsePgTextArray :: Text -> Either Text TagList
parsePgTextArray t =
  case stripPrefix "{" t >>= stripSuffix "}" of
    Nothing -> Left $ "Invalid PostgreSQL array format: " <> t
    Just "" -> Right (TagList [])
    Just inner -> TagList <$> go inner
  where
    go "" = Right []
    go s = do
      (el, rest) <- parseElem s
      more <- case T.uncons rest of
        Just (',', more) -> go more
        Just _ -> Left $ "Unexpected character in array: " <> rest
        Nothing -> Right []
      Right (el : more)
    parseElem s = case T.uncons s of
      Just ('"', s') -> parseQuoted s' ""
      _ -> Right (T.break (== ',') s)
    parseQuoted s acc = case T.uncons s of
      Nothing -> Right (acc, "")
      Just ('"', r) -> case T.uncons r of
        Just ('"', more) -> parseQuoted more (acc <> T.singleton '"')
        _ -> Right (acc, r)
      Just ('\\', r) -> case T.uncons r of
        Just (c, more) -> parseQuoted more (acc <> T.singleton c)
        Nothing -> Right (acc, "")
      Just (c, more) -> parseQuoted more (acc <> T.singleton c)

instance PersistFieldSql TagList where
  sqlType _ = SqlOther "text[]"

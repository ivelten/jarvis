-- | Integration tests for database migration.
--
-- These tests connect to the 'jarvis_test' database and verify that the
-- full migration process works end-to-end: enum types are created, all
-- tables are present, and Haskell values for every custom type round-trip
-- correctly via actual INSERT / SELECT operations.
module Orchestrator.Database.MigrationSpec (spec) where

import Data.Text (Text, pack)
import Data.Time (getCurrentTime)
import Database.Persist (get, insert)
import Database.Persist.Sql (Single (..), rawSql)
import Orchestrator.Database.Connection (DbPool, migrateDatabase, runDb)
import Orchestrator.Database.Entities
import Orchestrator.Database.Models
import Test.Hspec
import TestHelpers

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  pool <- runIO setupTestPool

  -- -----------------------------------------------------------------------
  -- Migration lifecycle
  -- -----------------------------------------------------------------------
  describe "migrateDatabase" $ do
    it "succeeds on a freshly migrated database" $
      migrateDatabase pool `shouldReturn` ()

    it "is idempotent (safe to run twice in succession)" $ do
      migrateDatabase pool
      migrateDatabase pool `shouldReturn` ()

  -- -----------------------------------------------------------------------
  -- Schema shape
  -- -----------------------------------------------------------------------
  describe "database schema" $ do
    it "creates all expected tables" $ do
      tables <- getPublicTables pool
      tables
        `shouldContain` ["ai_analysis", "post_draft", "post_draft_source", "raw_content", "review_comment", "subject"]

    it "creates the content_status enum type" $ do
      types <- getPgEnumTypes pool
      types `shouldContain` ["content_status"]

    it "creates the draft_status enum type" $ do
      types <- getPgEnumTypes pool
      types `shouldContain` ["draft_status"]

    it "creates the comment_author enum type" $ do
      types <- getPgEnumTypes pool
      types `shouldContain` ["comment_author"]

  -- -----------------------------------------------------------------------
  -- ContentStatus persistence
  -- -----------------------------------------------------------------------
  describe "ContentStatus persistence" $
    before_ (truncateTestTables pool) $ do
      it "persists and reads back ContentNew" $
        roundTripStatus pool ContentNew

      it "persists and reads back ContentRejected" $
        roundTripStatus pool ContentRejected

      it "persists and reads back ContentDrafted" $
        roundTripStatus pool ContentDrafted

  -- -----------------------------------------------------------------------
  -- DraftStatus persistence
  -- -----------------------------------------------------------------------
  describe "DraftStatus persistence" $
    before_ (truncateTestTables pool) $ do
      it "persists and reads back DraftReviewing" $
        roundTripDraftStatus pool DraftReviewing

      it "persists and reads back DraftApproved" $
        roundTripDraftStatus pool DraftApproved

      it "persists and reads back DraftPublished" $
        roundTripDraftStatus pool DraftPublished

  -- -----------------------------------------------------------------------
  -- CommentAuthor persistence
  -- -----------------------------------------------------------------------
  describe "CommentAuthor persistence" $
    before_ (truncateTestTables pool) $ do
      it "persists and reads back CommentAuthorUser" $
        roundTripCommentAuthor pool CommentAuthorUser

      it "persists and reads back CommentAuthorJarvis" $
        roundTripCommentAuthor pool CommentAuthorJarvis

  -- -----------------------------------------------------------------------
  -- TagList persistence
  -- -----------------------------------------------------------------------
  describe "TagList persistence" $
    before_ (truncateTestTables pool) $ do
      it "persists and reads back an empty tag list" $
        roundTripTagList pool "branch/empty" (TagList [])

      it "persists and reads back simple tags" $
        roundTripTagList pool "branch/simple" (TagList ["haskell", "fp", "types"])

      it "persists and reads back tags containing spaces" $
        roundTripTagList pool "branch/spaces" (TagList ["lazy evaluation", "type classes"])

      it "persists and reads back tags with commas and quotes" $
        roundTripTagList pool "branch/special" (TagList ["a,b", "say \"hi\""])

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | All table names in the public schema, sorted.
getPublicTables :: DbPool -> IO [Text]
getPublicTables pool = do
  rows <-
    runDb pool $
      rawSql
        "SELECT tablename FROM pg_tables \
        \WHERE schemaname = 'public' ORDER BY tablename"
        []
  return $ map (\(Single t) -> t) rows

-- | All user-defined enum type names, sorted.
getPgEnumTypes :: DbPool -> IO [Text]
getPgEnumTypes pool = do
  rows <-
    runDb pool $
      rawSql
        "SELECT typname FROM pg_type WHERE typtype = 'e' ORDER BY typname"
        []
  return $ map (\(Single t) -> t) rows

-- | Insert a RawContent row with the given status and verify the row reads
-- back with the same status.
roundTripStatus :: DbPool -> ContentStatus -> IO ()
roundTripStatus pool status = do
  now <- getCurrentTime
  let url = "https://example.com/" <> pack (show status)
  rcId <-
    runDb pool $
      insert
        RawContent
          { rawContentTitle = "Test article",
            rawContentUrl = url,
            rawContentSummary = "A test summary.",
            rawContentRawHtml = Nothing,
            rawContentSubjectId = Nothing,
            rawContentStatus = status,
            rawContentRejectionReason = Nothing,
            rawContentCreatedAt = now,
            rawContentUpdatedAt = now
          }
  mrc <- runDb pool $ get rcId
  fmap rawContentStatus mrc `shouldBe` Just status

-- | Insert a PostDraft row with the given draft status and verify it reads
-- back correctly.
roundTripDraftStatus :: DbPool -> DraftStatus -> IO ()
roundTripDraftStatus pool status = do
  now <- getCurrentTime
  let branch = "draft/test-" <> pack (show status)
  pdId <-
    runDb pool $
      insert
        PostDraft
          { postDraftTitle = "Test draft",
            postDraftGitBranch = branch,
            postDraftSubjectId = Nothing,
            postDraftSuggestedTags = TagList [],
            postDraftStatus = status,
            postDraftDiscordThreadId = Nothing,
            postDraftPublishedAt = Nothing,
            postDraftPublishedUrl = Nothing,
            postDraftCreatedAt = now,
            postDraftUpdatedAt = now
          }
  mpd <- runDb pool $ get pdId
  fmap postDraftStatus mpd `shouldBe` Just status

-- | Insert a ReviewComment with the given author and verify it reads back.
-- Requires a PostDraft to satisfy the foreign key constraint; one is
-- inserted and used as the parent.
roundTripCommentAuthor :: DbPool -> CommentAuthor -> IO ()
roundTripCommentAuthor pool author = do
  now <- getCurrentTime
  pdId <-
    runDb pool $
      insert
        PostDraft
          { postDraftTitle = "Parent draft",
            postDraftGitBranch = "draft/comment-author-" <> pack (show author),
            postDraftSubjectId = Nothing,
            postDraftSuggestedTags = TagList [],
            postDraftStatus = DraftReviewing,
            postDraftDiscordThreadId = Nothing,
            postDraftPublishedAt = Nothing,
            postDraftPublishedUrl = Nothing,
            postDraftCreatedAt = now,
            postDraftUpdatedAt = now
          }
  rcId <-
    runDb pool $
      insert
        ReviewComment
          { reviewCommentPostDraftId = pdId,
            reviewCommentAuthor = author,
            reviewCommentMessage = "Test message",
            reviewCommentCreatedAt = now
          }
  mrc <- runDb pool $ get rcId
  fmap reviewCommentAuthor mrc `shouldBe` Just author

-- | Insert a PostDraft with the given TagList and verify it reads back.
roundTripTagList :: DbPool -> Text -> TagList -> IO ()
roundTripTagList pool branch tags = do
  now <- getCurrentTime
  pdId <-
    runDb pool $
      insert
        PostDraft
          { postDraftTitle = "Tag test draft",
            postDraftGitBranch = branch,
            postDraftSubjectId = Nothing,
            postDraftSuggestedTags = tags,
            postDraftStatus = DraftReviewing,
            postDraftDiscordThreadId = Nothing,
            postDraftPublishedAt = Nothing,
            postDraftPublishedUrl = Nothing,
            postDraftCreatedAt = now,
            postDraftUpdatedAt = now
          }
  mpd <- runDb pool $ get pdId
  fmap postDraftSuggestedTags mpd `shouldBe` Just tags

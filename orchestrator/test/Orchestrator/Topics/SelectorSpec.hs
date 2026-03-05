-- | Integration tests for 'Orchestrator.Topics.Selector'.
--
-- Requires the 'jarvis_test' database to exist and be migrated; this is
-- handled automatically by the top-level test 'Main'.
module Orchestrator.Topics.SelectorSpec (spec) where

import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Database.Persist.Sql (Entity (..), entityVal, insert_, update, (=.))
import Orchestrator.AI.Client (DiscoveredContent (..))
import Orchestrator.Database.Connection (runDb)
import Orchestrator.Database.Entities
import Orchestrator.Database.Models
import Orchestrator.Topics.Selector
import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  pool <- runIO setupTestPool

  -- Wipe all rows before every test so tests are fully independent.
  before_ (truncateTestTables pool) $ do
    describe "pendingContent" $ do
      it "returns [] when no content has been ingested" $ do
        result <- runDb pool pendingContent
        result `shouldBe` []

      it "returns ContentNew items" $ do
        runDb pool $ insert_ (rawContent "https://a.com" ContentNew)
        result <- runDb pool pendingContent
        length result `shouldBe` 1

      it "excludes ContentDrafted items" $ do
        runDb pool $ insert_ (rawContent "https://b.com" ContentDrafted)
        result <- runDb pool pendingContent
        result `shouldBe` []

      it "excludes ContentRejected items" $ do
        runDb pool $ insert_ (rawContent "https://c.com" ContentRejected)
        result <- runDb pool pendingContent
        result `shouldBe` []

      it "returns only ContentNew when a mix of statuses exist" $ do
        runDb pool $ do
          insert_ (rawContent "https://new.com" ContentNew)
          insert_ (rawContent "https://drafted.com" ContentDrafted)
          insert_ (rawContent "https://rejected.com" ContentRejected)
        result <- runDb pool pendingContent
        length result `shouldBe` 1
        rawContentStatus (entityVal (head result)) `shouldBe` ContentNew

    describe "ingestContent" $ do
      it "inserts new items as ContentNew" $ do
        runDb pool $ ingestContent [discovered "https://new.com" "Original title"]
        result <- runDb pool pendingContent
        length result `shouldBe` 1
        rawContentTitle (entityVal (head result)) `shouldBe` "Original title"

      it "upserts: updates title on a duplicate URL" $ do
        runDb pool $ ingestContent [discovered "https://dup.com" "Old title"]
        runDb pool $ ingestContent [discovered "https://dup.com" "New title"]
        result <- runDb pool pendingContent
        length result `shouldBe` 1
        rawContentTitle (entityVal (head result)) `shouldBe` "New title"

      it "upserts: does not reset a non-New status back to New" $ do
        runDb pool $ ingestContent [discovered "https://triaged.com" "Title"]
        -- Simulate a human changing the status to Drafted.
        rows <- runDb pool pendingContent
        let key = case rows of
              (e : _) -> entityKey e
              [] -> error "expected a row"
        runDb pool $ update key [RawContentStatus =. ContentDrafted]
        -- Re-ingest with the same URL — status must stay Drafted.
        runDb pool $ ingestContent [discovered "https://triaged.com" "Updated title"]
        pending' <- runDb pool pendingContent
        pending' `shouldBe` []

      it "inserts with subjectId = Nothing when subject name is unknown" $ do
        runDb pool $
          ingestContent
            [ DiscoveredContent
                { dcTitle = "No subject",
                  dcUrl = "https://nosubject.com",
                  dcSummary = "Some summary",
                  dcSubject = Just "NonExistentSubject"
                }
            ]
        result <- runDb pool pendingContent
        length result `shouldBe` 1
        rawContentSubjectId (entityVal (head result)) `shouldBe` Nothing

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

epoch :: UTCTime
epoch = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)

rawContent :: Text -> ContentStatus -> RawContent
rawContent url status =
  RawContent
    { rawContentTitle = "Test title",
      rawContentUrl = url,
      rawContentSummary = "Test summary",
      rawContentRawHtml = Nothing,
      rawContentSubjectId = Nothing,
      rawContentStatus = status,
      rawContentRejectionReason = Nothing,
      rawContentCreatedAt = epoch,
      rawContentUpdatedAt = epoch
    }

discovered :: Text -> Text -> DiscoveredContent
discovered url title =
  DiscoveredContent
    { dcTitle = title,
      dcUrl = url,
      dcSummary = "Some summary",
      dcSubject = Nothing
    }

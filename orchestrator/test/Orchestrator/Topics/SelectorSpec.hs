-- | Integration tests for 'Orchestrator.Topics.Selector'.
--
-- Requires the 'jarvis_test' database to exist and be migrated; this is
-- handled automatically by the top-level test 'Main'.
module Orchestrator.Topics.SelectorSpec (spec) where

import Data.Text (Text, pack, unpack)
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Database.Persist.Sql (Entity (..), Filter, entityKey, entityVal, insert_, selectList, update, (=.), (==.))
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

      it "does not create a subject link when subject name is unknown" $ do
        runDb pool $
          ingestContent
            [ DiscoveredContent
                { dcTitle = "No subject",
                  dcUrl = "https://nosubject.com",
                  dcSummary = "Some summary",
                  dcSubjects = ["NonExistentSubject"]
                }
            ]
        result <- runDb pool pendingContent
        length result `shouldBe` 1
        links <- runDb pool $ selectList [RawContentSubjectRawContentId ==. entityKey (head result)] []
        links `shouldBe` ([] :: [Entity RawContentSubject])

      it "links content to an existing subject by name" $ do
        let score = either (error . unpack) id (mkInterestScore 3)
        runDb pool $
          insert_
            Subject
              { subjectName = "Haskell Basics",
                subjectInterestScore = score,
                subjectCreatedAt = epoch,
                subjectUpdatedAt = epoch
              }
        runDb pool $
          ingestContent
            [ DiscoveredContent
                { dcTitle = "A Haskell Basics article",
                  dcUrl = "https://linked-subject.com",
                  dcSummary = "About basics",
                  dcSubjects = ["Haskell Basics"]
                }
            ]
        result <- runDb pool pendingContent
        length result `shouldBe` 1
        links <- runDb pool $ selectList [RawContentSubjectRawContentId ==. entityKey (head result)] []
        length links `shouldBe` 1

      it "links content to multiple subjects when several names match" $ do
        let score = either (error . unpack) id (mkInterestScore 3)
        runDb pool $ do
          insert_
            Subject
              { subjectName = "Monads",
                subjectInterestScore = score,
                subjectCreatedAt = epoch,
                subjectUpdatedAt = epoch
              }
          insert_
            Subject
              { subjectName = "Error Handling",
                subjectInterestScore = score,
                subjectCreatedAt = epoch,
                subjectUpdatedAt = epoch
              }
        runDb pool $
          ingestContent
            [ DiscoveredContent
                { dcTitle = "Monadic error handling",
                  dcUrl = "https://multi-subject.com",
                  dcSummary = "About both",
                  dcSubjects = ["Monads", "Error Handling"]
                }
            ]
        result <- runDb pool pendingContent
        length result `shouldBe` 1
        links <- runDb pool $ selectList [RawContentSubjectRawContentId ==. entityKey (head result)] []
        length links `shouldBe` 2

    describe "topSubjects" $ do
      it "returns subjects ordered by interest score descending" $ do
        let mkScore n = either (error . unpack) id (mkInterestScore n)
        runDb pool $ do
          insert_ Subject {subjectName = "Low", subjectInterestScore = mkScore 1, subjectCreatedAt = epoch, subjectUpdatedAt = epoch}
          insert_ Subject {subjectName = "High", subjectInterestScore = mkScore 5, subjectCreatedAt = epoch, subjectUpdatedAt = epoch}
          insert_ Subject {subjectName = "Mid", subjectInterestScore = mkScore 3, subjectCreatedAt = epoch, subjectUpdatedAt = epoch}
        results <- runDb pool topSubjects
        map (subjectName . entityVal) results `shouldBe` ["High", "Mid", "Low"]

      it "returns at most 10 subjects even when more exist" $ do
        let mkScore n = either (error . unpack) id (mkInterestScore n)
        runDb pool $
          mapM_
            ( \i ->
                insert_
                  Subject
                    { subjectName = "Subject " <> pack (show (i :: Int)),
                      subjectInterestScore = mkScore (((i - 1) `mod` 5) + 1),
                      subjectCreatedAt = epoch,
                      subjectUpdatedAt = epoch
                    }
            )
            [1 .. 12]
        results <- runDb pool topSubjects
        length results `shouldBe` 10

    describe "recordDiscovery" $ do
      it "inserts a ContentSearchAiAnalysis row recording the item count" $ do
        let items = [discovered "https://telemetry-a.com" "Title A", discovered "https://telemetry-b.com" "Title B"]
        runDb pool $ recordDiscovery 42 items
        rows <- runDb pool $ selectList ([] :: [Filter ContentSearchAiAnalysis]) []
        length rows `shouldBe` 1
        let row = entityVal (head rows)
        contentSearchAiAnalysisTotalItemsFound row `shouldBe` 2
        contentSearchAiAnalysisItemsIngested row `shouldBe` 2
        contentSearchAiAnalysisTokensUsed row `shouldBe` 42

      it "inserts one telemetry row per recordDiscovery call" $ do
        runDb pool $ recordDiscovery 10 [discovered "https://t1.com" "T1"]
        runDb pool $ recordDiscovery 20 [discovered "https://t2.com" "T2"]
        rows <- runDb pool $ selectList ([] :: [Filter ContentSearchAiAnalysis]) []
        length rows `shouldBe` 2

      it "records zero items found when the discovered list is empty" $ do
        runDb pool $ recordDiscovery 5 []
        rows <- runDb pool $ selectList ([] :: [Filter ContentSearchAiAnalysis]) []
        length rows `shouldBe` 1
        contentSearchAiAnalysisTotalItemsFound (entityVal (head rows)) `shouldBe` 0

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
      rawContentStatus = status,
      rawContentCreatedAt = epoch,
      rawContentUpdatedAt = epoch
    }

discovered :: Text -> Text -> DiscoveredContent
discovered url title =
  DiscoveredContent
    { dcTitle = title,
      dcUrl = url,
      dcSummary = "Some summary",
      dcSubjects = []
    }

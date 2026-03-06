-- | Integration tests for pipeline-level database helpers.
--
-- Tests focus on the 'DraftAiAnalysis' telemetry inserted by
-- 'persistDraftAnalysis', exercising the same path used by both
-- 'persistInitialDraft' (initial generation) and 'recordRevision' (each
-- revision step).
module Orchestrator.PipelineSpec (spec) where

import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Database.Persist.Sql (Entity (..), Filter, entityVal, insert, selectList)
import Orchestrator.AI.Client (GeneratedDraft (..))
import Orchestrator.Database.Connection (runDb)
import Orchestrator.Database.Entities
import Orchestrator.Database.Models
import Orchestrator.Pipeline (persistDraftAnalysis)
import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  pool <- runIO setupTestPool

  before_ (truncateTestTables pool) $ do
    describe "persistDraftAnalysis" $ do
      it "inserts a DraftAiAnalysis row linked to the post draft" $ do
        pdKey <- runDb pool $ insert (sampleDraft "draft/test-post")
        runDb pool $ persistDraftAnalysis pdKey sampleGenerated epoch
        rows <- runDb pool $ selectList ([] :: [Filter DraftAiAnalysis]) []
        length rows `shouldBe` 1
        let row = entityVal (head rows)
        draftAiAnalysisPostDraftId row `shouldBe` pdKey

      it "records the token count from the generated draft" $ do
        pdKey <- runDb pool $ insert (sampleDraft "draft/tokens-post")
        runDb pool $ persistDraftAnalysis pdKey sampleGenerated {gdTokensUsed = 42} epoch
        rows <- runDb pool $ selectList ([] :: [Filter DraftAiAnalysis]) []
        draftAiAnalysisTokensUsed (entityVal (head rows)) `shouldBe` 42

      it "inserts one row per call (initial + revision each produce a row)" $ do
        pdKey <- runDb pool $ insert (sampleDraft "draft/multi-analysis")
        runDb pool $ persistDraftAnalysis pdKey sampleGenerated epoch
        runDb pool $ persistDraftAnalysis pdKey sampleGenerated {gdBodyEn = "Revised body"} epoch
        rows <- runDb pool $ selectList ([] :: [Filter DraftAiAnalysis]) []
        length rows `shouldBe` 2

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

epoch :: UTCTime
epoch = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)

sampleDraft :: Text -> PostDraft
sampleDraft branch =
  PostDraft
    { postDraftTitle = "Test Post",
      postDraftGitBranch = branch,
      postDraftSuggestedTags = TagList [],
      postDraftStatus = DraftReviewing,
      postDraftDiscordThreadId = Nothing,
      postDraftContentMarkdownEn = Nothing,
      postDraftContentMarkdownPtBr = Nothing,
      postDraftPublishedAt = Nothing,
      postDraftPublishedUrl = Nothing,
      postDraftCreatedAt = epoch,
      postDraftUpdatedAt = epoch
    }

sampleGenerated :: GeneratedDraft
sampleGenerated =
  GeneratedDraft
    { gdTitle = "Test Post",
      gdBranch = "draft/test-post",
      gdBodyEn = "Some English body content.",
      gdBodyPtBr = "Algum conteúdo em português.",
      gdTags = ["haskell", "fp"],
      gdTokensUsed = 100
    }

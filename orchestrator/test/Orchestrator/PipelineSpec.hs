-- | Integration tests for pipeline-level database helpers.
--
-- Tests focus on the 'DraftAiAnalysis' telemetry inserted by
-- 'persistDraftAnalysis', exercising the same path used by both
-- 'persistInitialDraft' (initial generation) and 'recordRevision' (each
-- revision step).
module Orchestrator.PipelineSpec (spec) where

import Control.Concurrent.MVar
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Database.Persist.Sql (Entity (..), Filter, entityVal, fromSqlKey, get, insert, selectList, toSqlKey)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Orchestrator.AI.Client (AiConfig (..), GeneratedDraft (..))
import Orchestrator.Database.Connection (DbPool, runDb)
import Orchestrator.Database.Entities
import Orchestrator.Database.Models
import Orchestrator.Discord.Bot (DisableSubjectCommandEvent (..), DiscordBotSettings (..), DiscordConfig (..), RevisionResult (..), SubjectCommandEvent (..), mkDiscordConfig)
import Orchestrator.GitHub.Client (GitHubConfig (..))
import Orchestrator.Pipeline (PipelineEnv (..), PublishDraftRequest (..), RenderedDraft (..), createSubject, disableSubject, persistDraftAnalysis, renderDraftFiles)
import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  pool <- runIO setupTestPool

  describe "renderDraftFiles" $ do
    let req =
          PublishDraftRequest
            { pubRcKey = toSqlKey 1,
              pubDraftKey = toSqlKey 1,
              pubCreatedAt = epoch,
              pubBodyEn = "# My Post Title\n\nEnglish body.",
              pubBodyPtBr = "# Meu Título\n\nCorpo em português.",
              pubTags = ["haskell", "fp"],
              pubThreadId = Nothing
            }
        rendered = renderDraftFiles req

    it "extracts the title from the EN H1" $
      rdTitle rendered `shouldBe` "My Post Title"

    it "derives the slug from the title" $
      rdSlug rendered `shouldBe` "my-post-title"

    it "builds the EN filename from the slug" $
      rdFilenameEn rendered `shouldBe` "my-post-title.en.md"

    it "builds the PT-BR filename from the slug" $
      rdFilenamePtBr rendered `shouldBe` "my-post-title.pt-br.md"

    it "includes the EN body in the EN content" $
      rdContentEn rendered `shouldSatisfy` T.isInfixOf "English body."

    it "includes the PT-BR body in the PT-BR content" $
      rdContentPtBr rendered `shouldSatisfy` T.isInfixOf "Corpo em português."

    it "EN and PT-BR files share the same slug" $
      T.takeWhile (/= '.') (rdFilenameEn rendered)
        `shouldBe` T.takeWhile (/= '.') (rdFilenamePtBr rendered)

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

    describe "createSubject" $ do
      it "inserts a Subject row with interest score 3" $ do
        env <- testPipelineEnv pool
        createSubject env (SubjectCommandEvent "Haskell Concurrency")
        rows <- runDb pool $ selectList ([] :: [Filter Subject]) []
        length rows `shouldBe` 1
        let row = entityVal (head rows)
        subjectName row `shouldBe` "Haskell Concurrency"
        unInterestScore (subjectInterestScore row) `shouldBe` 3

      it "does not insert a duplicate when the name already exists" $ do
        env <- testPipelineEnv pool
        createSubject env (SubjectCommandEvent "Haskell")
        createSubject env (SubjectCommandEvent "Haskell")
        rows <- runDb pool $ selectList ([] :: [Filter Subject]) []
        length rows `shouldBe` 1

      it "inserts multiple subjects with distinct names" $ do
        env <- testPipelineEnv pool
        createSubject env (SubjectCommandEvent "Haskell")
        createSubject env (SubjectCommandEvent "Nix")
        rows <- runDb pool $ selectList ([] :: [Filter Subject]) []
        length rows `shouldBe` 2

    describe "disableSubject" $ do
      it "sets enabled=False on an existing subject" $ do
        env <- testPipelineEnv pool
        key <- runDb pool $ insert Subject {subjectName = "Rust", subjectInterestScore = either (error . T.unpack) id (mkInterestScore 3), subjectEnabled = True, subjectCreatedAt = epoch, subjectUpdatedAt = epoch}
        disableSubject env (DisableSubjectCommandEvent (fromSqlKey key))
        mRow <- runDb pool $ get key
        fmap subjectEnabled mRow `shouldBe` Just False

      it "does not crash when subject ID does not exist" $ do
        env <- testPipelineEnv pool
        disableSubject env (DisableSubjectCommandEvent 99999) `shouldReturn` ()

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

epoch :: UTCTime
epoch = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)

-- | Build a minimal 'PipelineEnv' backed by the test pool.
-- The 'DiscordConfig' uses no-op callbacks and a dummy token; Discord API
-- calls are never made from integration tests.
testPipelineEnv :: DbPool -> IO PipelineEnv
testPipelineEnv pool = do
  mgr <- newManager defaultManagerSettings
  dcCfg <- mkDiscordConfig dummyBotSettings
  -- Pre-fill the handle MVar with an undefined value so that
  -- sendInteractionMessage does not block waiting for a bot connection.
  -- Any REST call will throw immediately, which tryLog catches and discards.
  putMVar (dcHandle dcCfg) (error "testPipelineEnv: no live Discord connection")
  pure
    PipelineEnv
      { pipeAiCfg = AiConfig {aiApiKey = "", aiModels = [], aiManager = mgr},
        pipeGhCfg =
          GitHubConfig
            { ghToken = "",
              ghRepoOwner = "",
              ghRepoName = "",
              ghBranch = "",
              ghPostsPath = "",
              ghWorkflowId = "",
              ghManager = mgr,
              ghApiBase = ""
            },
        pipeDcCfg = dcCfg,
        pipeDbPool = pool
      }
  where
    dummyBotSettings =
      DiscordBotSettings
        { dbsBotToken = "test-token",
          dbsGuildId = 0,
          dbsChannelId = 0,
          dbsInteractionChannelId = 0,
          dbsOnDiscoverCommand = pure (),
          dbsOnDraftCommand = pure (),
          dbsOnSubjectCommand = \_ -> pure (),
          dbsOnDisableSubjectCommand = \_ -> pure (),
          dbsOnListSubjectsCommand = pure (),
          dbsOnApproveReview = \_ -> pure (),
          dbsOnRejectReview = \_ -> pure (),
          dbsOnReviseRequest = \_ -> pure ReviewNotActive
        }

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

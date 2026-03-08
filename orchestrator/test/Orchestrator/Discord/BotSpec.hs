-- | Unit tests for 'Orchestrator.Discord.Bot'.
--
-- Tests cover the send-queue mechanics, 'isApprovalMessage', callback wiring
-- through 'DiscordConfig', and the 'rrOnThreadCreated' callback.
-- Business logic (revise/approve/reject) now lives in 'Pipeline' where the
-- DB is accessible, so those behaviours are tested in 'PipelineSpec'.
module Orchestrator.Discord.BotSpec (spec) where

import Control.Concurrent.MVar
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Orchestrator.Discord.Bot
import Orchestrator.TextUtils (emojiApprove, emojiReject)
import Test.Hspec

-- ---------------------------------------------------------------------------
-- Test helpers
-- ---------------------------------------------------------------------------

-- | Default 'DiscordBotSettings' with no-op handlers, suitable for most tests.
testSettings :: DiscordBotSettings
testSettings =
  DiscordBotSettings
    { dbsBotToken = "token",
      dbsGuildId = 1,
      dbsChannelId = 2,
      dbsInteractionChannelId = 3,
      dbsOnDiscoverCommand = pure (),
      dbsOnDraftCommand = pure (),
      dbsOnSubjectCommand = \_ -> pure (),
      dbsOnApproveReview = \_ -> pure (),
      dbsOnRejectReview = \_ -> pure (),
      dbsOnReviseRequest = \_ -> pure ReviewNotActive
    }

-- | Minimal 'ReviewRequest' with a no-op 'rrOnThreadCreated'.
minimalReq :: Text -> Text -> ReviewRequest
minimalReq title body =
  ReviewRequest
    { rrTitle = title,
      rrBodyEn = body,
      rrBodyPtBr = "",
      rrTags = [],
      rrSourceTitle = "",
      rrSourceSummary = "",
      rrSourceUrl = "",
      rrSubjects = [],
      rrOnThreadCreated = \_ -> pure ()
    }

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "mkDiscordConfig" $ do
    it "creates a config with an empty send queue" $ do
      cfg <- mkDiscordConfig testSettings
      isEmpty <- isEmptyMVar (dcSendQueue cfg)
      isEmpty `shouldBe` True

    it "stores the approve handler from settings" $ do
      calledWith <- newIORef ("" :: Text)
      let settings = testSettings {dbsOnApproveReview = \ev -> writeIORef calledWith (aprThreadId ev)}
      cfg <- mkDiscordConfig settings
      dcOnApproveReview cfg ApproveReviewEvent {aprThreadId = "thread-42", aprTriggerText = emojiApprove}
      readIORef calledWith `shouldReturn` "thread-42"

    it "stores the reject handler from settings" $ do
      calledWith <- newIORef ("" :: Text)
      let settings = testSettings {dbsOnRejectReview = \ev -> writeIORef calledWith (rejThreadId ev)}
      cfg <- mkDiscordConfig settings
      dcOnRejectReview cfg RejectReviewEvent {rejThreadId = "thread-99", rejReason = emojiReject}
      readIORef calledWith `shouldReturn` "thread-99"

    it "stores the revise handler from settings" $ do
      calledWith <- newIORef ("" :: Text)
      let settings =
            testSettings
              { dbsOnReviseRequest = \ev -> writeIORef calledWith (rvsThreadId ev) >> pure ReviewNotActive
              }
      cfg <- mkDiscordConfig settings
      _ <- dcOnReviseRequest cfg ReviseReviewEvent {rvsThreadId = "thread-77", rvsFeedback = "feedback"}
      readIORef calledWith `shouldReturn` "thread-77"

  describe "isApprovalMessage" $ do
    it "recognises 'publish'" $
      isApprovalMessage "publish this" `shouldBe` True

    it "recognises 'lgtm'" $
      isApprovalMessage "LGTM!" `shouldBe` True

    it "recognises 'looks good'" $
      isApprovalMessage "looks good to me" `shouldBe` True

    it "does not match arbitrary feedback" $
      isApprovalMessage "can you expand the introduction?" `shouldBe` False

    it "is case-insensitive" $
      isApprovalMessage "APPROVE" `shouldBe` True

  describe "registerForReview" $ do
    it "queues the request with the correct title and body" $ do
      cfg <- mkDiscordConfig testSettings
      registerForReview cfg (minimalReq "My Title" "My Body")
      req <- takeMVar (dcSendQueue cfg)
      rrTitle req `shouldBe` "My Title"
      rrBodyEn req `shouldBe` "My Body"

    it "rrOnThreadCreated is called with the supplied key" $ do
      cfg <- mkDiscordConfig testSettings
      calledWith <- newIORef ("" :: Text)
      let req = (minimalReq "T" "B") {rrOnThreadCreated = writeIORef calledWith}
      registerForReview cfg req
      taken <- takeMVar (dcSendQueue cfg)
      rrOnThreadCreated taken "key-tc"
      readIORef calledWith `shouldReturn` "key-tc"

  describe "RevisionResult" $ do
    it "reviewer handler returning ReviewNotActive is distinguishable" $ do
      let result = ReviewNotActive
      result `shouldBe` ReviewNotActive

    it "handler can return RevisionOk bodies" $ do
      let result = RevisionOk "body-en" "body-ptbr"
      result `shouldBe` RevisionOk "body-en" "body-ptbr"

    it "handler can return RevisionError with message" $ do
      let result = RevisionError "something broke"
      result `shouldBe` RevisionError "something broke"

  describe "buildContextMessage" $ do
    let req =
          (minimalReq "My Title" "My Body")
            { rrSourceTitle = "Source Article",
              rrSourceSummary = "A brief summary of the source.",
              rrSourceUrl = "https://example.com/article",
              rrTags = ["haskell", "fp"],
              rrSubjects = ["Programming", "Open Source"]
            }

    it "includes the source title" $
      buildContextMessage req `shouldSatisfy` T.isInfixOf "Source Article"

    it "includes the source summary" $
      buildContextMessage req `shouldSatisfy` T.isInfixOf "A brief summary of the source."

    it "includes the source URL" $
      buildContextMessage req `shouldSatisfy` T.isInfixOf "https://example.com/article"

    it "includes each tag in backticks" $ do
      let msg = buildContextMessage req
      msg `shouldSatisfy` T.isInfixOf "`haskell`"
      msg `shouldSatisfy` T.isInfixOf "`fp`"

    it "includes each subject" $ do
      let msg = buildContextMessage req
      msg `shouldSatisfy` T.isInfixOf "Programming"
      msg `shouldSatisfy` T.isInfixOf "Open Source"

    it "omits the Tags line when rrTags is empty" $ do
      let noTags = req {rrTags = []}
      buildContextMessage noTags `shouldSatisfy` (not . T.isInfixOf "**Tags:**")

    it "omits the Subjects line when rrSubjects is empty" $ do
      let noSubjects = req {rrSubjects = []}
      buildContextMessage noSubjects `shouldSatisfy` (not . T.isInfixOf "**Subjects:**")

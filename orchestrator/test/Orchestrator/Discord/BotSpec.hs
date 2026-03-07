-- | Unit tests for 'Orchestrator.Discord.Bot'.
--
-- These tests exercise the callback-based review contract between
-- 'registerForReview', the bot's internal send queue, review maps, and the
-- event-handler logic — all without making a real Discord connection.
module Orchestrator.Discord.BotSpec (spec) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Discord.Types (ChannelId, DiscordId (..), Snowflake (..))
import Orchestrator.Discord.Bot
import Orchestrator.TextUtils (emojiReject)
import Test.Hspec

-- ---------------------------------------------------------------------------
-- Test helpers
-- ---------------------------------------------------------------------------

-- | Synthetic 'ChannelId' used in place of a real Discord thread ID.
testThreadId :: ChannelId
testThreadId = DiscordId (Snowflake 999)

-- | Simulate what 'drainQueue' does after creating a forum thread: take the
-- 'ReviewRequest' from the queue, build a 'PendingReview', and register it in
-- 'dcReviewMap' under the supplied synthetic key.  In a real forum channel the
-- key is the thread/starter-message snowflake; here we use a caller-supplied
-- string.
simulateDrain :: DiscordConfig -> Text -> IO PendingReview
simulateDrain cfg key = do
  req <- takeMVar (dcSendQueue cfg)
  bodyEnVar <- newTVarIO (rrBodyEn req)
  bodyPtBrVar <- newTVarIO (rrBodyPtBr req)
  tagsVar <- newTVarIO (rrTags req)
  let pr =
        PendingReview
          { prTitle = rrTitle req,
            prCurrentBodyEn = bodyEnVar,
            prCurrentBodyPtBr = bodyPtBrVar,
            prCurrentTags = tagsVar,
            prKey = key,
            prThreadId = testThreadId,
            prRevise = rrRevise req,
            prApprove = rrApprove req,
            prReject = rrReject req,
            prOnUserMessage = rrOnUserMessage req,
            prOnBotMessage = rrOnBotMessage req
          }
  atomically $ modifyTVar' (dcReviewMap cfg) (Map.insert key pr)
  rrOnThreadCreated req key
  pure pr

-- | Simulate what 'eventHandler' does for a 'MessageReactionAdd': atomically
-- remove from 'dcReviewMap' and return the 'PendingReview' so the caller can
-- fire callbacks.
simulateReaction :: DiscordConfig -> Text -> IO (Maybe PendingReview)
simulateReaction cfg key =
  atomically $ do
    rm <- readTVar (dcReviewMap cfg)
    case Map.lookup key rm of
      Nothing -> pure Nothing
      Just pr -> do
        modifyTVar' (dcReviewMap cfg) (Map.delete (prKey pr))
        pure (Just pr)

-- | Simulate what 'eventHandler' does for a 'MessageCreate' inside a forum thread:
-- if 'isApprovalMessage' matches, deregister and run 'prApprove';
-- otherwise run the revision and update 'prCurrentBodyEn'.
-- Mirrors the real @handleRevision@ / @eventHandler@ behaviour:
--   * 'prOnUserMessage' is always called with the incoming text.
--   * 'prOnBotMessage' is called with the new body after a successful revision.
simulateThreadMessage :: DiscordConfig -> Text -> Text -> IO ()
simulateThreadMessage cfg key content = do
  rm <- readTVarIO (dcReviewMap cfg)
  case Map.lookup key rm of
    Nothing -> pure ()
    Just pr -> do
      prOnUserMessage pr content
      if isApprovalMessage content
        then do
          atomically $ modifyTVar' (dcReviewMap cfg) (Map.delete key)
          body <- readTVarIO (prCurrentBodyEn pr)
          bodyPtBr <- readTVarIO (prCurrentBodyPtBr pr)
          tags <- readTVarIO (prCurrentTags pr)
          prApprove pr body bodyPtBr tags
        else do
          currentBody <- readTVarIO (prCurrentBodyEn pr)
          currentBodyPtBr <- readTVarIO (prCurrentBodyPtBr pr)
          (newBody, newBodyPtBr, newTags) <- prRevise pr currentBody currentBodyPtBr content
          atomically $ do
            writeTVar (prCurrentBodyEn pr) newBody
            writeTVar (prCurrentBodyPtBr pr) newBodyPtBr
            writeTVar (prCurrentTags pr) newTags
          prOnBotMessage pr newBody

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "mkDiscordConfig" $ do
    it "creates a config with an empty send queue" $ do
      cfg <- mkDiscordConfig "token" 123 456
      isEmpty <- isEmptyMVar (dcSendQueue cfg)
      isEmpty `shouldBe` True

    it "creates a config with an empty review map" $ do
      cfg <- mkDiscordConfig "token" 123 456
      rm <- readTVarIO (dcReviewMap cfg)
      Map.size rm `shouldBe` 0

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
      cfg <- mkDiscordConfig "token" 1 2
      registerForReview cfg ReviewRequest {rrTitle = "My Title", rrBodyEn = "My Body", rrBodyPtBr = "", rrTags = [], rrRevise = \_ _ _ -> pure ("", "", []), rrApprove = \_ _ _ -> pure (), rrReject = \_ -> pure (), rrOnThreadCreated = \_ -> pure (), rrOnUserMessage = \_ -> pure (), rrOnBotMessage = \_ -> pure ()}
      req <- takeMVar (dcSendQueue cfg)
      rrTitle req `shouldBe` "My Title"
      rrBodyEn req `shouldBe` "My Body"

    it "fires the approve callback with the current body when \x2705 is received" $ do
      cfg <- mkDiscordConfig "token" 1 2
      result <- newIORef ("" :: Text)
      registerForReview cfg ReviewRequest {rrTitle = "T", rrBodyEn = "original-body", rrBodyPtBr = "", rrTags = [], rrRevise = \_ _ _ -> pure ("", "", []), rrApprove = \body _ptbr _tags -> writeIORef result body, rrReject = \_ -> pure (), rrOnThreadCreated = \_ -> pure (), rrOnUserMessage = \_ -> pure (), rrOnBotMessage = \_ -> pure ()}
      _ <- simulateDrain cfg "key-1"
      Just pr' <- simulateReaction cfg "key-1"
      do
        body <- readTVarIO (prCurrentBodyEn pr')
        bodyPtBr <- readTVarIO (prCurrentBodyPtBr pr')
        tags <- readTVarIO (prCurrentTags pr')
        prApprove pr' body bodyPtBr tags
      readIORef result `shouldReturn` "original-body"

    it "fires the reject callback with the emoji when \x274c is received" $ do
      cfg <- mkDiscordConfig "token" 1 2
      rejectRef <- newIORef ("" :: Text)
      registerForReview cfg ReviewRequest {rrTitle = "T", rrBodyEn = "B", rrBodyPtBr = "", rrTags = [], rrRevise = \_ _ _ -> pure ("", "", []), rrApprove = \_ _ _ -> pure (), rrReject = writeIORef rejectRef, rrOnThreadCreated = \_ -> pure (), rrOnUserMessage = \_ -> pure (), rrOnBotMessage = \_ -> pure ()}
      _ <- simulateDrain cfg "key-2"
      Just pr <- simulateReaction cfg "key-2"
      prReject pr emojiReject
      readIORef rejectRef `shouldReturn` emojiReject

    it "removes both map entries before firing the approve callback" $ do
      cfg <- mkDiscordConfig "token" 1 2
      mapSizeRef <- newIORef (-1 :: Int)
      let onApprove _ _ _ = do
            rm <- readTVarIO (dcReviewMap cfg)
            writeIORef mapSizeRef (Map.size rm)
      registerForReview cfg ReviewRequest {rrTitle = "T", rrBodyEn = "B", rrBodyPtBr = "", rrTags = [], rrRevise = \_ _ _ -> pure ("", "", []), rrApprove = onApprove, rrReject = \_ -> pure (), rrOnThreadCreated = \_ -> pure (), rrOnUserMessage = \_ -> pure (), rrOnBotMessage = \_ -> pure ()}
      _ <- simulateDrain cfg "key-3"
      Just pr <- simulateReaction cfg "key-3"
      do
        body <- readTVarIO (prCurrentBodyEn pr)
        bodyPtBr <- readTVarIO (prCurrentBodyPtBr pr)
        tags <- readTVarIO (prCurrentTags pr)
        prApprove pr body bodyPtBr tags
      readIORef mapSizeRef `shouldReturn` 0

    it "handles two concurrent reviews independently" $ do
      cfg <- mkDiscordConfig "token" 1 2
      resultA <- newIORef ("" :: Text)
      resultB <- newIORef ("" :: Text)
      registerForReview
        cfg
        ReviewRequest {rrTitle = "A", rrBodyEn = "bodyA", rrBodyPtBr = "", rrTags = [], rrRevise = \_ _ _ -> pure ("", "", []), rrApprove = \body _ptbr _tags -> writeIORef resultA body, rrReject = \_ -> writeIORef resultA "rejected", rrOnThreadCreated = \_ -> pure (), rrOnUserMessage = \_ -> pure (), rrOnBotMessage = \_ -> pure ()}
      prA <- simulateDrain cfg "key-A"
      registerForReview
        cfg
        ReviewRequest {rrTitle = "B", rrBodyEn = "bodyB", rrBodyPtBr = "", rrTags = [], rrRevise = \_ _ _ -> pure ("", "", []), rrApprove = \body _ptbr _tags -> writeIORef resultB body, rrReject = writeIORef resultB, rrOnThreadCreated = \_ -> pure (), rrOnUserMessage = \_ -> pure (), rrOnBotMessage = \_ -> pure ()}
      prB <- simulateDrain cfg "key-B"
      Just _ <- simulateReaction cfg "key-A"
      Just _ <- simulateReaction cfg "key-B"
      do
        body <- readTVarIO (prCurrentBodyEn prA)
        bodyPtBr <- readTVarIO (prCurrentBodyPtBr prA)
        tags <- readTVarIO (prCurrentTags prA)
        prApprove prA body bodyPtBr tags
      prReject prB emojiReject
      readIORef resultA `shouldReturn` "bodyA"
      readIORef resultB `shouldReturn` emojiReject

  describe "thread message flow" $ do
    it "calls the revise function with current body and feedback" $ do
      cfg <- mkDiscordConfig "token" 1 2
      revCount <- newIORef (0 :: Int)
      let revise _body _ptbr _feedback = modifyIORef revCount (+ 1) >> pure ("revised", "", [])
      registerForReview cfg ReviewRequest {rrTitle = "T", rrBodyEn = "initial", rrBodyPtBr = "", rrTags = [], rrRevise = revise, rrApprove = \_ _ _ -> pure (), rrReject = \_ -> pure (), rrOnThreadCreated = \_ -> pure (), rrOnUserMessage = \_ -> pure (), rrOnBotMessage = \_ -> pure ()}
      _ <- simulateDrain cfg "key-r1"
      simulateThreadMessage cfg "key-r1" "please add more examples"
      readIORef revCount `shouldReturn` 1

    it "updates the current body after a revision" $ do
      cfg <- mkDiscordConfig "token" 1 2
      registerForReview cfg ReviewRequest {rrTitle = "T", rrBodyEn = "initial", rrBodyPtBr = "", rrTags = [], rrRevise = \_ _ _ -> pure ("revised", "", []), rrApprove = \_ _ _ -> pure (), rrReject = \_ -> pure (), rrOnThreadCreated = \_ -> pure (), rrOnUserMessage = \_ -> pure (), rrOnBotMessage = \_ -> pure ()}
      pr <- simulateDrain cfg "key-r2"
      simulateThreadMessage cfg "key-r2" "make it shorter"
      readTVarIO (prCurrentBodyEn pr) `shouldReturn` "revised"

    it "passes the LATEST body to onApprove after revisions" $ do
      cfg <- mkDiscordConfig "token" 1 2
      result <- newIORef ("" :: Text)
      -- First revision changes body to "revised"; second approval should fire with "revised".
      registerForReview cfg ReviewRequest {rrTitle = "T", rrBodyEn = "initial", rrBodyPtBr = "", rrTags = [], rrRevise = \_ _ _ -> pure ("revised", "", []), rrApprove = \body _ptbr _tags -> writeIORef result body, rrReject = \_ -> pure (), rrOnThreadCreated = \_ -> pure (), rrOnUserMessage = \_ -> pure (), rrOnBotMessage = \_ -> pure ()}
      _ <- simulateDrain cfg "key-ap"
      simulateThreadMessage cfg "key-ap" "add examples"
      simulateThreadMessage cfg "key-ap" "publish"
      readIORef result `shouldReturn` "revised"

    it "removes both maps when approved via thread message" $ do
      cfg <- mkDiscordConfig "token" 1 2
      registerForReview cfg ReviewRequest {rrTitle = "T", rrBodyEn = "B", rrBodyPtBr = "", rrTags = [], rrRevise = \_ _ _ -> pure ("", "", []), rrApprove = \_ _ _ -> pure (), rrReject = \_ -> pure (), rrOnThreadCreated = \_ -> pure (), rrOnUserMessage = \_ -> pure (), rrOnBotMessage = \_ -> pure ()}
      _ <- simulateDrain cfg "key-cl"
      simulateThreadMessage cfg "key-cl" "looks good"
      rm <- readTVarIO (dcReviewMap cfg)
      Map.size rm `shouldBe` 0
  describe "callback contract" $ do
    it "rrOnThreadCreated is called with the thread key after drain" $ do
      cfg <- mkDiscordConfig "token" 1 2
      calledWith <- newIORef ("" :: Text)
      registerForReview
        cfg
        ReviewRequest
          { rrTitle = "T",
            rrBodyEn = "B",
            rrBodyPtBr = "",
            rrTags = [],
            rrRevise = \_ _ _ -> pure ("", "", []),
            rrApprove = \_ _ _ -> pure (),
            rrReject = \_ -> pure (),
            rrOnThreadCreated = writeIORef calledWith,
            rrOnUserMessage = \_ -> pure (),
            rrOnBotMessage = \_ -> pure ()
          }
      _ <- simulateDrain cfg "key-tc"
      readIORef calledWith `shouldReturn` "key-tc"

    it "rrOnUserMessage is called with the feedback text" $ do
      cfg <- mkDiscordConfig "token" 1 2
      msgs <- newIORef ([] :: [Text])
      registerForReview
        cfg
        ReviewRequest
          { rrTitle = "T",
            rrBodyEn = "initial",
            rrBodyPtBr = "",
            rrTags = [],
            rrRevise = \_ _ _ -> pure ("revised", "", []),
            rrApprove = \_ _ _ -> pure (),
            rrReject = \_ -> pure (),
            rrOnThreadCreated = \_ -> pure (),
            rrOnUserMessage = \m -> modifyIORef msgs (<> [m]),
            rrOnBotMessage = \_ -> pure ()
          }
      _ <- simulateDrain cfg "key-um"
      simulateThreadMessage cfg "key-um" "add more examples"
      readIORef msgs `shouldReturn` ["add more examples"]

    it "rrOnUserMessage is called with the approval text on text-approval" $ do
      cfg <- mkDiscordConfig "token" 1 2
      msgs <- newIORef ([] :: [Text])
      registerForReview
        cfg
        ReviewRequest
          { rrTitle = "T",
            rrBodyEn = "B",
            rrBodyPtBr = "",
            rrTags = [],
            rrRevise = \_ _ _ -> pure ("", "", []),
            rrApprove = \_ _ _ -> pure (),
            rrReject = \_ -> pure (),
            rrOnThreadCreated = \_ -> pure (),
            rrOnUserMessage = \m -> modifyIORef msgs (<> [m]),
            rrOnBotMessage = \_ -> pure ()
          }
      _ <- simulateDrain cfg "key-ua"
      simulateThreadMessage cfg "key-ua" "looks good to me"
      readIORef msgs `shouldReturn` ["looks good to me"]

    it "rrOnBotMessage is called with the revised body after a revision" $ do
      cfg <- mkDiscordConfig "token" 1 2
      botMsgs <- newIORef ([] :: [Text])
      registerForReview
        cfg
        ReviewRequest
          { rrTitle = "T",
            rrBodyEn = "initial",
            rrBodyPtBr = "",
            rrTags = [],
            rrRevise = \_ _ _ -> pure ("revised-body", "", []),
            rrApprove = \_ _ _ -> pure (),
            rrReject = \_ -> pure (),
            rrOnThreadCreated = \_ -> pure (),
            rrOnUserMessage = \_ -> pure (),
            rrOnBotMessage = \m -> modifyIORef botMsgs (<> [m])
          }
      _ <- simulateDrain cfg "key-bm"
      simulateThreadMessage cfg "key-bm" "shorten it"
      readIORef botMsgs `shouldReturn` ["revised-body"]

    it "rrOnBotMessage accumulates across multiple revisions" $ do
      cfg <- mkDiscordConfig "token" 1 2
      botMsgs <- newIORef ([] :: [Text])
      counter <- newIORef (0 :: Int)
      let revise _ _ _ = do
            n <- modifyIORef counter (+ 1) >> readIORef counter
            pure ("rev-" <> T.pack (show n), "", [])
      registerForReview
        cfg
        ReviewRequest
          { rrTitle = "T",
            rrBodyEn = "initial",
            rrBodyPtBr = "",
            rrTags = [],
            rrRevise = revise,
            rrApprove = \_ _ _ -> pure (),
            rrReject = \_ -> pure (),
            rrOnThreadCreated = \_ -> pure (),
            rrOnUserMessage = \_ -> pure (),
            rrOnBotMessage = \m -> modifyIORef botMsgs (<> [m])
          }
      _ <- simulateDrain cfg "key-multi"
      simulateThreadMessage cfg "key-multi" "first feedback"
      simulateThreadMessage cfg "key-multi" "second feedback"
      readIORef botMsgs `shouldReturn` ["rev-1", "rev-2"]

    it "rrOnBotMessage is NOT called on approval" $ do
      cfg <- mkDiscordConfig "token" 1 2
      botMsgs <- newIORef ([] :: [Text])
      registerForReview
        cfg
        ReviewRequest
          { rrTitle = "T",
            rrBodyEn = "B",
            rrBodyPtBr = "",
            rrTags = [],
            rrRevise = \_ _ _ -> pure ("", "", []),
            rrApprove = \_ _ _ -> pure (),
            rrReject = \_ -> pure (),
            rrOnThreadCreated = \_ -> pure (),
            rrOnUserMessage = \_ -> pure (),
            rrOnBotMessage = \m -> modifyIORef botMsgs (<> [m])
          }
      _ <- simulateDrain cfg "key-bm-ap"
      simulateThreadMessage cfg "key-bm-ap" "publish"
      readIORef botMsgs `shouldReturn` []

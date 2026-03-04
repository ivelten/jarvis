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
import Discord.Types (ChannelId, DiscordId (..), Snowflake (..))
import Orchestrator.Discord.Bot
import Test.Hspec

-- ---------------------------------------------------------------------------
-- Test helpers
-- ---------------------------------------------------------------------------

-- | Synthetic 'ChannelId' used in place of a real Discord thread ID.
testThreadId :: ChannelId
testThreadId = DiscordId (Snowflake 999)

-- | Simulate what 'drainQueue' does after posting a message: take the
-- 'ReviewRequest' from the queue, build a 'PendingReview', and register it in
-- both maps under the supplied synthetic keys.
simulateDrain :: DiscordConfig -> Text -> Text -> IO PendingReview
simulateDrain cfg msgId threadId = do
  req <- takeMVar (dcSendQueue cfg)
  bodyVar <- newTVarIO (rrBody req)
  let pr =
        PendingReview
          { prTitle = rrTitle req,
            prCurrentBody = bodyVar,
            prMsgKey = msgId,
            prThreadKey = threadId,
            prThreadId = testThreadId,
            prRevise = rrRevise req,
            prApprove = rrApprove req,
            prReject = rrReject req
          }
  atomically $ do
    modifyTVar' (dcReviewMap cfg) (Map.insert msgId pr)
    modifyTVar' (dcThreadMap cfg) (Map.insert threadId pr)
  pure pr

-- | Simulate what 'eventHandler' does for a 'MessageReactionAdd': atomically
-- remove from both maps and return the 'PendingReview' so the caller can fire it.
simulateReaction :: DiscordConfig -> Text -> IO (Maybe PendingReview)
simulateReaction cfg msgId =
  atomically $ do
    rm <- readTVar (dcReviewMap cfg)
    case Map.lookup msgId rm of
      Nothing -> pure Nothing
      Just pr -> do
        modifyTVar' (dcReviewMap cfg) (Map.delete (prMsgKey pr))
        modifyTVar' (dcThreadMap cfg) (Map.delete (prThreadKey pr))
        pure (Just pr)

-- | Simulate what 'eventHandler' does for a 'MessageCreate' inside a thread:
-- if 'isApprovalMessage' matches, deregister both maps and run 'prApprove';
-- otherwise run the revision and update 'prCurrentBody'.
simulateThreadMessage :: DiscordConfig -> Text -> Text -> IO ()
simulateThreadMessage cfg threadId content = do
  tm <- readTVarIO (dcThreadMap cfg)
  case Map.lookup threadId tm of
    Nothing -> pure ()
    Just pr ->
      if isApprovalMessage content
        then do
          atomically $ do
            modifyTVar' (dcReviewMap cfg) (Map.delete (prMsgKey pr))
            modifyTVar' (dcThreadMap cfg) (Map.delete threadId)
          readTVarIO (prCurrentBody pr) >>= prApprove pr
        else do
          currentBody <- readTVarIO (prCurrentBody pr)
          newBody <- prRevise pr currentBody content
          atomically $ writeTVar (prCurrentBody pr) newBody

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "mkDiscordConfig" $ do
    it "creates a config with an empty send queue" $ do
      cfg <- mkDiscordConfig "token" 123 456
      isEmpty <- isEmptyMVar (dcSendQueue cfg)
      isEmpty `shouldBe` True

    it "creates a config with empty review and thread maps" $ do
      cfg <- mkDiscordConfig "token" 123 456
      rm <- readTVarIO (dcReviewMap cfg)
      tm <- readTVarIO (dcThreadMap cfg)
      Map.size rm `shouldBe` 0
      Map.size tm `shouldBe` 0

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
      registerForReview cfg ReviewRequest {rrTitle = "My Title", rrBody = "My Body", rrRevise = \_ _ -> pure "", rrApprove = \_ -> pure (), rrReject = \_ -> pure ()}
      req <- takeMVar (dcSendQueue cfg)
      rrTitle req `shouldBe` "My Title"
      rrBody req `shouldBe` "My Body"

    it "fires the approve callback with the current body when \x2705 is received" $ do
      cfg <- mkDiscordConfig "token" 1 2
      result <- newIORef ("" :: Text)
      registerForReview cfg ReviewRequest {rrTitle = "T", rrBody = "original-body", rrRevise = \_ _ -> pure "", rrApprove = writeIORef result, rrReject = \_ -> pure ()}
      pr <- simulateDrain cfg "msg-1" "thr-1"
      Just pr' <- simulateReaction cfg "msg-1"
      readTVarIO (prCurrentBody pr') >>= prApprove pr'
      readIORef result `shouldReturn` "original-body"

    it "fires the reject callback with the emoji when \x274c is received" $ do
      cfg <- mkDiscordConfig "token" 1 2
      rejectRef <- newIORef ("" :: Text)
      registerForReview cfg ReviewRequest {rrTitle = "T", rrBody = "B", rrRevise = \_ _ -> pure "", rrApprove = \_ -> pure (), rrReject = writeIORef rejectRef}
      _ <- simulateDrain cfg "msg-2" "thr-2"
      Just pr <- simulateReaction cfg "msg-2"
      prReject pr "\x274c"
      readIORef rejectRef `shouldReturn` "\x274c"

    it "removes both map entries before firing the approve callback" $ do
      cfg <- mkDiscordConfig "token" 1 2
      mapSizeRef <- newIORef (-1 :: Int)
      let onApprove _ = do
            rm <- readTVarIO (dcReviewMap cfg)
            tm <- readTVarIO (dcThreadMap cfg)
            writeIORef mapSizeRef (Map.size rm + Map.size tm)
      registerForReview cfg ReviewRequest {rrTitle = "T", rrBody = "B", rrRevise = \_ _ -> pure "", rrApprove = onApprove, rrReject = \_ -> pure ()}
      _ <- simulateDrain cfg "msg-3" "thr-3"
      Just pr <- simulateReaction cfg "msg-3"
      readTVarIO (prCurrentBody pr) >>= prApprove pr
      readIORef mapSizeRef `shouldReturn` 0

    it "handles two concurrent reviews independently" $ do
      cfg <- mkDiscordConfig "token" 1 2
      resultA <- newIORef ("" :: Text)
      resultB <- newIORef ("" :: Text)
      registerForReview
        cfg
        ReviewRequest {rrTitle = "A", rrBody = "bodyA", rrRevise = \_ _ -> pure "", rrApprove = writeIORef resultA, rrReject = \_ -> writeIORef resultA "rejected"}
      prA <- simulateDrain cfg "msg-A" "thr-A"
      registerForReview
        cfg
        ReviewRequest {rrTitle = "B", rrBody = "bodyB", rrRevise = \_ _ -> pure "", rrApprove = writeIORef resultB, rrReject = writeIORef resultB}
      prB <- simulateDrain cfg "msg-B" "thr-B"
      Just _ <- simulateReaction cfg "msg-A"
      Just _ <- simulateReaction cfg "msg-B"
      readTVarIO (prCurrentBody prA) >>= prApprove prA
      prReject prB "\x274c"
      readIORef resultA `shouldReturn` "bodyA"
      readIORef resultB `shouldReturn` "\x274c"

  describe "thread message flow" $ do
    it "calls the revise function with current body and feedback" $ do
      cfg <- mkDiscordConfig "token" 1 2
      revCount <- newIORef (0 :: Int)
      let revise _body _feedback = modifyIORef revCount (+ 1) >> pure "revised"
      registerForReview cfg ReviewRequest {rrTitle = "T", rrBody = "initial", rrRevise = revise, rrApprove = \_ -> pure (), rrReject = \_ -> pure ()}
      _ <- simulateDrain cfg "msg-r1" "thr-r1"
      simulateThreadMessage cfg "thr-r1" "please add more examples"
      readIORef revCount `shouldReturn` 1

    it "updates the current body after a revision" $ do
      cfg <- mkDiscordConfig "token" 1 2
      registerForReview cfg ReviewRequest {rrTitle = "T", rrBody = "initial", rrRevise = \_ _ -> pure "revised", rrApprove = \_ -> pure (), rrReject = \_ -> pure ()}
      pr <- simulateDrain cfg "msg-r2" "thr-r2"
      simulateThreadMessage cfg "thr-r2" "make it shorter"
      readTVarIO (prCurrentBody pr) `shouldReturn` "revised"

    it "passes the LATEST body to onApprove after revisions" $ do
      cfg <- mkDiscordConfig "token" 1 2
      result <- newIORef ("" :: Text)
      -- First revision changes body to "revised"; second approval should fire with "revised".
      registerForReview cfg ReviewRequest {rrTitle = "T", rrBody = "initial", rrRevise = \_ _ -> pure "revised", rrApprove = writeIORef result, rrReject = \_ -> pure ()}
      _ <- simulateDrain cfg "msg-ap" "thr-ap"
      simulateThreadMessage cfg "thr-ap" "add examples"
      simulateThreadMessage cfg "thr-ap" "publish"
      readIORef result `shouldReturn` "revised"

    it "removes both maps when approved via thread message" $ do
      cfg <- mkDiscordConfig "token" 1 2
      registerForReview cfg ReviewRequest {rrTitle = "T", rrBody = "B", rrRevise = \_ _ -> pure "", rrApprove = \_ -> pure (), rrReject = \_ -> pure ()}
      _ <- simulateDrain cfg "msg-cl" "thr-cl"
      simulateThreadMessage cfg "thr-cl" "looks good"
      rm <- readTVarIO (dcReviewMap cfg)
      tm <- readTVarIO (dcThreadMap cfg)
      Map.size rm `shouldBe` 0
      Map.size tm `shouldBe` 0

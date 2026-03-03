-- | Unit tests for 'Orchestrator.Discord.Bot'.
--
-- These tests exercise the shared-state contract between 'sendForReview',
-- 'awaitReview', and the bot's internal queue / review-map — all without
-- making a real Discord connection.
module Orchestrator.Discord.BotSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Orchestrator.Discord.Bot
import Test.Hspec

spec :: Spec
spec = do
  describe "mkDiscordConfig" $ do
    it "creates a config with an empty send queue" $ do
      cfg <- mkDiscordConfig "token" 123 456
      isEmpty <- isEmptyMVar (dcSendQueue cfg)
      isEmpty `shouldBe` True

    it "creates a config with an empty review map" $ do
      cfg <- mkDiscordConfig "token" 123 456
      m <- readTVarIO (dcReviewMap cfg)
      Map.size m `shouldBe` 0

  describe "sendForReview" $ do
    it "queues the title and body, and returns the supplied message ID" $ do
      cfg <- mkDiscordConfig "token" 1 2
      -- Simulate the bot worker: drain the queue and fill in a message ID.
      _ <- forkIO $ do
        req <- takeMVar (dcSendQueue cfg)
        srTitle req `shouldBe` "My Title"
        srBody req `shouldBe` "My Body"
        putMVar (srReply req) "discord-msg-42"
      msgId <- sendForReview cfg "My Title" "My Body"
      msgId `shouldBe` "discord-msg-42"

  describe "awaitReview" $ do
    it "returns Approved when Approved is delivered" $ do
      cfg <- mkDiscordConfig "token" 1 2
      resultVar <- newEmptyMVar
      _ <- forkIO $ awaitReview cfg "msg-001" >>= putMVar resultVar
      -- Wait for awaitReview to register itself in the review map.
      var <- atomically $ do
        m <- readTVar (dcReviewMap cfg)
        case Map.lookup "msg-001" m of
          Nothing -> retry
          Just v -> pure v
      putMVar var Approved
      takeMVar resultVar `shouldReturn` Approved

    it "returns Rejected when Rejected is delivered" $ do
      cfg <- mkDiscordConfig "token" 1 2
      resultVar <- newEmptyMVar
      _ <- forkIO $ awaitReview cfg "msg-002" >>= putMVar resultVar
      var <- atomically $ do
        m <- readTVar (dcReviewMap cfg)
        case Map.lookup "msg-002" m of
          Nothing -> retry
          Just v -> pure v
      putMVar var (Rejected "not relevant")
      takeMVar resultVar `shouldReturn` Rejected "not relevant"

    it "removes the entry from the review map after resolution" $ do
      cfg <- mkDiscordConfig "token" 1 2
      resultVar <- newEmptyMVar
      _ <- forkIO $ awaitReview cfg "msg-003" >>= putMVar resultVar
      var <- atomically $ do
        m <- readTVar (dcReviewMap cfg)
        case Map.lookup "msg-003" m of
          Nothing -> retry
          Just v -> pure v
      putMVar var Approved
      _ <- takeMVar resultVar
      -- Give the forked thread a moment to finish the STM cleanup.
      threadDelay 10_000
      m <- readTVarIO (dcReviewMap cfg)
      Map.member "msg-003" m `shouldBe` False

    it "handles concurrent reviews independently" $ do
      cfg <- mkDiscordConfig "token" 1 2
      varA <- newEmptyMVar
      varB <- newEmptyMVar
      _ <- forkIO $ awaitReview cfg "msg-A" >>= putMVar varA
      _ <- forkIO $ awaitReview cfg "msg-B" >>= putMVar varB
      -- Wait for both registrations.
      (mvA, mvB) <- atomically $ do
        m <- readTVar (dcReviewMap cfg)
        case (Map.lookup "msg-A" m, Map.lookup "msg-B" m) of
          (Just a, Just b) -> pure (a, b)
          _ -> retry
      putMVar mvB (Rejected "B rejected")
      putMVar mvA Approved
      takeMVar varA `shouldReturn` Approved
      takeMVar varB `shouldReturn` Rejected "B rejected"

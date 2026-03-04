{-# LANGUAGE RecordWildCards #-}

module Orchestrator.Discord.Bot
  ( DiscordConfig (..),
    PendingReview (..),
    ReviewRequest (..),
    mkDiscordConfig,
    registerForReview,
    isApprovalMessage,
    startBot,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, runReaderT)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Discord
import Discord.Requests (ChannelRequest (..), MessageDetailedOpts (..), StartThreadOpts (..))
import Discord.Types

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | State for a draft currently under review in a Discord thread.
data PendingReview = PendingReview
  { -- | Post title (used for context).
    prTitle :: !Text,
    -- | Current draft body; updated atomically after each AI revision.
    prCurrentBody :: !(TVar Text),
    -- | Key in 'dcReviewMap': original channel message Snowflake as text.
    prMsgKey :: !Text,
    -- | Key in 'dcThreadMap': thread channel Snowflake as text.
    prThreadKey :: !Text,
    -- | Thread 'ChannelId' for posting revision replies.
    prThreadId :: !ChannelId,
    -- | @(currentBody, feedback) -> revisedBody@.  Called on every thread message.
    prRevise :: Text -> Text -> IO Text,
    -- | Called with the final body when ✅ is received or an approval phrase is typed.
    prApprove :: Text -> IO (),
    -- | Called with the rejection reason when ❌ is received.
    prReject :: Text -> IO ()
  }

-- | All the information needed to submit a draft for Discord review.
-- Passed to 'registerForReview' as a single record instead of individual arguments.
data ReviewRequest = ReviewRequest
  { -- | Post title shown in the embed and as the thread name.
    rrTitle :: !Text,
    -- | Full Markdown draft body.
    rrBody :: !Text,
    -- | @(currentBody, feedback) -> revisedBody@.  Called for each reviewer
    -- message in the thread that is not an approval.
    rrRevise :: Text -> Text -> IO Text,
    -- | Called with the final body when the reviewer approves (✅ reaction or
    -- approval phrase in the thread).
    rrApprove :: Text -> IO (),
    -- | Called with the rejection reason (e.g. the ❌ emoji) when rejected.
    rrReject :: Text -> IO ()
  }

-- | Runtime configuration for the Discord bot.
data DiscordConfig = DiscordConfig
  { -- | Discord bot token (from environment, without the @Bot @ prefix).
    dcBotToken :: !Text,
    -- | Personal server (guild) ID.
    dcGuildId :: !Word,
    -- | Review channel ID where embed messages are posted.
    dcChannelId :: !Word,
    -- | Internal queue: pipeline puts review requests here.
    dcSendQueue :: !(MVar ReviewRequest),
    -- | Map from original-message-ID text to pending review (for reaction lookup).
    dcReviewMap :: !(TVar (Map Text PendingReview)),
    -- | Map from thread-channel-ID text to pending review (for thread-message lookup).
    dcThreadMap :: !(TVar (Map Text PendingReview))
  }

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Smart constructor — allocates the internal communication channels.
mkDiscordConfig :: Text -> Word -> Word -> IO DiscordConfig
mkDiscordConfig token guildId channelId = do
  sendQueue <- newEmptyMVar
  reviewMap <- newTVarIO Map.empty
  threadMap <- newTVarIO Map.empty
  pure
    DiscordConfig
      { dcBotToken = token,
        dcGuildId = guildId,
        dcChannelId = channelId,
        dcSendQueue = sendQueue,
        dcReviewMap = reviewMap,
        dcThreadMap = threadMap
      }

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Submit a draft to the Discord review channel and return immediately.
--
-- The bot posts an embed in the review channel, spawns a thread from that
-- message, and posts the full draft as the first thread message.  Subsequent
-- reviewer messages in the thread trigger AI revisions.  Reacting ✅ (or
-- typing an approval phrase) passes the final body to 'rrApprove'; reacting
-- ❌ passes the emoji to 'rrReject'.
registerForReview :: DiscordConfig -> ReviewRequest -> IO ()
registerForReview cfg = putMVar (dcSendQueue cfg)

-- | Pure predicate: does a message text constitute an approval?
--
-- Exported so it can be unit-tested without a Discord connection.
isApprovalMessage :: Text -> Bool
isApprovalMessage t =
  any
    (`T.isInfixOf` T.toLower t)
    ["publish", "approve", "lgtm", "looks good", "ship it", "done", "go ahead", "deploy"]

-- | Start the long-running Discord bot event loop.
-- This function blocks indefinitely; call it on the main thread or a dedicated thread.
startBot :: DiscordConfig -> IO ()
startBot cfg = do
  err <-
    runDiscord $
      def
        { discordToken = "Bot " <> dcBotToken cfg,
          discordGatewayIntent =
            def
              { gatewayIntentMessageReactions = True,
                gatewayIntentMessageChanges = True,
                gatewayIntentMessageContent = True
              },
          discordOnStart = botWorker cfg,
          discordOnEvent = eventHandler cfg,
          discordOnLog = \t -> putStrLn $ "[Discord] " <> T.unpack t
        }
  putStrLn $ "[Discord] bot stopped: " <> T.unpack err

-- ---------------------------------------------------------------------------
-- Internal bot workers
-- ---------------------------------------------------------------------------

-- | Launched from 'discordOnStart': forks a thread that drains the send
-- queue and sends embed messages on behalf of the pipeline.
botWorker :: DiscordConfig -> DiscordHandler ()
botWorker cfg = do
  hdl <- ask
  liftIO $ void $ forkIO $ drainQueue hdl cfg

-- | Continuously reads 'SendRequest' values from the queue, posts an embed in
-- the review channel, creates a Discord thread from that message, posts the
-- full draft body as the first thread message, and registers the review in both
-- lookup maps.
drainQueue :: DiscordHandle -> DiscordConfig -> IO ()
drainQueue hdl cfg = forever $ do
  ReviewRequest {..} <- takeMVar (dcSendQueue cfg)
  let chanId = mkChannelId (dcChannelId cfg)
      embed =
        def
          { createEmbedTitle = rrTitle,
            createEmbedDescription = T.take 500 rrBody,
            createEmbedFooterText =
              "React \x2705 to approve, \x274c to reject, or type feedback in the thread."
          }
      opts = def {messageDetailedEmbeds = Just [embed]}
  result <- runReaderT (restCall (CreateMessageDetailed chanId opts)) hdl
  case result of
    Left err ->
      putStrLn $ "[Discord] failed to send review message: " <> show err
    Right msg -> do
      let msgKey = showId (messageId msg)
          threadOpts = def {startThreadName = rrTitle}
      threadResult <-
        runReaderT
          (restCall (StartThreadFromMessage chanId (messageId msg) threadOpts))
          hdl
      case threadResult of
        Left tErr ->
          putStrLn $ "[Discord] failed to create review thread: " <> show tErr
        Right thread -> do
          let tid = channelId thread
              threadKey = showId tid
          -- Post full draft body in the thread.
          void $
            runReaderT
              (restCall (CreateMessage tid ("\x1f4dd **Draft:**\n\n" <> T.take 1_900 rrBody)))
              hdl
          -- Build and register the PendingReview.
          bodyVar <- newTVarIO rrBody
          let pr =
                PendingReview
                  { prTitle = rrTitle,
                    prCurrentBody = bodyVar,
                    prMsgKey = msgKey,
                    prThreadKey = threadKey,
                    prThreadId = tid,
                    prRevise = rrRevise,
                    prApprove = rrApprove,
                    prReject = rrReject
                  }
          atomically $ do
            modifyTVar' (dcReviewMap cfg) (Map.insert msgKey pr)
            modifyTVar' (dcThreadMap cfg) (Map.insert threadKey pr)
          -- Add reaction affordances to the original embed message.
          void $
            runReaderT
              (restCall (CreateReaction (chanId, messageId msg) "\x2705"))
              hdl
          void $
            runReaderT
              (restCall (CreateReaction (chanId, messageId msg) "\x274c"))
              hdl

-- | Handle incoming Discord events.
--
-- * 'MessageReactionAdd' on a known review message: approve (✅) or reject (❌).
-- * 'MessageCreate' inside a known review thread: if 'isApprovalMessage' returns
--   'True', approve with the current body; otherwise call the AI revision
--   function and post the revised draft back.
eventHandler :: DiscordConfig -> Event -> DiscordHandler ()
-- \| Reaction-based approval / rejection.
eventHandler cfg (MessageReactionAdd ri) = do
  cache <- readCache
  let botId = userId (cacheCurrentUser cache)
  when (reactionUserId ri /= botId) $ do
    let msgKey = showId (reactionMessageId ri)
        emoji = emojiName (reactionEmoji ri)
    mReview <- liftIO $ atomically (lookupAndDeregister cfg msgKey)
    case mReview of
      Nothing -> pure ()
      Just pr ->
        liftIO $
          if emoji == "\x2705"
            then readTVarIO (prCurrentBody pr) >>= prApprove pr
            else prReject pr emoji
-- \| Thread-message-based revision / approval.
eventHandler cfg (MessageCreate m) = do
  cache <- readCache
  let botId = userId (cacheCurrentUser cache)
  when (not (userIsBot (messageAuthor m)) && userId (messageAuthor m) /= botId) $ do
    let threadKey = showId (messageChannelId m)
    threadMapSnap <- liftIO $ readTVarIO (dcThreadMap cfg)
    case Map.lookup threadKey threadMapSnap of
      Nothing -> pure ()
      Just pr ->
        if isApprovalMessage (messageContent m)
          then liftIO $ do
            atomically $ deregisterBoth cfg pr
            readTVarIO (prCurrentBody pr) >>= prApprove pr
          else handleRevision pr (messageContent m)
eventHandler _ _ = pure ()

-- | Atomically look up a pending review by original-message key and remove
-- both map entries so callbacks cannot fire more than once.
lookupAndDeregister :: DiscordConfig -> Text -> STM (Maybe PendingReview)
lookupAndDeregister cfg msgKey = do
  rm <- readTVar (dcReviewMap cfg)
  case Map.lookup msgKey rm of
    Nothing -> pure Nothing
    Just pr -> do
      deregisterBoth cfg pr
      pure (Just pr)

-- | Atomically remove a pending review from both maps.
deregisterBoth :: DiscordConfig -> PendingReview -> STM ()
deregisterBoth cfg pr = do
  modifyTVar' (dcReviewMap cfg) (Map.delete (prMsgKey pr))
  modifyTVar' (dcThreadMap cfg) (Map.delete (prThreadKey pr))

-- | Call the AI revision function and post the result back into the thread.
-- On failure, post an error message instead.
handleRevision :: PendingReview -> Text -> DiscordHandler ()
handleRevision pr feedback = do
  result <- liftIO $ do
    currentBody <- readTVarIO (prCurrentBody pr)
    try (prRevise pr currentBody feedback) :: IO (Either SomeException Text)
  case result of
    Left ex ->
      void $
        restCall $
          CreateMessage
            (prThreadId pr)
            ("\x26a0\xfe0f Could not revise draft: " <> T.pack (show ex))
    Right newBody -> do
      liftIO $ atomically $ writeTVar (prCurrentBody pr) newBody
      void $
        restCall $
          CreateMessage
            (prThreadId pr)
            ("\x270f\xfe0f **Revised draft:**\n\n" <> T.take 1_900 newBody)

-- ---------------------------------------------------------------------------
-- Utilities
-- ---------------------------------------------------------------------------

-- | Convert a raw Word ID to a Discord 'ChannelId'.
mkChannelId :: Word -> ChannelId
mkChannelId w = DiscordId (Snowflake (fromIntegral w))

-- | Render any Discord snowflake-based ID to text for use as a map key.
showId :: DiscordId a -> Text
showId i = T.pack $ show $ unSnowflake $ unId i

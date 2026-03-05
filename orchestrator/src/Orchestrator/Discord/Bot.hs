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

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, runReaderT)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Discord
import Discord.Requests (ChannelRequest (..), StartThreadForumMediaMessage (..), StartThreadForumMediaOpts (..), StartThreadOpts (..))
import Discord.Types
import Orchestrator.TextUtils (chunkText, truncateText)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | State for a draft currently under review in a Discord forum thread.
data PendingReview = PendingReview
  { -- | Post title (used for context).
    prTitle :: !Text,
    -- | Current draft body; updated atomically after each AI revision.
    prCurrentBody :: !(TVar Text),
    -- | Current tags; updated atomically alongside 'prCurrentBody' after each revision.
    prCurrentTags :: !(TVar [Text]),
    -- | Lookup key: forum thread Snowflake as text.  Because in a forum
    -- channel the thread ID equals the starter message ID, one key serves
    -- both reaction events and thread-message events.
    prKey :: !Text,
    -- | Thread 'ChannelId' for posting revision replies.
    prThreadId :: !ChannelId,
    -- | @(currentBody, feedback) -> (revisedBody, revisedTags)@.  Called on every thread message.
    prRevise :: Text -> Text -> IO (Text, [Text]),
    -- | Called with the final body and tags when ✅ is received or an approval phrase is typed.
    prApprove :: Text -> [Text] -> IO (),
    -- | Called with the rejection reason when ❌ is received.
    prReject :: Text -> IO (),
    -- | Called with the text of each message the human reviewer sends.
    prOnUserMessage :: Text -> IO (),
    -- | Called with the text of each reply posted by the bot (revised drafts).
    prOnBotMessage :: Text -> IO ()
  }

-- | All the information needed to submit a draft for Discord review.
-- Passed to 'registerForReview' as a single record instead of individual arguments.
data ReviewRequest = ReviewRequest
  { -- | Post title shown in the embed and as the thread name.
    rrTitle :: !Text,
    -- | Full Markdown draft body.
    rrBody :: !Text,
    -- | Initial tags from the draft; kept in sync with the body after revisions.
    rrTags :: ![Text],
    -- | @(currentBody, feedback) -> (revisedBody, revisedTags)@.  Called for each reviewer
    -- message in the thread that is not an approval.
    rrRevise :: Text -> Text -> IO (Text, [Text]),
    -- | Called with the final body and tags when the reviewer approves (✅ reaction or
    -- approval phrase in the thread).
    rrApprove :: Text -> [Text] -> IO (),
    -- | Called with the rejection reason (e.g. the ❌ emoji) when rejected.
    rrReject :: Text -> IO (),
    -- | Called with the text of each message the human reviewer sends in the thread
    -- (feedback, approval phrase, or rejection emoji from a reaction).
    rrOnUserMessage :: Text -> IO (),
    -- | Called with the text of each reply the bot posts (i.e. a revised draft).
    rrOnBotMessage :: Text -> IO (),
    -- | Called with the Discord thread ID (as 'Text') once the review thread
    -- has been created.  Useful for persisting the thread ID back to the DB.
    rrOnThreadCreated :: Text -> IO ()
  }

-- | Runtime configuration for the Discord bot.
data DiscordConfig = DiscordConfig
  { -- | Discord bot token (from environment, without the @Bot @ prefix).
    dcBotToken :: !Text,
    -- | Personal server (guild) ID.
    dcGuildId :: !Word,
    -- | Forum channel ID where review threads are created.
    dcChannelId :: !Word,
    -- | Internal queue: pipeline puts review requests here.
    dcSendQueue :: !(MVar ReviewRequest),
    -- | Map from forum-thread-ID text to pending review.  Because the forum
    -- thread ID equals the starter message ID, this single map handles both
    -- reaction events and thread-message events.
    dcReviewMap :: !(TVar (Map Text PendingReview))
  }

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Smart constructor — allocates the internal communication channels.
mkDiscordConfig :: Text -> Word -> Word -> IO DiscordConfig
mkDiscordConfig token guildId channelId = do
  sendQueue <- newEmptyMVar
  reviewMap <- newTVarIO Map.empty
  pure
    DiscordConfig
      { dcBotToken = token,
        dcGuildId = guildId,
        dcChannelId = channelId,
        dcSendQueue = sendQueue,
        dcReviewMap = reviewMap
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

-- | Continuously reads 'ReviewRequest' values from the queue, creates a forum
-- thread with the embed as the starter message, posts the full draft body as a
-- follow-up, and registers the review in 'dcReviewMap'.
--
-- Because Discord forum channels assign the same snowflake to the thread and
-- its starter message, a single map key handles both reaction events and
-- thread-message events.
drainQueue :: DiscordHandle -> DiscordConfig -> IO ()
drainQueue hdl cfg = forever $ do
  ReviewRequest {..} <- takeMVar (dcSendQueue cfg)
  let chanId = mkChannelId (dcChannelId cfg)
      embed =
        def
          { createEmbedTitle = rrTitle,
            createEmbedDescription = truncateText 500 rrBody,
            createEmbedFooterText =
              "React " <> emojiApprove <> " to approve, " <> emojiReject <> " to reject, or type feedback in the thread."
          }
      forumMsg =
        StartThreadForumMediaMessage
          { startThreadForumMediaMessageContent = "",
            startThreadForumMediaMessageEmbeds = Just [embed],
            startThreadForumMediaMessageAllowedMentions = Nothing,
            startThreadForumMediaMessageComponents = Nothing,
            startThreadForumMediaMessageStickerIds = Nothing
          }
      forumOpts =
        StartThreadForumMediaOpts
          { startThreadForumMediaBaseOpts = def {startThreadName = rrTitle},
            startThreadForumMediaMessage = forumMsg,
            startThreadForumMediaAppliedTags = Nothing
          }
  result <- runReaderT (restCall (StartThreadForumMedia chanId forumOpts)) hdl
  case result of
    Left err ->
      putStrLn $ "[Discord] failed to create forum thread: " <> show err
    Right thread -> do
      let tid = channelId thread
          key = showId tid
          -- In a forum channel the starter message ID equals the thread ID.
          starterMsgId = DiscordId (unId tid) :: MessageId
      -- Post full draft body as the first thread reply (split across messages if needed).
      postChunked hdl tid (emojiDraft <> " **Draft:**\n\n") rrBody
      -- Wait before adding reactions so they don't compete for the rate-limit
      -- bucket with the last draft chunk.
      threadDelay 2_000_000
      -- Build and register the PendingReview.
      bodyVar <- newTVarIO rrBody
      tagsVar <- newTVarIO rrTags
      let pr =
            PendingReview
              { prTitle = rrTitle,
                prCurrentBody = bodyVar,
                prCurrentTags = tagsVar,
                prKey = key,
                prThreadId = tid,
                prRevise = rrRevise,
                prApprove = rrApprove,
                prReject = rrReject,
                prOnUserMessage = rrOnUserMessage,
                prOnBotMessage = rrOnBotMessage
              }
      atomically $ modifyTVar' (dcReviewMap cfg) (Map.insert key pr)
      -- Notify the caller that the thread has been created.
      rrOnThreadCreated key
      -- Add reaction affordances to the forum thread starter message.
      -- The starter message lives inside the thread channel (tid), not the
      -- parent forum channel (chanId).
      restCallIO hdl (CreateReaction (tid, starterMsgId) emojiApprove)
      restCallIO hdl (CreateReaction (tid, starterMsgId) emojiReject)

-- | Handle incoming Discord events.
--
-- * 'MessageReactionAdd' on a forum starter message: approve (✅) or reject (❌).
--   The starter message ID equals the thread ID, so 'dcReviewMap' is used.
-- * 'MessageCreate' inside a review thread: if 'isApprovalMessage' returns
--   'True', approve with the current body; otherwise call the AI revision
--   function and post the revised draft back.
eventHandler :: DiscordConfig -> Event -> DiscordHandler ()
-- \| Reaction-based approval / rejection.
eventHandler cfg (MessageReactionAdd ri) = do
  cache <- readCache
  let botId = userId (cacheCurrentUser cache)
  when (reactionUserId ri /= botId) $ do
    let key = showId (reactionMessageId ri)
        emoji = emojiName (reactionEmoji ri)
    mReview <- liftIO $ atomically (lookupAndDeregister cfg key)
    case mReview of
      Nothing -> pure ()
      Just pr ->
        liftIO $
          if emoji == emojiApprove
            then do
              prOnUserMessage pr emoji
              body <- readTVarIO (prCurrentBody pr)
              tags <- readTVarIO (prCurrentTags pr)
              prApprove pr body tags
            else do
              prOnUserMessage pr emoji
              prReject pr emoji
-- \| Thread-message-based revision / approval.
eventHandler cfg (MessageCreate m) = do
  cache <- readCache
  let botId = userId (cacheCurrentUser cache)
  when (not (userIsBot (messageAuthor m)) && userId (messageAuthor m) /= botId) $ do
    let key = showId (messageChannelId m)
    reviewMapSnap <- liftIO $ readTVarIO (dcReviewMap cfg)
    case Map.lookup key reviewMapSnap of
      Nothing -> pure ()
      Just pr ->
        if isApprovalMessage (messageContent m)
          then liftIO $ do
            atomically $ deregister cfg key
            prOnUserMessage pr (messageContent m)
            body <- readTVarIO (prCurrentBody pr)
            tags <- readTVarIO (prCurrentTags pr)
            prApprove pr body tags
          else handleRevision pr (messageContent m)
eventHandler _ _ = pure ()

-- | Atomically look up a pending review by key and remove it so the
-- callback cannot fire more than once.
lookupAndDeregister :: DiscordConfig -> Text -> STM (Maybe PendingReview)
lookupAndDeregister cfg key = do
  rm <- readTVar (dcReviewMap cfg)
  case Map.lookup key rm of
    Nothing -> pure Nothing
    Just pr -> do
      deregister cfg key
      pure (Just pr)

-- | Atomically remove a pending review from 'dcReviewMap'.
deregister :: DiscordConfig -> Text -> STM ()
deregister cfg key =
  modifyTVar' (dcReviewMap cfg) (Map.delete key)

-- | Call the AI revision function and post the result back into the thread.
-- On failure, post an error message instead.
handleRevision :: PendingReview -> Text -> DiscordHandler ()
handleRevision pr feedback = do
  liftIO $ prOnUserMessage pr feedback
  result <- liftIO $ do
    currentBody <- readTVarIO (prCurrentBody pr)
    try (prRevise pr currentBody feedback) :: IO (Either SomeException (Text, [Text]))
  case result of
    Left ex -> do
      let errMsg = "Could not revise draft: " <> T.pack (displayException ex)
      liftIO $ putStrLn $ "[Drafts] Gemini error: " <> T.unpack errMsg
      void $
        restCall $
          CreateMessage
            (prThreadId pr)
            (emojiWarning <> " " <> errMsg)
    Right (newBody, newTags) -> do
      liftIO $ atomically $ do
        writeTVar (prCurrentBody pr) newBody
        writeTVar (prCurrentTags pr) newTags
      liftIO $ prOnBotMessage pr newBody
      hdl <- ask
      liftIO $ postChunked hdl (prThreadId pr) (emojiRevise <> " **Revised draft:**\n\n") newBody

-- ---------------------------------------------------------------------------
-- Utilities
-- ---------------------------------------------------------------------------

-- | Convert a raw Word ID to a Discord 'ChannelId'.
mkChannelId :: Word -> ChannelId
mkChannelId w = DiscordId (Snowflake (fromIntegral w))

-- | Render any Discord snowflake-based ID to text for use as a map key.
showId :: DiscordId a -> Text
showId i = T.pack $ show $ unSnowflake $ unId i

-- | Fire a Discord REST call via a raw handle, discarding the result.
restCallIO :: (FromJSON a) => DiscordHandle -> ChannelRequest a -> IO ()
restCallIO hdl req = void $ runReaderT (restCall req) hdl

-- Discord emoji constants -------------------------------------------------

emojiApprove, emojiReject, emojiDraft, emojiRevise, emojiWarning :: Text
emojiApprove = "\x2705" -- ✅
emojiReject = "\x274c" -- ❌
emojiDraft = "\x1f4dd" -- 📝
emojiRevise = "\x270f\xfe0f" -- ✏️
emojiWarning = "\x26a0\xfe0f" -- ⚠️

-- | Post a (potentially long) body to a Discord channel, splitting it into
-- multiple messages of at most 1900 characters each. The @header@ is
-- prepended to the first message only.
--
-- A 2-second delay is inserted between consecutive messages to stay well
-- within Discord's per-channel rate limit (5 messages / 5 seconds).
postChunked :: DiscordHandle -> ChannelId -> Text -> Text -> IO ()
postChunked hdl tid header body = do
  let chunks = chunkText 1_900 body
      messages = case chunks of
        [] -> [header]
        (c : cs) -> (header <> c) : cs
      total = length messages
  putStrLn $
    "[Discord] Sending draft in "
      <> show total
      <> " message(s), body length: "
      <> show (T.length body)
      <> " chars."
  mapM_ send (zip [1 :: Int ..] messages)
  where
    send (i, msg) = do
      when (i > 1) $ threadDelay 2_000_000 -- 2 s between messages
      result <- runReaderT (restCall (CreateMessage tid msg)) hdl
      case result of
        Left err ->
          putStrLn $
            "[Discord] Failed to send message chunk "
              <> show i
              <> ": "
              <> show err
        Right _ ->
          putStrLn $
            "[Discord] Sent chunk "
              <> show i
              <> " ("
              <> show (T.length msg)
              <> " chars)."

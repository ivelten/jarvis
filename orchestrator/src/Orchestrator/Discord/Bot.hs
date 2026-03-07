{-# LANGUAGE RecordWildCards #-}

module Orchestrator.Discord.Bot
  ( DiscordConfig (..),
    PendingReview (..),
    ReviewRequest (..),
    mkDiscordConfig,
    registerForReview,
    restoreReview,
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
import Data.Text.Encoding (encodeUtf8)
import Discord
import Discord.Requests (ChannelRequest (..), MessageDetailedOpts (..), StartThreadForumMediaMessage (..), StartThreadForumMediaOpts (..), StartThreadOpts (..))
import Discord.Types
import Orchestrator.TextUtils (truncateText)
import Text.Read (readMaybe)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | State for a draft currently under review in a Discord forum thread.
data PendingReview = PendingReview
  { -- | Post title (used for context).
    prTitle :: !Text,
    -- | Current English draft body; updated atomically after each AI revision.
    prCurrentBodyEn :: !(TVar Text),
    -- | Current tags; updated atomically alongside the body after each revision.
    prCurrentTags :: !(TVar [Text]),
    -- | Lookup key: forum thread Snowflake as text.  Because in a forum
    -- channel the thread ID equals the starter message ID, one key serves
    -- both reaction events and thread-message events.
    prKey :: !Text,
    -- | Thread 'ChannelId' for posting revision replies.
    prThreadId :: !ChannelId,
    -- | @(currentBodyEn, feedback) -> (revisedBodyEn, revisedTags)@.
    prRevise :: Text -> Text -> IO (Text, [Text]),
    -- | Called with the final body and tags when ✅ is received or an approval phrase is typed.
    prApprove :: Text -> [Text] -> IO (),
    -- | Called with the rejection reason when ❌ is received.
    prReject :: Text -> IO (),
    -- | Called with the text of each message the human reviewer sends.
    prOnUserMessage :: Text -> IO (),
    -- | Called with the English body of each reply posted by the bot (revised drafts).
    prOnBotMessage :: Text -> IO ()
  }

-- | All the information needed to submit a draft for Discord review.
-- Passed to 'registerForReview' as a single record instead of individual arguments.
data ReviewRequest = ReviewRequest
  { -- | Post title shown in the embed and as the thread name.
    rrTitle :: !Text,
    -- | Full English Markdown draft body.
    rrBodyEn :: !Text,
    -- | Initial tags from the draft; kept in sync with the body after revisions.
    rrTags :: ![Text],
    -- | @(currentBodyEn, feedback) -> (revisedBodyEn, revisedTags)@.
    rrRevise :: Text -> Text -> IO (Text, [Text]),
    -- | Called with the final body and tags when the reviewer approves.
    rrApprove :: Text -> [Text] -> IO (),
    -- | Called with the rejection reason when rejected.
    rrReject :: Text -> IO (),
    -- | Called with the text of each message the human reviewer sends in the thread.
    rrOnUserMessage :: Text -> IO (),
    -- | Called with the English body of each reply the bot posts (i.e. a revised draft).
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
  rr <- takeMVar (dcSendQueue cfg)
  putStrLn $ "[Discord] Dequeued review request: " <> T.unpack (rrTitle rr)
  result <- try (processReview hdl cfg rr) :: IO (Either SomeException ())
  case result of
    Left ex -> putStrLn $ "[Discord] drainQueue exception: " <> displayException ex
    Right () -> pure ()

-- | Create the forum thread, post the draft, and register the pending review.
processReview :: DiscordHandle -> DiscordConfig -> ReviewRequest -> IO ()
processReview hdl cfg ReviewRequest {..} = do
  threadResult <- runReaderT (restCall (StartThreadForumMedia chanId forumOpts)) hdl
  case threadResult of
    Left err -> ioError . userError $ "failed to create forum thread: " <> show err
    Right thread -> do
      let tid = channelId thread
          key = showId tid
          starterMsgId = DiscordId (unId tid) :: MessageId
      putStrLn $ "[Discord] Thread created (tid=" <> T.unpack key <> "), running on-created callback..."
      persistResult <- try (rrOnThreadCreated key) :: IO (Either SomeException ())
      case persistResult of
        Left ex ->
          putStrLn $ "[Discord] ERROR: on-thread-created callback failed; draft not persisted: " <> displayException ex
        Right () -> do
          putStrLn "[Discord] Draft persisted. Posting draft body to thread..."
          tryLog "[Discord] ERROR: failed to post draft body" $
            postAsFile hdl tid (emojiDraft <> " **Draft:**") rrBodyEn
          threadDelay 2_000_000
          bodyEnVar <- newTVarIO rrBodyEn
          tagsVar <- newTVarIO rrTags
          let pr =
                PendingReview
                  { prTitle = rrTitle,
                    prCurrentBodyEn = bodyEnVar,
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
          tryLog "[Discord] WARNING: failed to add approve reaction" $
            restCallIO hdl (CreateReaction (tid, starterMsgId) emojiApprove)
          tryLog "[Discord] WARNING: failed to add reject reaction" $
            restCallIO hdl (CreateReaction (tid, starterMsgId) emojiReject)
          putStrLn "[Discord] Review setup complete."
  where
    chanId = mkChannelId (dcChannelId cfg)
    embed =
      def
        { createEmbedTitle = rrTitle,
          createEmbedDescription = truncateText 500 rrBodyEn,
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

-- | Handle incoming Discord events.
--
-- * 'MessageReactionAdd': approve (✅) or reject (❌) by a non-bot user.
-- * 'MessageCreate': approval phrase triggers the approve callback;
--   anything else triggers an AI revision.
eventHandler :: DiscordConfig -> Event -> DiscordHandler ()
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
          withReregistration cfg key pr "reaction callback" $
            if emoji == emojiApprove
              then fireApproval pr emoji
              else fireRejection pr emoji
eventHandler cfg (MessageCreate m) = do
  cache <- readCache
  let botId = userId (cacheCurrentUser cache)
  when (not (userIsBot (messageAuthor m)) && userId (messageAuthor m) /= botId) $ do
    let key = showId (messageChannelId m)
        content = messageContent m
    reviewMap <- liftIO $ readTVarIO (dcReviewMap cfg)
    case Map.lookup key reviewMap of
      Nothing -> pure ()
      Just pr ->
        if isApprovalMessage content
          then liftIO $ do
            atomically $ deregister cfg key
            withReregistration cfg key pr "approval" $ fireApproval pr content
          else handleRevision pr content
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

-- | Call the AI revision function and post the revised English draft back
-- into the thread.  On failure, post an error message instead.
handleRevision :: PendingReview -> Text -> DiscordHandler ()
handleRevision pr feedback = do
  liftIO $
    tryLog "[Discord] WARNING: failed to record user message" $
      prOnUserMessage pr feedback
  reviseResult <- liftIO $ do
    currentBodyEn <- readTVarIO (prCurrentBodyEn pr)
    try (prRevise pr currentBodyEn feedback) :: IO (Either SomeException (Text, [Text]))
  case reviseResult of
    Left ex -> do
      let errMsg = "Could not revise draft: " <> T.pack (displayException ex)
      liftIO $ putStrLn $ "[Drafts] Gemini error: " <> T.unpack errMsg
      sendResult <- restCall $ CreateMessage (prThreadId pr) (emojiWarning <> " " <> errMsg)
      case sendResult of
        Left e -> liftIO $ putStrLn $ "[Discord] WARNING: also failed to send error message to thread: " <> show e
        Right _ -> pure ()
    Right (newBodyEn, newTags) -> do
      liftIO $ atomically $ do
        writeTVar (prCurrentBodyEn pr) newBodyEn
        writeTVar (prCurrentTags pr) newTags
      liftIO $
        tryLog "[Discord] WARNING: failed to record bot message" $
          prOnBotMessage pr newBodyEn
      hdl <- ask
      liftIO $
        tryLog "[Discord] ERROR: failed to post revised draft to thread" $
          postAsFile hdl (prThreadId pr) (emojiRevise <> " **Revised draft:**") newBodyEn

-- ---------------------------------------------------------------------------
-- Utilities
-- ---------------------------------------------------------------------------

-- | Re-register a review that already has a Discord forum thread (e.g. after
-- a bot restart).  Unlike 'registerForReview', this does NOT create a new
-- forum thread — it simply inserts a fresh 'PendingReview' into 'dcReviewMap'
-- so that subsequent reactions and messages are handled normally.
--
-- If @tidText@ cannot be parsed as a Discord snowflake the call is a no-op
-- and a warning is printed.
restoreReview :: DiscordConfig -> Text -> ReviewRequest -> IO ()
restoreReview cfg tidText ReviewRequest {..} =
  case readMaybe (T.unpack tidText) :: Maybe Integer of
    Nothing ->
      putStrLn $
        "[Discord] WARNING: could not restore review (unparseable thread ID): "
          <> T.unpack tidText
    Just n -> do
      let tid = DiscordId (Snowflake (fromIntegral n)) :: ChannelId
      bodyEnVar <- newTVarIO rrBodyEn
      tagsVar <- newTVarIO rrTags
      let pr =
            PendingReview
              { prTitle = rrTitle,
                prCurrentBodyEn = bodyEnVar,
                prCurrentTags = tagsVar,
                prKey = tidText,
                prThreadId = tid,
                prRevise = rrRevise,
                prApprove = rrApprove,
                prReject = rrReject,
                prOnUserMessage = rrOnUserMessage,
                prOnBotMessage = rrOnBotMessage
              }
      atomically $ modifyTVar' (dcReviewMap cfg) (Map.insert tidText pr)
      putStrLn $ "[Discord] Restored pending review: " <> T.unpack rrTitle

-- | Convert a raw Word ID to a Discord 'ChannelId'.
mkChannelId :: Word -> ChannelId
mkChannelId w = DiscordId (Snowflake (fromIntegral w))

-- | Render any Discord snowflake-based ID to text for use as a map key.
showId :: DiscordId a -> Text
showId i = T.pack $ show $ unSnowflake $ unId i

-- | Run an IO action, catching any exception and logging it with the given
-- prefix.  Always continues after logging; use this for non-fatal steps.
tryLog :: String -> IO () -> IO ()
tryLog prefix action = do
  result <- try action :: IO (Either SomeException ())
  case result of
    Left ex -> putStrLn $ prefix <> ": " <> displayException ex
    Right () -> pure ()

-- | Run an IO action that was preceded by deregistering a 'PendingReview'.
-- On failure the review is re-inserted into 'dcReviewMap' so it can be
-- retried, and the exception is logged.
withReregistration :: DiscordConfig -> Text -> PendingReview -> String -> IO () -> IO ()
withReregistration cfg key pr label action = do
  result <- try action :: IO (Either SomeException ())
  case result of
    Left ex -> do
      atomically $ modifyTVar' (dcReviewMap cfg) (Map.insert key pr)
      putStrLn $ "[Discord] " <> label <> " failed (review re-registered): " <> displayException ex
    Right () -> pure ()

-- | Read the current body and tags from a 'PendingReview' and fire the
-- approve callback.  Records the trigger text (emoji or message) as a user
-- comment first.
fireApproval :: PendingReview -> Text -> IO ()
fireApproval pr trigger = do
  prOnUserMessage pr trigger
  bodyEn <- readTVarIO (prCurrentBodyEn pr)
  tags <- readTVarIO (prCurrentTags pr)
  prApprove pr bodyEn tags

-- | Record the trigger text as a user comment and fire the reject callback.
fireRejection :: PendingReview -> Text -> IO ()
fireRejection pr trigger = do
  prOnUserMessage pr trigger
  prReject pr trigger

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

-- | Post a draft body to a Discord channel as a @draft.md@ file attachment.
-- This avoids Discord's 2000-character message limit entirely and keeps the
-- draft in a single, easily downloadable message.
postAsFile :: DiscordHandle -> ChannelId -> Text -> Text -> IO ()
postAsFile hdl tid caption body = do
  putStrLn $
    "[Discord] Posting draft as file attachment ("
      <> show (T.length body)
      <> " chars)."
  let opts =
        def
          { messageDetailedContent = caption,
            messageDetailedFile = Just ("draft.md", encodeUtf8 body)
          }
  result <- runReaderT (restCall (CreateMessageDetailed tid opts)) hdl
  case result of
    Left err -> putStrLn $ "[Discord] Failed to post file attachment: " <> show err
    Right _ -> putStrLn "[Discord] Draft posted as file attachment."

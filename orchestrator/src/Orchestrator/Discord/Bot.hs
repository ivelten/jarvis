{-# LANGUAGE RecordWildCards #-}

module Orchestrator.Discord.Bot
  ( DiscordConfig (..),
    PendingReview (..),
    ReviewRequest (..),
    mkDiscordConfig,
    registerForReview,
    isApprovalMessage,
    chunkText,
    startBot,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, runReaderT)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Discord
import Discord.Requests (ChannelRequest (..), StartThreadForumMediaMessage (..), StartThreadForumMediaOpts (..), StartThreadOpts (..))
import Discord.Types

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | State for a draft currently under review in a Discord forum thread.
data PendingReview = PendingReview
  { -- | Post title (used for context).
    prTitle :: !Text,
    -- | Current draft body; updated atomically after each AI revision.
    prCurrentBody :: !(TVar Text),
    -- | Lookup key: forum thread Snowflake as text.  Because in a forum
    -- channel the thread ID equals the starter message ID, one key serves
    -- both reaction events and thread-message events.
    prKey :: !Text,
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
            createEmbedDescription = mkEmbedPreview 500 rrBody,
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
      let pr =
            PendingReview
              { prTitle = rrTitle,
                prCurrentBody = bodyVar,
                prKey = key,
                prThreadId = tid,
                prRevise = rrRevise,
                prApprove = rrApprove,
                prReject = rrReject
              }
      atomically $ modifyTVar' (dcReviewMap cfg) (Map.insert key pr)
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
            then readTVarIO (prCurrentBody pr) >>= prApprove pr
            else prReject pr emoji
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
            readTVarIO (prCurrentBody pr) >>= prApprove pr
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
  result <- liftIO $ do
    currentBody <- readTVarIO (prCurrentBody pr)
    try (prRevise pr currentBody feedback) :: IO (Either SomeException Text)
  case result of
    Left ex ->
      void $
        restCall $
          CreateMessage
            (prThreadId pr)
            (emojiWarning <> " Could not revise draft: " <> T.pack (show ex))
    Right newBody -> do
      liftIO $ atomically $ writeTVar (prCurrentBody pr) newBody
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

-- | Truncate body text to at most @maxLen@ characters at a word boundary,
-- stripping trailing punctuation and appending '\x2026' (…) when truncated.
mkEmbedPreview :: Int -> Text -> Text
mkEmbedPreview maxLen body
  | T.length body <= maxLen = body
  | otherwise =
      let window = T.take maxLen body
          trimmed = case T.breakOnEnd " " window of
            (pre, _) | not (T.null pre) -> T.dropWhileEnd (== ' ') pre
            _ -> window
          clean = T.dropWhileEnd (`elem` (",;:—-" :: String)) trimmed
       in clean <> "\x2026"

-- | Split 'Text' into chunks of at most @maxLen@ characters.
-- Break priority (highest to lowest):
--   1. Last @\\n\\n@ paragraph boundary within the window
--   2. Last @\\n@ line boundary within the window
--   3. Last word boundary (space) within the window
--   4. Hard cut at @maxLen@ (only when no whitespace exists at all,
--      e.g. an extremely long URL or code token)
chunkText :: Int -> Text -> [Text]
chunkText maxLen = filter (not . T.null) . map T.strip . go
  where
    go t
      | T.length t <= maxLen = [t]
      | otherwise =
          let window = T.take maxLen t
              remaining = T.drop maxLen t
              (chunk, leftover) = bestSplit window
           in case currentFenceOpener chunk of
                Nothing ->
                  chunk : go (leftover <> remaining)
                Just opener ->
                  -- Split inside a code fence: close it here, reopen next chunk.
                  (chunk <> "\n```") : go (opener <> "\n" <> leftover <> remaining)

    bestSplit window =
      case breakLast "\n\n" window of
        Just p -> p
        Nothing -> case breakLast "\n" window of
          Just p -> p
          Nothing -> fromMaybe (window, "") (breakLastWord window)

    -- \| Returns the opening fence line (e.g. @\"```haskell\"@) when @t@ ends
    -- inside an open code fence, or 'Nothing' if we are outside a fence.
    -- Walks through lines treating any @```@-prefixed line as a toggle.
    currentFenceOpener :: Text -> Maybe Text
    currentFenceOpener = stepLines Nothing . T.lines
      where
        stepLines state [] = state
        stepLines Nothing (l : ls)
          | "```" `T.isPrefixOf` T.strip l = stepLines (Just (T.strip l)) ls
          | otherwise = stepLines Nothing ls
        stepLines (Just opener) (l : ls)
          | T.strip l == "```" = stepLines Nothing ls
          | otherwise = stepLines (Just opener) ls

    -- \| Split at the last occurrence of @sep@, returning
    -- @(part before and including sep, part after sep)@.
    breakLast sep t =
      let parts = T.splitOn sep t
       in if length parts <= 1
            then Nothing
            else Just (T.intercalate sep (init parts) <> sep, last parts)

    -- \| Split at the last space in @t@, returning
    -- @(part up to and including the space, part after the space)@.
    -- Returns 'Nothing' when there is no space in @t@.
    breakLastWord t =
      case T.breakOnEnd " " t of
        ("", _) -> Nothing
        (pre, suf) -> Just (pre, suf)

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

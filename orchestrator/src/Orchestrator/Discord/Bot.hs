module Orchestrator.Discord.Bot
  ( DiscordConfig (..),
    ReviewResult (..),
    SendRequest (..),
    mkDiscordConfig,
    sendForReview,
    awaitReview,
    startBot,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, runReaderT)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Discord
import Discord.Requests (ChannelRequest (..), MessageDetailedOpts (..))
import Discord.Types
import GHC.Generics (Generic)

-- | Internal record passed through the send queue.
data SendRequest = SendRequest
  { srTitle :: !Text,
    srBody :: !Text,
    -- | Filled with the Discord message ID once the post is sent.
    srReply :: !(MVar Text)
  }

-- | Runtime configuration for the Discord bot.
data DiscordConfig = DiscordConfig
  { -- | Discord bot token (from environment, without "Bot " prefix).
    dcBotToken :: !Text,
    -- | Personal server (guild) ID.
    dcGuildId :: !Word,
    -- | Review channel ID.
    dcChannelId :: !Word,
    -- | Internal queue: pipeline puts review requests here.
    dcSendQueue :: !(MVar SendRequest),
    -- | Map from message-ID text to the MVar waiting for a review result.
    dcReviewMap :: !(TVar (Map Text (MVar ReviewResult)))
  }

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

-- | The outcome of a human review performed through Discord.
data ReviewResult
  = -- | Reviewer reacted with ✅.
    Approved
  | -- | Reviewer reacted with ❌.
    Rejected !Text
  deriving (Show, Eq, Generic)

-- | Post a draft to the configured Discord review channel.
-- Blocks until the bot has sent the message, then returns its Discord message
-- ID (as text) for subsequent use in 'awaitReview'.
sendForReview ::
  DiscordConfig ->
  -- | Post title.
  Text ->
  -- | Post body (Markdown).
  Text ->
  IO Text
sendForReview cfg title body = do
  replyVar <- newEmptyMVar
  putMVar (dcSendQueue cfg) (SendRequest title body replyVar)
  takeMVar replyVar

-- | Block the calling thread until either ✅ or ❌ is reacted to the
-- message with the given Discord message ID.
awaitReview ::
  DiscordConfig ->
  -- | Discord message ID returned by 'sendForReview'.
  Text ->
  IO ReviewResult
awaitReview cfg msgIdText = do
  resultVar <- newEmptyMVar
  atomically $ modifyTVar' (dcReviewMap cfg) (Map.insert msgIdText resultVar)
  result <- takeMVar resultVar
  atomically $ modifyTVar' (dcReviewMap cfg) (Map.delete msgIdText)
  pure result

-- | Start the long-running Discord bot event loop.
-- This function blocks indefinitely; call it in a dedicated thread.
startBot :: DiscordConfig -> IO ()
startBot cfg = do
  err <-
    runDiscord $
      def
        { discordToken = "Bot " <> dcBotToken cfg,
          discordGatewayIntent =
            def
              { gatewayIntentMessageReactions = True
              },
          discordOnStart = botWorker cfg,
          discordOnEvent = eventHandler cfg,
          discordOnLog = \t -> putStrLn $ "[Discord] " <> T.unpack t
        }
  putStrLn $ "[Discord] bot stopped: " <> T.unpack err

-- | Launched from 'discordOnStart': forks a thread that drains the send
-- queue and sends embed messages on behalf of the pipeline.
botWorker :: DiscordConfig -> DiscordHandler ()
botWorker cfg = do
  hdl <- ask
  liftIO $ void $ forkIO $ drainQueue hdl cfg

-- | Continuously reads 'SendRequest' values from the queue, posts an embed,
-- adds ✅/❌ reaction affordances, and fills the reply MVar with the message ID.
drainQueue :: DiscordHandle -> DiscordConfig -> IO ()
drainQueue hdl cfg = forever $ do
  SendRequest {..} <- takeMVar (dcSendQueue cfg)
  let chanId = mkChannelId (dcChannelId cfg)
      preview = T.take 1_000 srBody
      embed =
        def
          { createEmbedTitle = srTitle,
            createEmbedDescription = preview,
            createEmbedFooterText = "React \x2705 to approve or \x274c to reject."
          }
      opts = def {messageDetailedEmbeds = Just [embed]}
  result <- runReaderT (restCall (CreateMessageDetailed chanId opts)) hdl
  case result of
    Left err -> do
      putStrLn $ "[Discord] failed to send review message: " <> show err
      putMVar srReply ""
    Right msg -> do
      let mid = showMsgId (messageId msg)
      void $
        runReaderT
          (restCall (CreateReaction (chanId, messageId msg) "\x2705"))
          hdl
      void $
        runReaderT
          (restCall (CreateReaction (chanId, messageId msg) "\x274c"))
          hdl
      putMVar srReply mid

-- | Handle incoming Discord events: on a reaction to a known review message,
-- fill the waiting 'ReviewResult' MVar.
eventHandler :: DiscordConfig -> Event -> DiscordHandler ()
eventHandler cfg (MessageReactionAdd ri) = do
  -- Filter out the bot's own reactions.
  cache <- readCache
  let botId = userId (cacheCurrentUser cache)
  when (reactionUserId ri /= botId) $ do
    let msgKey = showMsgId (reactionMessageId ri)
        emoji = emojiName (reactionEmoji ri)
    mVar <- liftIO $
      atomically $ do
        rm <- readTVar (dcReviewMap cfg)
        case Map.lookup msgKey rm of
          Nothing -> pure Nothing
          Just var -> pure (Just var)
    case mVar of
      Nothing -> pure ()
      Just var ->
        liftIO $
          putMVar var $
            if emoji == "\x2705"
              then Approved
              else Rejected emoji
eventHandler _ _ = pure ()

-- | Convert a raw Word ID to a Discord 'ChannelId'.
mkChannelId :: Word -> ChannelId
mkChannelId w = DiscordId (Snowflake (fromIntegral w))

-- | Convert a 'MessageId' to text for use as a key in the review map.
showMsgId :: MessageId -> Text
showMsgId mid = T.pack $ show $ unSnowflake $ unId mid

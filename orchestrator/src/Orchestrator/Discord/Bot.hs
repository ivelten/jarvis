module Orchestrator.Discord.Bot
  ( DiscordConfig (..),
    ReviewResult (..),
    sendForReview,
    awaitReview,
    startBot,
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | Runtime configuration for the Discord bot.
data DiscordConfig = DiscordConfig
  { -- | Discord bot token (from environment).
    dcBotToken :: !Text,
    -- | Personal server ID.
    dcGuildId :: !Word,
    -- | Review forum channel ID.
    dcChannelId :: !Word
  }

-- | The outcome of a human review performed through Discord.
data ReviewResult
  = -- | Reviewer reacted with ✅.
    Approved
  | -- | Reviewer reacted with ❌ and optionally left a comment.
    Rejected !Text
  deriving (Show, Eq, Generic)

-- | Post a draft to the configured Discord review channel.
-- Returns the Discord message ID for later polling.
sendForReview ::
  DiscordConfig ->
  -- | Post title.
  Text ->
  -- | Post body (Markdown).
  Text ->
  -- | Discord message ID of the created thread.
  IO Text
sendForReview _cfg _title _body = do
  -- TODO: use discord-haskell to send an embed with the post content.
  pure ""

-- | Block until the reviewer reacts to a previously sent message.
awaitReview ::
  DiscordConfig ->
  -- | Discord message ID.
  Text ->
  IO ReviewResult
awaitReview _cfg _msgId = do
  -- TODO: poll / listen for reaction events on the message.
  pure Approved

-- | Start the long-running Discord bot event loop.
startBot :: DiscordConfig -> IO ()
startBot _cfg = do
  -- TODO: wire up discord-haskell's runDiscord and handle events.
  pure ()

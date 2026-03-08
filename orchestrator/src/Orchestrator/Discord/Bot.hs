{-# LANGUAGE RecordWildCards #-}

module Orchestrator.Discord.Bot
  ( DiscordConfig (..),
    DiscordBotSettings (..),
    RevisionResult (..),
    ReviewRequest (..),
    ApproveReviewEvent (..),
    RejectReviewEvent (..),
    ReviseReviewEvent (..),
    SubjectCommandEvent (..),
    DisableSubjectCommandEvent (..),
    mkDiscordConfig,
    registerForReview,
    sendInteractionMessage,
    sendInteractionFile,
    sendThreadMessage,
    isApprovalMessage,
    buildContextMessage,
    startBot,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, runReaderT)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Discord
import Discord.Interactions
import Discord.Requests (ApplicationCommandRequest (..), ChannelRequest (..), InteractionResponseRequest (..), MessageDetailedOpts (..), StartThreadForumMediaMessage (..), StartThreadForumMediaOpts (..), StartThreadOpts (..))
import Discord.Types
import Orchestrator.TextUtils (emojiApprove, emojiDraft, emojiReject, emojiRevise, emojiWarning)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Result returned by 'dcOnReviseRequest' so the bot knows how to respond.
data RevisionResult
  = -- | No 'DraftReviewing' draft found for this thread; ignore silently.
    ReviewNotActive
  | -- | The AI or pipeline failed; the message is shown to the reviewer.
    RevisionError !Text
  | -- | Successful revision; bot posts both updated bodies to the thread.
    RevisionOk !Text !Text
  deriving (Show, Eq)

-- | All the information needed to submit a draft for Discord review.
data ReviewRequest = ReviewRequest
  { -- | Post title shown as the forum thread name.
    rrTitle :: !Text,
    -- | Full English Markdown draft body.
    rrBodyEn :: !Text,
    -- | Full Brazilian Portuguese Markdown draft body.
    rrBodyPtBr :: !Text,
    -- | Initial tags from the draft.
    rrTags :: ![Text],
    -- | Title of the source content the draft was based on.
    rrSourceTitle :: !Text,
    -- | Summary of the source content.
    rrSourceSummary :: !Text,
    -- | URL of the source content.
    rrSourceUrl :: !Text,
    -- | Subjects associated with the source content.
    rrSubjects :: ![Text],
    -- | Called with the Discord thread ID (as 'Text') once the review thread
    -- has been created.  Used for persisting the thread ID back to the DB.
    rrOnThreadCreated :: Text -> IO ()
  }

-- | Payload for the @\/subject@ slash command callback.
newtype SubjectCommandEvent = SubjectCommandEvent
  { -- | The subject name provided by the user.
    sceSubjectName :: Text
  }

-- | Payload for the @\/disable-subject@ slash command callback.
newtype DisableSubjectCommandEvent = DisableSubjectCommandEvent
  { -- | The integer ID of the subject to disable.
    dsceSubjectId :: Int64
  }

-- | Payload for the approve-review callback.
data ApproveReviewEvent = ApproveReviewEvent
  { -- | Discord thread ID where the reaction or message arrived.
    aprThreadId :: !Text,
    -- | The emoji or approval phrase that triggered the event.
    aprTriggerText :: !Text
  }

-- | Payload for the reject-review callback.
data RejectReviewEvent = RejectReviewEvent
  { -- | Discord thread ID where the reaction arrived.
    rejThreadId :: !Text,
    -- | The emoji that triggered the rejection.
    rejReason :: !Text
  }

-- | Payload for the revise-review callback.
data ReviseReviewEvent = ReviseReviewEvent
  { -- | Discord thread ID where the message was posted.
    rvsThreadId :: !Text,
    -- | Reviewer feedback message content.
    rvsFeedback :: !Text
  }

-- | Runtime configuration for the Discord bot.
data DiscordConfig = DiscordConfig
  { -- | Discord bot token (from environment, without the @Bot @ prefix).
    dcBotToken :: !Text,
    -- | Personal server (guild) ID.
    dcGuildId :: !Word,
    -- | Forum channel ID where review threads are created.
    dcChannelId :: !Word,
    -- | Text channel ID used for slash commands and bot notices.
    dcInteractionChannelId :: !Word,
    -- | Internal queue: pipeline puts review requests here.
    dcSendQueue :: !(MVar ReviewRequest),
    -- | Live 'DiscordHandle', filled once the bot connects.  Used by
    -- 'sendInteractionMessage' to post notices from outside the event loop.
    dcHandle :: !(MVar DiscordHandle),
    -- | IO action invoked when the @\/discover@ slash command is used.
    dcOnDiscoverCommand :: IO (),
    -- | IO action invoked when the @\/draft@ slash command is used.
    dcOnDraftCommand :: IO (),
    -- | IO action invoked when the @\/subject@ slash command is used.
    dcOnSubjectCommand :: !(SubjectCommandEvent -> IO ()),
    -- | IO action invoked when the @\/disable-subject@ slash command is used.
    dcOnDisableSubjectCommand :: !(DisableSubjectCommandEvent -> IO ()),
    -- | IO action invoked when the @\/list-subjects@ slash command is used.
    dcOnListSubjectsCommand :: IO (),
    -- | Called when an approve reaction or approval message arrives.
    dcOnApproveReview :: !(ApproveReviewEvent -> IO ()),
    -- | Called when a reject reaction arrives.
    dcOnRejectReview :: !(RejectReviewEvent -> IO ()),
    -- | Called when a non-approval user message arrives in a review thread.
    -- Returns 'ReviewNotActive' if no active review was found (bot ignores),
    -- 'RevisionError' on failure (bot posts an error), or 'RevisionOk' with
    -- the updated bodies (bot posts them to the thread).
    dcOnReviseRequest :: !(ReviseReviewEvent -> IO RevisionResult)
  }

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Static settings supplied once at startup to 'mkDiscordConfig'.
data DiscordBotSettings = DiscordBotSettings
  { -- | Discord bot token (without the @Bot @ prefix).
    dbsBotToken :: !Text,
    -- | Personal server (guild) ID.
    dbsGuildId :: !Word,
    -- | Forum channel ID where review threads are created.
    dbsChannelId :: !Word,
    -- | Text channel ID used for slash commands and bot notices.
    dbsInteractionChannelId :: !Word,
    -- | IO action invoked when the @\/discover@ slash command is used.
    dbsOnDiscoverCommand :: IO (),
    -- | IO action invoked when the @\/draft@ slash command is used.
    dbsOnDraftCommand :: IO (),
    -- | IO action invoked when the @\/subject@ slash command is used.
    dbsOnSubjectCommand :: !(SubjectCommandEvent -> IO ()),
    -- | IO action invoked when the @\/disable-subject@ slash command is used.
    dbsOnDisableSubjectCommand :: !(DisableSubjectCommandEvent -> IO ()),
    -- | IO action invoked when the @\/list-subjects@ slash command is used.
    dbsOnListSubjectsCommand :: IO (),
    -- | Called when a draft is approved (reaction or approval phrase).
    dbsOnApproveReview :: ApproveReviewEvent -> IO (),
    -- | Called when a draft is rejected (reaction).
    dbsOnRejectReview :: RejectReviewEvent -> IO (),
    -- | Called when a revision is requested (non-approval thread message).
    dbsOnReviseRequest :: ReviseReviewEvent -> IO RevisionResult
  }

-- | Smart constructor — allocates the internal communication channels.
mkDiscordConfig :: DiscordBotSettings -> IO DiscordConfig
mkDiscordConfig DiscordBotSettings {..} = do
  sendQueue <- newEmptyMVar
  handleVar <- newEmptyMVar
  pure
    DiscordConfig
      { dcBotToken = dbsBotToken,
        dcGuildId = dbsGuildId,
        dcChannelId = dbsChannelId,
        dcInteractionChannelId = dbsInteractionChannelId,
        dcSendQueue = sendQueue,
        dcHandle = handleVar,
        dcOnDiscoverCommand = dbsOnDiscoverCommand,
        dcOnDraftCommand = dbsOnDraftCommand,
        dcOnSubjectCommand = dbsOnSubjectCommand,
        dcOnDisableSubjectCommand = dbsOnDisableSubjectCommand,
        dcOnListSubjectsCommand = dbsOnListSubjectsCommand,
        dcOnApproveReview = dbsOnApproveReview,
        dcOnRejectReview = dbsOnRejectReview,
        dcOnReviseRequest = dbsOnReviseRequest
      }

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Submit a draft to the Discord review channel and return immediately.
--
-- The bot posts an embed in the review channel, spawns a thread from that
-- message, and posts the full draft as the first thread message.  Subsequent
-- reviewer messages in the thread trigger AI revisions via 'dcOnReviseRequest';
-- reacting ✅ or typing an approval phrase calls 'dcOnApproveReview'; reacting
-- ❌ calls 'dcOnRejectReview'.  All three handlers perform their own DB lookups.
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

-- | Launched from 'discordOnStart': stores the live handle for out-of-band
-- REST calls and forks a thread that drains the send queue.
botWorker :: DiscordConfig -> DiscordHandler ()
botWorker cfg = do
  hdl <- ask
  liftIO $ putMVar (dcHandle cfg) hdl
  liftIO $ void $ forkIO $ drainQueue hdl cfg

-- | Continuously reads 'ReviewRequest' values from the queue, creates a forum
-- thread with a plain-text starter message, and posts a context reply followed
-- by the draft files.  Event callbacks are dispatched via 'DiscordConfig' handlers
-- that perform all DB lookups on demand.
drainQueue :: DiscordHandle -> DiscordConfig -> IO ()
drainQueue hdl cfg = forever $ do
  rr <- takeMVar (dcSendQueue cfg)
  putStrLn $ "[Discord] Dequeued review request: " <> T.unpack (rrTitle rr)
  result <- try (processReview hdl cfg rr) :: IO (Either SomeException ())
  case result of
    Left ex -> putStrLn $ "[Discord] drainQueue exception: " <> displayException ex
    Right () -> pure ()

-- | Orchestrate the full review flow: create thread, persist, then activate.
processReview :: DiscordHandle -> DiscordConfig -> ReviewRequest -> IO ()
processReview hdl cfg rr@ReviewRequest {..} = do
  thread <- createReviewThread hdl cfg rr
  let tid = channelId thread
      key = showId tid
      starterMsgId = DiscordId (unId tid) :: MessageId
  putStrLn $ "[Discord] Thread created (tid=" <> T.unpack key <> "), running on-created callback..."
  persistResult <- try (rrOnThreadCreated key) :: IO (Either SomeException ())
  case persistResult of
    Left ex ->
      putStrLn $ "[Discord] ERROR: on-thread-created callback failed; draft not persisted: " <> displayException ex
    Right () ->
      activateReview hdl cfg rr tid key starterMsgId

-- | Create the forum thread. Throws 'IOError' if the Discord API call fails.
createReviewThread :: DiscordHandle -> DiscordConfig -> ReviewRequest -> IO Channel
createReviewThread hdl cfg ReviewRequest {..} =
  runReaderT (restCall (StartThreadForumMedia chanId forumOpts)) hdl
    >>= either (ioError . userError . ("failed to create forum thread: " <>) . show) pure
  where
    chanId = mkChannelId (dcChannelId cfg)
    forumMsg =
      StartThreadForumMediaMessage
        { startThreadForumMediaMessageContent =
            emojiDraft <> " **" <> rrTitle <> "**\n\nReact " <> emojiApprove <> " to approve, " <> emojiReject <> " to reject, or type feedback in this thread.",
          startThreadForumMediaMessageEmbeds = Nothing,
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

-- | Post draft files and add emoji reactions to the starter message.
activateReview :: DiscordHandle -> DiscordConfig -> ReviewRequest -> ChannelId -> Text -> MessageId -> IO ()
activateReview hdl _cfg rr tid _key starterMsgId = do
  putStrLn "[Discord] Draft persisted. Posting draft bodies to thread..."
  tryLog "[Discord] ERROR: failed to post context info" $
    restCallIO hdl (CreateMessage tid (buildContextMessage rr))
  tryLog "[Discord] ERROR: failed to post EN draft" $
    postAsFile hdl tid (emojiDraft <> " **Draft (EN):**") "draft-en.md" (rrBodyEn rr)
  tryLog "[Discord] ERROR: failed to post PT-BR draft" $
    postAsFile hdl tid (emojiDraft <> " **Draft (PT-BR):**") "draft-pt-br.md" (rrBodyPtBr rr)
  threadDelay 2_000_000
  tryLog "[Discord] WARNING: failed to add approve reaction" $
    restCallIO hdl (CreateReaction (tid, starterMsgId) emojiApprove)
  tryLog "[Discord] WARNING: failed to add reject reaction" $
    restCallIO hdl (CreateReaction (tid, starterMsgId) emojiReject)
  putStrLn "[Discord] Review setup complete."

-- | Handle incoming Discord events.
--
-- * 'MessageReactionAdd': approve (✅) or reject (❌) by a non-bot user;
--   dispatches to 'dcOnApproveReview' or 'dcOnRejectReview' which look up
--   the active draft from the database.
-- * 'MessageCreate': approval phrase triggers 'dcOnApproveReview'; anything
--   else triggers 'dcOnReviseRequest' and the bot posts the revised bodies.
eventHandler :: DiscordConfig -> Event -> DiscordHandler ()
eventHandler cfg (MessageReactionAdd ri) = do
  cache <- readCache
  let botId = userId (cacheCurrentUser cache)
  when (reactionUserId ri /= botId) $
    liftIO $
      handleReactionEvent cfg ri
eventHandler cfg (MessageCreate m) = do
  cache <- readCache
  let botId = userId (cacheCurrentUser cache)
  when (not (userIsBot (messageAuthor m)) && userId (messageAuthor m) /= botId) $ do
    hdl <- ask
    liftIO $ handleMessageEvent cfg hdl m
eventHandler cfg (Ready _ _ _ _ _ _ (PartialApplication appId _)) =
  registerSlashCommands appId cfg
eventHandler cfg (InteractionCreate intr) =
  handleInteraction cfg intr
eventHandler _ _ = pure ()

-- | Dispatch a reaction event to the approve or reject callback.
handleReactionEvent :: DiscordConfig -> ReactionInfo -> IO ()
handleReactionEvent cfg ri = do
  let threadId = showId (reactionChannelId ri)
      emoji = emojiName (reactionEmoji ri)
  if emoji == emojiApprove
    then
      tryLog "[Discord] approve reaction error" $
        dcOnApproveReview cfg ApproveReviewEvent {aprThreadId = threadId, aprTriggerText = emoji}
    else
      tryLog "[Discord] reject reaction error" $
        dcOnRejectReview cfg RejectReviewEvent {rejThreadId = threadId, rejReason = emoji}

-- | Dispatch a non-bot message to the approve or revise callback, and post
-- the revised draft bodies back to the thread on a successful revision.
handleMessageEvent :: DiscordConfig -> DiscordHandle -> Message -> IO ()
handleMessageEvent cfg hdl m = do
  let threadId = showId (messageChannelId m)
      content = messageContent m
  if isApprovalMessage content
    then
      tryLog "[Discord] approve message error" $
        dcOnApproveReview cfg ApproveReviewEvent {aprThreadId = threadId, aprTriggerText = content}
    else do
      result <- dcOnReviseRequest cfg ReviseReviewEvent {rvsThreadId = threadId, rvsFeedback = content}
      handleRevisionResult hdl (messageChannelId m) result

-- | Post the outcome of a revision request back to the thread.
handleRevisionResult :: DiscordHandle -> ChannelId -> RevisionResult -> IO ()
handleRevisionResult _ _ ReviewNotActive = pure ()
handleRevisionResult hdl chanId (RevisionError errMsg) =
  restCallIO hdl (CreateMessage chanId (emojiWarning <> " " <> errMsg))
handleRevisionResult hdl chanId (RevisionOk newBodyEn newBodyPtBr) = do
  tryLog "[Discord] ERROR: failed to post revised EN draft" $
    postAsFile hdl chanId (emojiRevise <> " **Revised draft (EN):**") "draft-en.md" newBodyEn
  tryLog "[Discord] ERROR: failed to post revised PT-BR draft" $
    postAsFile hdl chanId (emojiRevise <> " **Revised draft (PT-BR):**") "draft-pt-br.md" newBodyPtBr

-- | Register the bot's guild slash commands.  Called once from the 'Ready'
-- event so commands are available immediately without waiting for global
-- propagation.
registerSlashCommands :: ApplicationId -> DiscordConfig -> DiscordHandler ()
registerSlashCommands appId cfg = do
  let gid = mkGuildId (dcGuildId cfg)
      cmds =
        [ ("discover", "Manually trigger content discovery"),
          ("draft", "Manually trigger post draft generation"),
          ("list-subjects", "List all enabled subjects")
        ]
  mapM_ (registerSimpleCommand appId gid) cmds
  registerSubjectCommand appId gid
  registerDisableSubjectCommand appId gid
  liftIO $ putStrLn "[Discord] Slash commands registered."

-- | Register a simple slash command with no options.
registerSimpleCommand :: ApplicationId -> GuildId -> (Text, Text) -> DiscordHandler ()
registerSimpleCommand appId gid (name, desc) =
  case createChatInput name desc of
    Nothing ->
      liftIO $ putStrLn $ "[Discord] WARNING: could not build slash command: " <> T.unpack name
    Just cmd ->
      void $ restCall (CreateGuildApplicationCommand appId gid cmd)

-- | Register the @\/subject@ slash command with a required string @name@ option.
registerSubjectCommand :: ApplicationId -> GuildId -> DiscordHandler ()
registerSubjectCommand appId gid =
  case createChatInput "subject" "Add a new subject of interest for the blog" of
    Nothing ->
      liftIO $ putStrLn "[Discord] WARNING: could not build subject slash command"
    Just cmd -> do
      let cmdWithOptions =
            cmd
              { createOptions =
                  Just
                    ( OptionsValues
                        [ OptionValueString
                            { optionValueName = "name",
                              optionValueLocalizedName = Nothing,
                              optionValueDescription = "The subject name to add (e.g. \"Haskell Concurrency\")",
                              optionValueLocalizedDescription = Nothing,
                              optionValueRequired = True,
                              optionValueStringChoices = Left False,
                              optionValueStringMinLen = Nothing,
                              optionValueStringMaxLen = Nothing
                            }
                        ]
                    )
              }
      void $ restCall (CreateGuildApplicationCommand appId gid cmdWithOptions)

-- | Register the @\/disable-subject@ slash command with a required integer @id@ option.
registerDisableSubjectCommand :: ApplicationId -> GuildId -> DiscordHandler ()
registerDisableSubjectCommand appId gid =
  case createChatInput "disable-subject" "Disable a subject by ID" of
    Nothing ->
      liftIO $ putStrLn "[Discord] WARNING: could not build disable-subject slash command"
    Just cmd -> do
      let cmdWithOptions =
            cmd
              { createOptions =
                  Just
                    ( OptionsValues
                        [ OptionValueInteger
                            { optionValueName = "id",
                              optionValueLocalizedName = Nothing,
                              optionValueDescription = "The ID of the subject to disable",
                              optionValueLocalizedDescription = Nothing,
                              optionValueRequired = True,
                              optionValueIntegerChoices = Left False,
                              optionValueIntegerMinVal = Nothing,
                              optionValueIntegerMaxVal = Nothing
                            }
                        ]
                    )
              }
      void $ restCall (CreateGuildApplicationCommand appId gid cmdWithOptions)

-- | Route an incoming 'Interaction' to the appropriate slash-command handler.
-- Commands are only processed when they originate from the configured
-- interaction channel; all other interactions are silently ignored.
handleInteraction :: DiscordConfig -> Interaction -> DiscordHandler ()
handleInteraction cfg intr =
  case intr of
    InteractionApplicationCommand
      { applicationCommandData = ApplicationCommandDataChatInput {applicationCommandDataName = cmdName},
        interactionChannelId = mChanId
      }
        | mChanId == Just (mkChannelId (dcInteractionChannelId cfg)) ->
            dispatchSlashCommand cfg intr cmdName
    _ -> pure ()

-- | Acknowledge and dispatch a recognised slash command.
-- The acknowledgement is sent immediately; the action runs in a forked thread.
dispatchSlashCommand :: DiscordConfig -> Interaction -> Text -> DiscordHandler ()
dispatchSlashCommand cfg intr cmdName = do
  let (reply, logPrefix, action) = case cmdName of
        "discover" ->
          ( "Content discovery started! I'll post new topics once I find them.",
            "[Discovery]",
            dcOnDiscoverCommand cfg
          )
        "draft" ->
          ( "Draft generation started! I'll create review threads when the drafts are ready.",
            "[Drafts]",
            dcOnDraftCommand cfg
          )
        "subject" ->
          case extractStringOption "name" intr of
            Nothing ->
              ("Please provide a subject name.", "[Subject]", pure ())
            Just subjectName ->
              ( "Adding subject \"" <> subjectName <> "\"...",
                "[Subject]",
                dcOnSubjectCommand cfg SubjectCommandEvent {sceSubjectName = subjectName}
              )
        "disable-subject" ->
          case extractIntegerOption "id" intr of
            Nothing ->
              ("Please provide a subject ID.", "[Subject]", pure ())
            Just sid ->
              ( "Disabling subject " <> T.pack (show sid) <> "...",
                "[Subject]",
                dcOnDisableSubjectCommand cfg DisableSubjectCommandEvent {dsceSubjectId = sid}
              )
        "list-subjects" ->
          ( "Fetching enabled subjects...",
            "[Subject]",
            dcOnListSubjectsCommand cfg
          )
        _ ->
          ("Unknown command.", "[Discord]", pure ())
  void $
    restCall $
      CreateInteractionResponse
        (interactionId intr)
        (interactionToken intr)
        (interactionResponseBasic reply)
  liftIO $ void $ forkIO $ tryLog (logPrefix <> " slash command error") action

-- | Extract a named string option value from an interaction's option data.
extractStringOption :: Text -> Interaction -> Maybe Text
extractStringOption optName InteractionApplicationCommand {applicationCommandData = ApplicationCommandDataChatInput {optionsData = Just (OptionsDataValues opts)}} =
  listToMaybe [s | OptionDataValueString {optionDataValueName = n, optionDataValueString = Right s} <- opts, n == optName]
extractStringOption _ _ = Nothing

-- | Extract a named integer option value from an interaction's option data.
extractIntegerOption :: Text -> Interaction -> Maybe Int64
extractIntegerOption optName InteractionApplicationCommand {applicationCommandData = ApplicationCommandDataChatInput {optionsData = Just (OptionsDataValues opts)}} =
  listToMaybe [fromIntegral n | OptionDataValueInteger {optionDataValueName = nm, optionDataValueInteger = Right n} <- opts, nm == optName]
extractIntegerOption _ _ = Nothing

-- | Post a plain-text notice to the interaction channel from any IO context.
-- Blocks until the bot is connected (i.e. 'dcHandle' is filled).
-- Exceptions are logged but never re-thrown.
sendInteractionMessage :: DiscordConfig -> Text -> IO ()
sendInteractionMessage cfg msg = do
  hdl <- readMVar (dcHandle cfg)
  tryLog "[Discord] WARNING: failed to send interaction message" $
    restCallIO hdl (CreateMessage (mkChannelId (dcInteractionChannelId cfg)) msg)

-- | Post a file attachment to the interaction channel from any IO context.
-- Blocks until the bot is connected (i.e. 'dcHandle' is filled).
-- Exceptions are logged but never re-thrown.
sendInteractionFile :: DiscordConfig -> Text -> Text -> Text -> IO ()
sendInteractionFile cfg filename caption content = do
  hdl <- readMVar (dcHandle cfg)
  tryLog "[Discord] WARNING: failed to send interaction file" $
    postAsFile hdl (mkChannelId (dcInteractionChannelId cfg)) caption filename content

-- ---------------------------------------------------------------------------
-- Utilities
-- ---------------------------------------------------------------------------

-- | Convert a raw Word ID to a Discord 'ChannelId'.
mkChannelId :: Word -> ChannelId
mkChannelId w = DiscordId (Snowflake (fromIntegral w))

-- | Convert a raw Word ID to a Discord 'GuildId'.
mkGuildId :: Word -> GuildId
mkGuildId w = DiscordId (Snowflake (fromIntegral w))

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

-- | Fire a Discord REST call via a raw handle, discarding the result.
restCallIO :: (FromJSON a) => DiscordHandle -> ChannelRequest a -> IO ()
restCallIO hdl req = void $ runReaderT (restCall req) hdl

-- | Post a draft body to a Discord channel as a named file attachment.
-- This avoids Discord's 2000-character message limit entirely and keeps the
-- draft in a single, easily downloadable message.
postAsFile :: DiscordHandle -> ChannelId -> Text -> Text -> Text -> IO ()
postAsFile hdl tid caption filename body = do
  putStrLn $
    "[Discord] Posting "
      <> T.unpack filename
      <> " as file attachment ("
      <> show (T.length body)
      <> " chars)."
  let opts =
        def
          { messageDetailedContent = caption,
            messageDetailedFile = Just (filename, encodeUtf8 body)
          }
  result <- runReaderT (restCall (CreateMessageDetailed tid opts)) hdl
  case result of
    Left err -> putStrLn $ "[Discord] Failed to post file attachment: " <> show err
    Right _ -> putStrLn $ "[Discord] Posted " <> T.unpack filename <> "."

-- | Format a 'ReviewRequest' as a context summary for the review thread.
-- Shows the source content info, tags, and subjects.
buildContextMessage :: ReviewRequest -> Text
buildContextMessage ReviewRequest {..} =
  T.unlines $
    [ "**Source:** " <> rrSourceTitle,
      "> " <> rrSourceSummary,
      "> <" <> rrSourceUrl <> ">"
    ]
      ++ ["**Tags:** " <> T.intercalate ", " (map (\t -> "`" <> t <> "`") rrTags) | not (null rrTags)]
      ++ ["**Subjects:** " <> T.intercalate ", " rrSubjects | not (null rrSubjects)]

-- | Post a plain-text message to a Discord thread from any IO context.
-- Blocks until the bot is connected (i.e. 'dcHandle' is filled).
-- Exceptions are logged but never re-thrown.
sendThreadMessage :: DiscordConfig -> Text -> Text -> IO ()
sendThreadMessage cfg threadId msg = do
  hdl <- readMVar (dcHandle cfg)
  tryLog "[Discord] WARNING: failed to send thread message" $
    restCallIO hdl (CreateMessage (parseThreadId threadId) msg)

-- | Parse a Discord thread ID (as 'Text') into a 'ChannelId'.
parseThreadId :: Text -> ChannelId
parseThreadId = mkChannelId . read . T.unpack

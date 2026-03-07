{-# LANGUAGE RecordWildCards #-}

{- HLINT ignore "Use lambda-case" -}

-- | Orchestration pipeline: discovery, draft generation, Discord review,
-- and approval / rejection callbacks.
--
-- 'Main' wires configuration into a 'PipelineEnv' and then drives the two
-- top-level entry points ('runDiscovery', 'runDraftGeneration') on their
-- own scheduled loops.  Everything else here is an implementation detail.
module Orchestrator.Pipeline
  ( PipelineEnv (..),
    runDiscovery,
    runDraftGeneration,
    reloadPendingReviews,
    persistDraftAnalysis,
  )
where

import Control.Exception (SomeException, displayException, try)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist (Entity (..), entityKey, entityVal, insert, insert_, selectList, update, (=.), (==.))
import Database.Persist.Sql (SqlPersistT)
import Orchestrator.AI.Client
  ( AiConfig,
    DiscoveredContent (..),
    GeneratedDraft (..),
    generateDraft,
    reviseDraft,
  )
import Orchestrator.Database.Connection (DbPool, runDb)
import Orchestrator.Database.Entities
import Orchestrator.Database.Models
  ( CommentAuthor (..),
    ContentStatus (..),
    DraftStatus (..),
    TagList (..),
  )
import Orchestrator.Discord.Bot (DiscordConfig, ReviewRequest (..), registerForReview, restoreReview, sendInteractionMessage)
import Orchestrator.GitHub.Client (GitHubConfig, commitPost, triggerDeploy)
import Orchestrator.Posts.Generator (renderHugoPost)
import Orchestrator.TextUtils (emojiQueue, emojiSearch, splitTitle, toSlug, truncateText)
import Orchestrator.Topics.Selector (ingestDiscoveredContent, pendingContent)

-- ---------------------------------------------------------------------------
-- Environment
-- ---------------------------------------------------------------------------

-- | All runtime handles needed by the pipeline.  Constructed once in 'Main'
-- and threaded through every pipeline step.
data PipelineEnv = PipelineEnv
  { pipeAiCfg :: !AiConfig,
    pipeGhCfg :: !GitHubConfig,
    pipeDcCfg :: !DiscordConfig,
    pipeDbPool :: !DbPool
  }

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Ask Gemini to discover new content and persist it to the database.
-- On success, posts a notice to the interaction channel.
runDiscovery :: PipelineEnv -> IO ()
runDiscovery PipelineEnv {..} = do
  putStrLn "[Discovery] Discovering content via Gemini..."
  runDb pipeDbPool (ingestDiscoveredContent pipeAiCfg)
  putStrLn "[Discovery] Done."
  sendInteractionMessage pipeDcCfg (emojiSearch <> " **Content discovery complete.** New topics have been added to the queue.")

-- | Pick the next pending content item, generate a Markdown draft, and
-- register it for Discord review.  The approve/reject callbacks fire as
-- soon as the reviewer reacts or types an approval in the Discord thread.
--
-- Also reloads any drafts that were already sent to Discord but not yet
-- responded to (e.g. after a bot restart), so every cycle self-heals the
-- in-memory review map without a separate startup step.
runDraftGeneration :: PipelineEnv -> IO ()
runDraftGeneration env@PipelineEnv {..} = do
  reloadPendingReviews env
  pending <- runDb pipeDbPool pendingContent
  case pending of
    [] -> putStrLn "[Drafts] No pending content to process. Skipping."
    (item : _) -> do
      processDraft env (entityKey item) (entityVal item)
      sendInteractionMessage pipeDcCfg (emojiQueue <> " **Draft ready for review.** Check the forum channel for a new thread.")

-- | Rehydrate 'dcReviewMap' from the database after a bot restart.
--
-- Queries for all 'PostDraft' rows with status 'DraftReviewing' that already
-- have a Discord thread ID, and re-registers each one in 'dcReviewMap' via
-- 'restoreReview'.  This lets the bot resume reacting to ✅\/❌ reactions and
-- review-thread messages without creating duplicate Discord threads.
reloadPendingReviews :: PipelineEnv -> IO ()
reloadPendingReviews env@PipelineEnv {..} = do
  rows <- runDb pipeDbPool $ selectList [PostDraftStatus ==. DraftReviewing] []
  let withThread = [(k, v, tid) | Entity k v <- rows, Just tid <- [postDraftDiscordThreadId v]]
  if null withThread
    then putStrLn "[Jarvis] No in-progress reviews to restore."
    else mapM_ restore withThread
  where
    restore (pdKey, pd, tidText) = do
      srcRows <- runDb pipeDbPool $ selectList [PostDraftSourcePostDraftId ==. pdKey] []
      case listToMaybe srcRows of
        Nothing ->
          putStrLn $
            "[Jarvis] Skipping restore for draft '"
              <> T.unpack (postDraftTitle pd)
              <> "': no source content found."
        Just (Entity _ src) -> do
          let rcKey = postDraftSourceRawContentId src
              bodyEn = fromMaybe "" (postDraftContentMarkdownEn pd)
              bodyPtBr = fromMaybe "" (postDraftContentMarkdownPtBr pd)
              tags = unTagList (postDraftSuggestedTags pd)
              createdAt = postDraftCreatedAt pd
          let rr =
                ReviewRequest
                  { rrTitle = postDraftTitle pd,
                    rrBodyEn = bodyEn,
                    rrBodyPtBr = bodyPtBr,
                    rrTags = tags,
                    rrRevise = \currentBodyEn currentBodyPtBr feedback -> do
                      revisedDraft <- reviseDraft pipeAiCfg currentBodyEn currentBodyPtBr feedback
                      recordRevision env pdKey revisedDraft
                      pure (gdBodyEn revisedDraft, gdBodyPtBr revisedDraft, gdTags revisedDraft),
                    rrApprove = publishDraft env rcKey pdKey createdAt,
                    rrReject = rejectDraft env rcKey pdKey,
                    rrOnUserMessage = insertComment env pdKey CommentAuthorUser,
                    rrOnBotMessage = insertComment env pdKey CommentAuthorJarvis . truncateText 500,
                    rrOnThreadCreated = \_ -> pure ()
                  }
          restoreReview pipeDcCfg tidText rr

-- ---------------------------------------------------------------------------
-- Internal pipeline steps
-- ---------------------------------------------------------------------------

-- | Full lifecycle for a single content item: generate → queue for Discord review.
-- Persistence is intentionally deferred: the 'PostDraft' row is only inserted
-- inside 'rrOnThreadCreated' after the Discord forum thread is successfully
-- created (see 'atomicPersistDraft').  This ensures the database never holds a
-- 'DraftReviewing' record without a 'discord_thread_id'.
processDraft :: PipelineEnv -> Key RawContent -> RawContent -> IO ()
processDraft env@PipelineEnv {..} rcKey rc = do
  putStrLn $ "[Drafts] Generating draft for: " <> T.unpack (rawContentTitle rc)
  draft <- generateDraft pipeAiCfg [rcToDiscovered rc]
  now <- getCurrentTime
  rr <- mkReviewRequest env rcKey now draft
  registerForReview pipeDcCfg rr
  putStrLn "[Drafts] Draft queued for Discord review."

-- | Build the 'ReviewRequest' record that wires the bot callbacks back into
-- the pipeline.  The 'PostDraft' row is not inserted until 'rrOnThreadCreated'
-- fires (after Discord confirms thread creation), so the record always has a
-- 'discord_thread_id' set.  A shared 'IORef' carries the key from
-- 'rrOnThreadCreated' to every other callback; if a callback fires before the
-- key is set (impossible in normal operation) it throws an 'IOError' that is
-- caught and logged by the bot.
mkReviewRequest ::
  PipelineEnv ->
  Key RawContent ->
  UTCTime ->
  GeneratedDraft ->
  IO ReviewRequest
mkReviewRequest env rcKey createdAt draft = do
  postDraftKeyRef <- newIORef Nothing
  pure
    ReviewRequest
      { rrTitle = gdTitle draft,
        rrBodyEn = gdBodyEn draft,
        rrBodyPtBr = gdBodyPtBr draft,
        rrTags = gdTags draft,
        rrRevise = revise postDraftKeyRef,
        rrApprove = approve postDraftKeyRef,
        rrReject = reject' postDraftKeyRef,
        rrOnUserMessage = onUserMsg postDraftKeyRef,
        rrOnBotMessage = onBotMsg postDraftKeyRef,
        rrOnThreadCreated = onThreadCreated postDraftKeyRef
      }
  where
    revise postDraftKeyRef currentBodyEn currentBodyPtBr feedback =
      withPostDraftKey postDraftKeyRef $ \pdKey -> do
        revisedDraft <- reviseDraft (pipeAiCfg env) currentBodyEn currentBodyPtBr feedback
        recordRevision env pdKey revisedDraft
        pure (gdBodyEn revisedDraft, gdBodyPtBr revisedDraft, gdTags revisedDraft)
    approve postDraftKeyRef finalBodyEn finalBodyPtBr tags =
      withPostDraftKey postDraftKeyRef $ \pdKey ->
        publishDraft env rcKey pdKey createdAt finalBodyEn finalBodyPtBr tags
    reject' postDraftKeyRef reason =
      withPostDraftKey postDraftKeyRef $ \pdKey ->
        rejectDraft env rcKey pdKey reason
    onUserMsg postDraftKeyRef msg =
      withPostDraftKey postDraftKeyRef $ \pdKey ->
        insertComment env pdKey CommentAuthorUser msg
    onBotMsg postDraftKeyRef msg =
      withPostDraftKey postDraftKeyRef $ \pdKey ->
        insertComment env pdKey CommentAuthorJarvis (truncateText 500 msg)
    onThreadCreated postDraftKeyRef threadId = do
      pdKey <- atomicPersistDraft env rcKey draft createdAt threadId
      writeIORef postDraftKeyRef (Just pdKey)

-- | Read the post draft key from its 'IORef', throwing an 'IOError' if it is
-- not yet populated.  In normal operation the key is always set by
-- 'rrOnThreadCreated' before any other callback can fire.
withPostDraftKey :: IORef (Maybe (Key PostDraft)) -> (Key PostDraft -> IO a) -> IO a
withPostDraftKey ref action =
  readIORef ref >>= \mKey ->
    case mKey of
      Nothing ->
        ioError $
          userError
            "[Drafts] Post draft key not yet set — 'rrOnThreadCreated' may not have fired"
      Just k -> action k

-- | Atomically persist the 'PostDraft' together with its Discord thread ID.
--
-- Runs a single database transaction that:
-- * Marks the source 'RawContent' as 'ContentDrafted'.
-- * Inserts the 'PostDraft' with 'DraftReviewing' status and the supplied
--   @threadId@ already set, so the record never exists without a thread ID.
-- * Copies subject associations from the source content item.
-- * Inserts the initial 'DraftAiAnalysis' telemetry row.
atomicPersistDraft ::
  PipelineEnv ->
  Key RawContent ->
  GeneratedDraft ->
  UTCTime ->
  -- | Discord thread ID returned by the forum-thread creation REST call.
  Text ->
  IO (Key PostDraft)
atomicPersistDraft PipelineEnv {..} rcKey draft createdAt threadId = do
  now <- getCurrentTime
  runDb pipeDbPool $ do
    update
      rcKey
      [ RawContentStatus =. ContentDrafted,
        RawContentUpdatedAt =. now
      ]
    pdKey <-
      insert
        PostDraft
          { postDraftTitle = gdTitle draft,
            postDraftGitBranch = gdBranch draft,
            postDraftSuggestedTags = TagList (gdTags draft),
            postDraftStatus = DraftReviewing,
            postDraftDiscordThreadId = Just threadId,
            postDraftContentMarkdownEn = Just (gdBodyEn draft),
            postDraftContentMarkdownPtBr = Just (gdBodyPtBr draft),
            postDraftPublishedAt = Nothing,
            postDraftPublishedUrl = Nothing,
            postDraftCreatedAt = createdAt,
            postDraftUpdatedAt = now
          }
    -- Copy subject associations from the source content item.
    rcSubjects <- selectList [RawContentSubjectRawContentId ==. rcKey] []
    mapM_
      ( \e ->
          insert_
            PostDraftSubject
              { postDraftSubjectPostDraftId = pdKey,
                postDraftSubjectSubjectId = rawContentSubjectSubjectId (entityVal e)
              }
      )
      rcSubjects
    insert_
      PostDraftSource
        { postDraftSourcePostDraftId = pdKey,
          postDraftSourceRawContentId = rcKey
        }
    persistDraftAnalysis pdKey draft now
    pure pdKey

-- | Insert a 'DraftAiAnalysis' row for a revision step and update the stored bodies.
recordRevision :: PipelineEnv -> Key PostDraft -> GeneratedDraft -> IO ()
recordRevision PipelineEnv {..} postDraftKey draft = do
  revisedAt <- getCurrentTime
  runDb pipeDbPool $ do
    update
      postDraftKey
      [ PostDraftContentMarkdownEn =. Just (gdBodyEn draft),
        PostDraftContentMarkdownPtBr =. Just (gdBodyPtBr draft),
        PostDraftUpdatedAt =. revisedAt
      ]
    persistDraftAnalysis postDraftKey draft revisedAt

-- | Insert a single 'DraftAiAnalysis' telemetry row for a draft or revision.
-- Exported so it can be tested without going through the full pipeline.
persistDraftAnalysis :: Key PostDraft -> GeneratedDraft -> UTCTime -> SqlPersistT IO ()
persistDraftAnalysis pdKey draft analyzedAt =
  insert_
    DraftAiAnalysis
      { draftAiAnalysisPostDraftId = pdKey,
        draftAiAnalysisSummary = truncateText 500 (gdBodyEn draft),
        draftAiAnalysisTokensUsed = gdTokensUsed draft,
        draftAiAnalysisAnalyzedAt = analyzedAt
      }

-- | Insert a 'ReviewComment' row for a reviewer or bot message.
insertComment :: PipelineEnv -> Key PostDraft -> CommentAuthor -> Text -> IO ()
insertComment PipelineEnv {..} postDraftKey author msg = do
  commentAt <- getCurrentTime
  runDb pipeDbPool $
    insert_
      ReviewComment
        { reviewCommentPostDraftId = postDraftKey,
          reviewCommentAuthor = author,
          reviewCommentMessage = msg,
          reviewCommentCreatedAt = commentAt
        }

-- | Commit and deploy the approved draft, then mark it as 'DraftPublished'.
-- On GitHub error, resets the raw content to 'ContentNew' and re-throws so
-- the scheduled loop retries on the next tick.
publishDraft ::
  PipelineEnv ->
  Key RawContent ->
  Key PostDraft ->
  -- | Draft creation timestamp — used as the Hugo front-matter @date@ field.
  UTCTime ->
  -- | Final English body (may differ from original draft after revisions).
  Text ->
  -- | Final Brazilian Portuguese body.
  Text ->
  -- | Final tags.
  [Text] ->
  IO ()
publishDraft PipelineEnv {..} rcKey postDraftKey createdAt finalBodyEn finalBodyPtBr tags = do
  let (finalTitle, bodyEn) = splitTitle finalBodyEn
      (titlePtBr, bodyPtBr) = splitTitle finalBodyPtBr
      finalSlug = toSlug finalTitle
      mdContentEn = renderHugoPost finalTitle finalSlug createdAt tags bodyEn
      mdContentPtBr = renderHugoPost titlePtBr finalSlug createdAt tags bodyPtBr
      filenameEn = finalSlug <> ".en.md"
      filenamePtBr = finalSlug <> ".pt-br.md"
  putStrLn $ "[Drafts] Approved! Committing " <> T.unpack filenameEn <> " and " <> T.unpack filenamePtBr <> " to GitHub..."
  approvedAt <- getCurrentTime
  runDb pipeDbPool $
    update
      postDraftKey
      [ PostDraftStatus =. DraftApproved,
        PostDraftUpdatedAt =. approvedAt
      ]
  commitResult <-
    ( try $ do
        commitPost pipeGhCfg finalTitle filenameEn mdContentEn
        commitPost pipeGhCfg finalTitle filenamePtBr mdContentPtBr
    ) ::
      IO (Either SomeException ())
  case commitResult of
    Left ex -> do
      -- Commit failed: roll back both the draft and the raw-content status so
      -- the next scheduled run picks this item up and generates a fresh draft.
      failedAt <- getCurrentTime
      runDb pipeDbPool $ do
        update postDraftKey [PostDraftStatus =. DraftReviewing, PostDraftUpdatedAt =. failedAt]
        update rcKey [RawContentStatus =. ContentNew, RawContentUpdatedAt =. failedAt]
      putStrLn $ "[Drafts] Commit failed; reset to pending. Error: " <> displayException ex
      ioError (userError (displayException ex))
    Right () -> do
      -- Commit succeeded: persist published state immediately so a deploy
      -- failure below cannot leave the draft stuck in DraftApproved.
      publishedAt' <- getCurrentTime
      runDb pipeDbPool $
        update
          postDraftKey
          [ PostDraftTitle =. finalTitle,
            PostDraftGitBranch =. "draft/" <> finalSlug,
            PostDraftStatus =. DraftPublished,
            PostDraftContentMarkdownEn =. Just mdContentEn,
            PostDraftContentMarkdownPtBr =. Just mdContentPtBr,
            PostDraftPublishedAt =. Just publishedAt',
            PostDraftUpdatedAt =. publishedAt'
          ]
      putStrLn "[Drafts] Post committed to GitHub. Triggering deploy workflow..."
      deployResult <-
        try (triggerDeploy pipeGhCfg) ::
          IO (Either SomeException ())
      case deployResult of
        Left ex ->
          -- The post is already committed; a deploy failure is non-fatal.
          -- The next push to the repo will trigger the deploy workflow anyway.
          putStrLn $ "[Drafts] Warning: deploy dispatch failed (post is committed): " <> displayException ex
        Right () ->
          putStrLn "[Drafts] Deploy triggered."

-- | Mark the raw content and post draft as rejected.
rejectDraft :: PipelineEnv -> Key RawContent -> Key PostDraft -> Text -> IO ()
rejectDraft PipelineEnv {..} rcKey postDraftKey reason = do
  putStrLn $ "[Drafts] Draft rejected. Reason: " <> T.unpack reason
  rejectedAt <- getCurrentTime
  runDb pipeDbPool $ do
    update
      rcKey
      [ RawContentStatus =. ContentRejected,
        RawContentUpdatedAt =. rejectedAt
      ]
    update
      postDraftKey
      [ PostDraftStatus =. DraftRejected,
        PostDraftUpdatedAt =. rejectedAt
      ]

-- ---------------------------------------------------------------------------
-- Utilities
-- ---------------------------------------------------------------------------

-- | Convert a 'RawContent' row into a 'DiscoveredContent' value suitable for
-- passing to 'generateDraft'.
rcToDiscovered :: RawContent -> DiscoveredContent
rcToDiscovered rc =
  DiscoveredContent
    { dcTitle = rawContentTitle rc,
      dcUrl = rawContentUrl rc,
      dcSummary = rawContentSummary rc,
      dcSubjects = []
    }

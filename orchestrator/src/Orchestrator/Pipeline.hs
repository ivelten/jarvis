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
    retryFailedDrafts,
    handleApproveReview,
    handleRejectReview,
    handleReviseRequest,
    handleCustomPostRequest,
    persistDraftAnalysis,
    renderDraftFiles,
    atomicPersistDraft,
    PersistDraftRequest (..),
    RenderedDraft (..),
    PublishDraftRequest (..),
    DraftingStats (..),
    createSubject,
    disableSubject,
    listEnabledSubjects,
  )
where

import Control.Exception (SomeException, displayException)
import Control.Monad (forM_)
import Data.Int (Int64)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist (Entity (..), entityKey, entityVal, get, insert, insertUnique, insert_, selectFirst, selectList, update, (=.), (==.))
import Database.Persist.Sql (SqlPersistT, fromSqlKey, toSqlKey)
import Orchestrator.AI.Client
  ( AiConfig,
    DiscoveredContent (..),
    GeneratedDraft (..),
    ReviseRequest (..),
    generateCustomDraft,
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
    mkInterestScore,
  )
import Orchestrator.Discord.Bot (ApproveReviewEvent (..), CustomPostRequestEvent (..), DisableSubjectCommandEvent (..), DiscordConfig, RejectReviewEvent (..), ReviewRequest (..), ReviseReviewEvent (..), RevisionResult (..), SubjectCommandEvent (..), activateCustomReview, closeThread, registerForReview, sendInteractionFile, sendInteractionMessage, sendThreadMessage)
import Orchestrator.GitHub.Client (GitHubConfig, commitPost, triggerDeploy)
import Orchestrator.IOUtils (tryIO)
import Orchestrator.Posts.Generator (HugoPostMeta (..), renderHugoPost)
import Orchestrator.TextUtils (emojiApprove, emojiDraft, emojiQueue, emojiReject, emojiSearch, emojiStar, emojiWarning, splitTitle, toSlug, truncateText)
import Orchestrator.Topics.Selector (DiscoveryStats (..), ingestDiscoveredContent, pendingContent)

-- ---------------------------------------------------------------------------
-- Stats
-- ---------------------------------------------------------------------------

-- | Statistics returned by a draft-generation run.
newtype DraftingStats = DraftingStats
  { -- | Tokens consumed by the AI draft-generation call.
    drsTokensUsed :: Int
  }
  deriving (Show, Eq)

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
-- On success, posts a notice to the interaction channel including discovery stats.
runDiscovery :: PipelineEnv -> IO ()
runDiscovery PipelineEnv {..} = do
  putStrLn "[Discovery] Discovering content via Gemini..."
  DiscoveryStats {..} <- runDb pipeDbPool (ingestDiscoveredContent pipeAiCfg)
  putStrLn "[Discovery] Done."
  sendInteractionMessage pipeDcCfg $
    emojiSearch
      <> " **Content discovery complete.**"
      <> " Found "
      <> T.pack (show dsItemsFound)
      <> " item(s), "
      <> T.pack (show dsItemsIngested)
      <> " ingested"
      <> " ("
      <> T.pack (show dsTokensUsed)
      <> " tokens used)."

-- | Pick the next pending content item, generate a Markdown draft, and
-- register it for Discord review.  The approve/reject/revision callbacks are
-- global handlers on 'DiscordConfig' that look up the active draft from the
-- database on every event, so no in-memory state is needed here.
runDraftGeneration :: PipelineEnv -> IO ()
runDraftGeneration env@PipelineEnv {..} = do
  pending <- runDb pipeDbPool pendingContent
  case pending of
    [] -> putStrLn "[Drafts] No pending content to process. Skipping."
    (item : _) -> do
      DraftingStats {..} <- processDraft env (entityKey item) (entityVal item)
      sendInteractionMessage pipeDcCfg $
        emojiQueue
          <> " **Draft ready for review.**"
          <> " Check the forum channel for a new thread"
          <> " ("
          <> T.pack (show drsTokensUsed)
          <> " tokens used)."

-- ---------------------------------------------------------------------------
-- Database-backed event handlers
-- ---------------------------------------------------------------------------

-- | Called by the Discord bot when an approval message arrives.  Looks up the active 'PostDraft' by thread ID, reads the current
-- bodies from the DB, and calls 'publishDraft'.  Works for both source-based
-- drafts and custom (source-free) posts.
handleApproveReview :: PipelineEnv -> ApproveReviewEvent -> IO ()
handleApproveReview env@PipelineEnv {..} ApproveReviewEvent {..} = do
  mDraft <-
    runDb pipeDbPool $
      selectFirst
        [PostDraftDiscordThreadId ==. Just aprThreadId, PostDraftStatus ==. DraftReviewing]
        []
  case mDraft of
    Nothing -> putStrLn $ "[Drafts] No active review for thread " <> T.unpack aprThreadId
    Just (Entity pdKey pd) -> do
      let bodyEn = fromMaybe "" (postDraftContentMarkdownEn pd)
          bodyPtBr = fromMaybe "" (postDraftContentMarkdownPtBr pd)
          tags = unTagList (postDraftSuggestedTags pd)
      insertComment env pdKey CommentAuthorUser aprTriggerText
      sendThreadMessage pipeDcCfg aprThreadId $
        emojiApprove <> " **Draft approved!** Publishing now — this thread is closed."
      publishDraft
        env
        PublishDraftRequest
          { pubDraftKey = pdKey,
            pubCreatedAt = postDraftCreatedAt pd,
            pubBodyEn = bodyEn,
            pubBodyPtBr = bodyPtBr,
            pubTags = tags,
            pubThreadId = Just aprThreadId
          }
      closeThread pipeDcCfg aprThreadId

-- | Called by the Discord bot when a rejection message arrives.
-- Looks up the active 'PostDraft' by thread ID and calls 'rejectDraft'.
-- For custom (source-free) posts, only the draft itself is rejected.
handleRejectReview :: PipelineEnv -> RejectReviewEvent -> IO ()
handleRejectReview env@PipelineEnv {..} RejectReviewEvent {..} = do
  mDraft <-
    runDb pipeDbPool $
      selectFirst
        [PostDraftDiscordThreadId ==. Just rejThreadId, PostDraftStatus ==. DraftReviewing]
        []
  case mDraft of
    Nothing -> putStrLn $ "[Drafts] No active review for thread " <> T.unpack rejThreadId
    Just (Entity pdKey _pd) -> do
      insertComment env pdKey CommentAuthorUser rejReason
      sendThreadMessage pipeDcCfg rejThreadId $
        emojiReject <> " **Draft rejected.** This thread has been closed."
      mSrc <- runDb pipeDbPool $ listToMaybe <$> selectList [PostDraftSourcePostDraftId ==. pdKey] []
      case mSrc of
        Nothing ->
          -- Custom post: no source content to update, just mark the draft rejected.
          rejectDraftOnly env pdKey rejReason
        Just (Entity _ src) ->
          rejectDraft env (postDraftSourceRawContentId src) pdKey rejReason
      closeThread pipeDcCfg rejThreadId

-- | Called by the Discord bot when a non-approval message arrives in a review
-- thread.  Looks up the active 'PostDraft' by thread ID, reads the current
-- body from the DB, calls the AI revision function, persists the result, and
-- returns the updated bodies for the bot to post.
handleReviseRequest :: PipelineEnv -> ReviseReviewEvent -> IO RevisionResult
handleReviseRequest env@PipelineEnv {..} ReviseReviewEvent {..} = do
  mDraft <-
    runDb pipeDbPool $
      selectFirst
        [PostDraftDiscordThreadId ==. Just rvsThreadId, PostDraftStatus ==. DraftReviewing]
        []
  case mDraft of
    Nothing -> do
      putStrLn $ "[Drafts] Revision request for unknown thread: " <> T.unpack rvsThreadId
      return ReviewNotActive
    Just (Entity pdKey pd) -> do
      let bodyEn = fromMaybe "" (postDraftContentMarkdownEn pd)
          bodyPtBr = fromMaybe "" (postDraftContentMarkdownPtBr pd)
      insertComment env pdKey CommentAuthorUser rvsFeedback
      result <-
        tryIO (reviseDraft pipeAiCfg ReviseRequest {rvBodyEn = bodyEn, rvBodyPtBr = bodyPtBr, rvFeedback = rvsFeedback})
      case result of
        Left ex -> do
          let errMsg = T.pack (displayException ex)
          putStrLn $ "[Drafts] Gemini error: " <> T.unpack errMsg
          return (RevisionError errMsg)
        Right draft -> do
          recordRevision env pdKey draft
          insertComment env pdKey CommentAuthorJarvis (truncateText 500 (gdBodyEn draft))
          return (RevisionOk (gdBodyEn draft) (gdBodyPtBr draft))

-- ---------------------------------------------------------------------------
-- Internal pipeline steps
-- ---------------------------------------------------------------------------

-- | Full lifecycle for a single content item: generate → queue for Discord review.
-- Persistence is intentionally deferred: the 'PostDraft' row is only inserted
-- inside 'rrOnThreadCreated' after the Discord forum thread is successfully
-- created (see 'atomicPersistDraft').  This ensures the database never holds a
-- 'DraftReviewing' record without a 'discord_thread_id'.
processDraft :: PipelineEnv -> Key RawContent -> RawContent -> IO DraftingStats
processDraft env@PipelineEnv {..} rcKey rc = do
  putStrLn $ "[Drafts] Generating draft for: " <> T.unpack (rawContentTitle rc)
  draft <- generateDraft pipeAiCfg [rcToDiscovered rc]
  now <- getCurrentTime
  subjects <- runDb pipeDbPool $ do
    subjectRows <- selectList [RawContentSubjectRawContentId ==. rcKey] []
    let subjectIds = map (rawContentSubjectSubjectId . entityVal) subjectRows
    mSubjects <- mapM get subjectIds
    pure (catMaybes mSubjects)
  let subjectNames = map subjectName subjects
      rr =
        ReviewRequest
          { rrTitle = gdTitle draft,
            rrBodyEn = gdBodyEn draft,
            rrBodyPtBr = gdBodyPtBr draft,
            rrTags = gdTags draft,
            rrSourceTitle = rawContentTitle rc,
            rrSourceSummary = rawContentSummary rc,
            rrSourceUrl = rawContentUrl rc,
            rrSubjects = subjectNames,
            rrOnThreadCreated = \threadId -> do
              _ <-
                atomicPersistDraft
                  env
                  PersistDraftRequest
                    { perRcKey = Just rcKey,
                      perDraft = draft,
                      perCreatedAt = now,
                      perThreadId = threadId
                    }
              pure ()
          }
  registerForReview pipeDcCfg rr
  putStrLn "[Drafts] Draft queued for Discord review."
  pure DraftingStats {drsTokensUsed = gdTokensUsed draft}

-- | All data needed to atomically persist a new draft along with its Discord thread ID.
data PersistDraftRequest = PersistDraftRequest
  { -- | Source content key.  'Nothing' for custom (source-free) posts.
    perRcKey :: !(Maybe (Key RawContent)),
    perDraft :: !GeneratedDraft,
    perCreatedAt :: !UTCTime,
    perThreadId :: !Text
  }

-- | Atomically persist the 'PostDraft' together with its Discord thread ID.
--
-- Runs a single database transaction that:
-- * Inserts the 'PostDraft' with 'DraftReviewing' status and the supplied
--   @threadId@ already set, so the record never exists without a thread ID.
-- * For source-based drafts (@perRcKey = Just rcKey@):
--     * Marks the 'RawContent' as 'ContentDrafted'.
--     * Copies subject associations from the source content item.
--     * Links the draft to the source via 'PostDraftSource'.
-- * Inserts the initial 'DraftAiAnalysis' telemetry row.
atomicPersistDraft :: PipelineEnv -> PersistDraftRequest -> IO (Key PostDraft)
atomicPersistDraft PipelineEnv {..} PersistDraftRequest {..} = do
  now <- getCurrentTime
  runDb pipeDbPool $ do
    pdKey <-
      insert
        PostDraft
          { postDraftTitle = gdTitle perDraft,
            postDraftGitBranch = gdBranch perDraft,
            postDraftSuggestedTags = TagList (gdTags perDraft),
            postDraftStatus = DraftReviewing,
            postDraftDiscordThreadId = Just perThreadId,
            postDraftContentMarkdownEn = Just (gdBodyEn perDraft),
            postDraftContentMarkdownPtBr = Just (gdBodyPtBr perDraft),
            postDraftPublishedAt = Nothing,
            postDraftPublishedUrl = Nothing,
            postDraftCreatedAt = perCreatedAt,
            postDraftUpdatedAt = now
          }
    -- Source-specific steps: update content status, copy subjects, link source.
    forM_ perRcKey $ \rcKey -> do
      update
        rcKey
        [ RawContentStatus =. ContentDrafted,
          RawContentUpdatedAt =. now
        ]
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
    persistDraftAnalysis pdKey perDraft now
    pure pdKey

-- | Insert a 'DraftAiAnalysis' row for a revision step and update the stored
-- bodies and tags.
recordRevision :: PipelineEnv -> Key PostDraft -> GeneratedDraft -> IO ()
recordRevision PipelineEnv {..} postDraftKey draft = do
  revisedAt <- getCurrentTime
  runDb pipeDbPool $ do
    update
      postDraftKey
      [ PostDraftContentMarkdownEn =. Just (gdBodyEn draft),
        PostDraftContentMarkdownPtBr =. Just (gdBodyPtBr draft),
        PostDraftSuggestedTags =. TagList (gdTags draft),
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

-- | All data needed to commit a draft to GitHub and mark it published.
data PublishDraftRequest = PublishDraftRequest
  { pubDraftKey :: !(Key PostDraft),
    pubCreatedAt :: !UTCTime,
    pubBodyEn :: !Text,
    pubBodyPtBr :: !Text,
    pubTags :: ![Text],
    pubThreadId :: !(Maybe Text)
  }

-- | Commit and deploy the approved draft, then mark it as 'DraftPublished'.
-- On commit error marks the draft 'DraftPublishFailed' (for the retry worker)
-- and re-throws.  A deploy failure is non-fatal: the post is already committed.
publishDraft :: PipelineEnv -> PublishDraftRequest -> IO ()
publishDraft env@PipelineEnv {..} req@PublishDraftRequest {..} = do
  let rendered = renderDraftFiles req
  putStrLn $
    "[Drafts] Approved! Committing "
      <> T.unpack (rdFilenameEn rendered)
      <> " and "
      <> T.unpack (rdFilenamePtBr rendered)
      <> " to GitHub..."
  approvedAt <- getCurrentTime
  runDb pipeDbPool $
    update pubDraftKey [PostDraftStatus =. DraftApproved, PostDraftUpdatedAt =. approvedAt]
  commitResult <-
    tryIO (commitRenderedFiles env rendered)
  case commitResult of
    Left ex -> onCommitFailure env pubDraftKey ex
    Right () -> onCommitSuccess env req rendered

-- | Rendered file content derived from a 'PublishDraftRequest'.
data RenderedDraft = RenderedDraft
  { rdTitle :: !Text,
    rdSlug :: !Text,
    rdFilenameEn :: !Text,
    rdFilenamePtBr :: !Text,
    rdContentEn :: !Text,
    rdContentPtBr :: !Text
  }

-- | Derive filenames and Hugo-formatted content from a publish request.
renderDraftFiles :: PublishDraftRequest -> RenderedDraft
renderDraftFiles PublishDraftRequest {..} =
  let (finalTitle, bodyEn) = splitTitle pubBodyEn
      (titlePtBr, bodyPtBr) = splitTitle pubBodyPtBr
      finalSlug = toSlug finalTitle
      metaEn = HugoPostMeta {hpmTitle = finalTitle, hpmSlug = finalSlug, hpmDate = pubCreatedAt, hpmTags = pubTags}
      metaPtBr = HugoPostMeta {hpmTitle = titlePtBr, hpmSlug = finalSlug, hpmDate = pubCreatedAt, hpmTags = pubTags}
   in RenderedDraft
        { rdTitle = finalTitle,
          rdSlug = finalSlug,
          rdFilenameEn = finalSlug <> ".en.md",
          rdFilenamePtBr = finalSlug <> ".pt-br.md",
          rdContentEn = renderHugoPost metaEn bodyEn,
          rdContentPtBr = renderHugoPost metaPtBr bodyPtBr
        }

-- | Commit both rendered files to GitHub.
commitRenderedFiles :: PipelineEnv -> RenderedDraft -> IO ()
commitRenderedFiles PipelineEnv {..} RenderedDraft {..} = do
  commitPost pipeGhCfg rdTitle rdFilenameEn rdContentEn
  commitPost pipeGhCfg rdTitle rdFilenamePtBr rdContentPtBr

-- | Handle a failed commit: mark the draft 'DraftPublishFailed' and re-throw.
onCommitFailure :: PipelineEnv -> Key PostDraft -> SomeException -> IO ()
onCommitFailure PipelineEnv {..} draftKey ex = do
  -- Commit failed: mark the draft as DraftPublishFailed so the retry
  -- worker can pick it up.  The source RawContent stays ContentDrafted
  -- so a new draft is not generated for the same content.
  failedAt <- getCurrentTime
  runDb pipeDbPool $
    update draftKey [PostDraftStatus =. DraftPublishFailed, PostDraftUpdatedAt =. failedAt]
  putStrLn $ "[Drafts] Commit failed; marked as publish_failed. Error: " <> displayException ex
  ioError (userError (displayException ex))

-- | Handle a successful commit: persist published state then trigger deploy.
onCommitSuccess :: PipelineEnv -> PublishDraftRequest -> RenderedDraft -> IO ()
onCommitSuccess PipelineEnv {..} PublishDraftRequest {..} RenderedDraft {..} = do
  -- Persist published state immediately so a deploy failure below cannot
  -- leave the draft stuck in DraftApproved.
  publishedAt' <- getCurrentTime
  runDb pipeDbPool $
    update
      pubDraftKey
      [ PostDraftTitle =. rdTitle,
        PostDraftGitBranch =. "draft/" <> rdSlug,
        PostDraftStatus =. DraftPublished,
        PostDraftContentMarkdownEn =. Just rdContentEn,
        PostDraftContentMarkdownPtBr =. Just rdContentPtBr,
        PostDraftPublishedAt =. Just publishedAt',
        PostDraftUpdatedAt =. publishedAt'
      ]
  putStrLn "[Drafts] Post committed to GitHub. Triggering deploy workflow..."
  deployResult <- tryIO (triggerDeploy pipeGhCfg)
  case deployResult of
    Left ex -> do
      -- The post is already committed; a deploy failure is non-fatal.
      -- The next push to the repo will trigger the deploy workflow anyway.
      putStrLn $ "[Drafts] Warning: deploy dispatch failed (post is committed): " <> displayException ex
      sendInteractionMessage pipeDcCfg $
        emojiWarning <> " **Deploy failed.** _" <> rdTitle <> "_ has been committed to GitHub, but the deploy workflow could not be triggered automatically."
      forM_ pubThreadId $ \tid ->
        sendThreadMessage pipeDcCfg tid $
          emojiWarning <> " **Deploy failed.** The workflow could not be triggered automatically \x2014 the post is committed and will deploy on the next push."
    Right () -> do
      putStrLn "[Drafts] Deploy triggered."
      sendInteractionMessage pipeDcCfg $
        emojiApprove <> " **Post published!** _" <> rdTitle <> "_ has been committed to GitHub and the deploy workflow has been triggered."

-- | Retry all drafts that previously failed to publish.
--
-- Looks up every 'PostDraft' with status 'DraftPublishFailed', reads their
-- latest bodies from the DB, and calls 'publishDraft' for each one.
-- Intended to run on a lightweight scheduled loop so transient GitHub errors
-- are automatically resolved without manual intervention.
retryFailedDrafts :: PipelineEnv -> IO ()
retryFailedDrafts env@PipelineEnv {..} = do
  failed <- runDb pipeDbPool $ selectList [PostDraftStatus ==. DraftPublishFailed] []
  case failed of
    [] -> putStrLn "[Retry] No failed drafts to retry."
    drafts -> do
      putStrLn $ "[Retry] Retrying " <> show (length drafts) <> " failed draft(s)..."
      mapM_ retryOne drafts
  where
    retryOne (Entity pdKey pd) = do
      let bodyEn = fromMaybe "" (postDraftContentMarkdownEn pd)
          bodyPtBr = fromMaybe "" (postDraftContentMarkdownPtBr pd)
          tags = unTagList (postDraftSuggestedTags pd)
      putStrLn $ "[Retry] Retrying publish for: '" <> T.unpack (postDraftTitle pd) <> "'"
      result <-
        tryIO
          ( publishDraft
              env
              PublishDraftRequest
                { pubDraftKey = pdKey,
                  pubCreatedAt = postDraftCreatedAt pd,
                  pubBodyEn = bodyEn,
                  pubBodyPtBr = bodyPtBr,
                  pubTags = tags,
                  pubThreadId = postDraftDiscordThreadId pd
                }
          )
      case result of
        Left ex -> putStrLn $ "[Retry] Publish still failing for '" <> T.unpack (postDraftTitle pd) <> "': " <> displayException ex
        Right () -> putStrLn $ "[Retry] Successfully published '" <> T.unpack (postDraftTitle pd) <> "'."

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

-- | Mark only the post draft as rejected (used for custom posts that have no
-- associated 'RawContent' to update).
rejectDraftOnly :: PipelineEnv -> Key PostDraft -> Text -> IO ()
rejectDraftOnly PipelineEnv {..} postDraftKey reason = do
  putStrLn $ "[Drafts] Custom draft rejected. Reason: " <> T.unpack reason
  rejectedAt <- getCurrentTime
  runDb pipeDbPool $
    update
      postDraftKey
      [ PostDraftStatus =. DraftRejected,
        PostDraftUpdatedAt =. rejectedAt
      ]

-- | Handle a new user-created forum thread: generate a bilingual blog post
-- from the thread title (as a hint) and the first message (as instructions),
-- persist the draft, and post it back to the same thread for the normal
-- review flow.
--
-- Idempotent: if the thread already has a 'PostDraft' (any status), the
-- handler logs and notifies the user without creating a second draft.
handleCustomPostRequest :: PipelineEnv -> CustomPostRequestEvent -> IO ()
handleCustomPostRequest env@PipelineEnv {..} evt@CustomPostRequestEvent {..} = do
  putStrLn $ "[CustomPost] New thread created: " <> T.unpack cpThreadId
  mExisting <-
    runDb pipeDbPool $
      selectFirst [PostDraftDiscordThreadId ==. Just cpThreadId] []
  case (mExisting :: Maybe (Entity PostDraft)) of
    Just _ -> do
      putStrLn $ "[CustomPost] Thread " <> T.unpack cpThreadId <> " already has a draft; ignoring."
      sendThreadMessage pipeDcCfg cpThreadId $
        emojiWarning <> " This thread already has a draft associated with it."
    Nothing -> do
      mDraft <- customPostGenerate env evt
      mapM_ (customPostPersistAndActivate env evt) mDraft

-- | Full lifecycle for a custom post request: generate → persist → activate review.
customPostGenerate :: PipelineEnv -> CustomPostRequestEvent -> IO (Maybe GeneratedDraft)
customPostGenerate PipelineEnv {..} CustomPostRequestEvent {..} = do
  putStrLn $ "[CustomPost] Generating draft for: \"" <> T.unpack cpTitleHint <> "\""
  sendThreadMessage pipeDcCfg cpThreadId $
    emojiQueue <> " **Generating post draft…** This may take a moment."
  result <-
    tryIO (generateCustomDraft pipeAiCfg cpTitleHint cpInstructions)
  case result of
    Left ex -> do
      let errMsg = T.pack (displayException ex)
      putStrLn $ "[CustomPost] Generation failed: " <> T.unpack errMsg
      sendThreadMessage pipeDcCfg cpThreadId $
        emojiWarning <> " **Draft generation failed:** " <> errMsg
      pure Nothing
    Right draft -> pure (Just draft)

-- | Persist the generated draft and activate the review thread with the draft content.
customPostPersistAndActivate :: PipelineEnv -> CustomPostRequestEvent -> GeneratedDraft -> IO ()
customPostPersistAndActivate env@PipelineEnv {..} CustomPostRequestEvent {..} draft = do
  now <- getCurrentTime
  persistResult <-
    tryIO
      ( atomicPersistDraft
          env
          PersistDraftRequest
            { perRcKey = Nothing,
              perDraft = draft,
              perCreatedAt = now,
              perThreadId = cpThreadId
            }
      )
  case persistResult of
    Left ex -> do
      let errMsg = T.pack (displayException ex)
      putStrLn $ "[CustomPost] Failed to persist draft: " <> T.unpack errMsg
      sendThreadMessage pipeDcCfg cpThreadId $
        emojiWarning <> " **Failed to save draft:** " <> errMsg
    Right _ -> do
      let tagsNote
            | null (gdTags draft) = ""
            | otherwise =
                "\n**Tags:** "
                  <> T.intercalate ", " (map (\t -> "`" <> t <> "`") (gdTags draft))
      sendThreadMessage pipeDcCfg cpThreadId $
        emojiDraft
          <> " **Draft ready!** Title: _"
          <> gdTitle draft
          <> "_"
          <> tagsNote
          <> "\n\nType **approve** to approve, **reject** to reject, or type feedback to request changes."
      activateCustomReview pipeDcCfg cpThreadId (gdBodyEn draft) (gdBodyPtBr draft)
      sendInteractionMessage pipeDcCfg $
        emojiStar
          <> " **Custom post draft ready for review.** _"
          <> gdTitle draft
          <> "_ ("
          <> T.pack (show (gdTokensUsed draft))
          <> " tokens used)."
      putStrLn $ "[CustomPost] Draft ready in thread " <> T.unpack cpThreadId

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

-- | Disable an existing 'Subject' by its Int64 key.
-- Posts a confirmation or not-found message to the interaction channel.
disableSubject :: PipelineEnv -> DisableSubjectCommandEvent -> IO ()
disableSubject PipelineEnv {..} DisableSubjectCommandEvent {..} = do
  let subjectKey = toSqlKey dsceSubjectId :: SubjectId
  mSubject <- runDb pipeDbPool $ get subjectKey
  case mSubject of
    Nothing ->
      sendInteractionMessage pipeDcCfg $
        emojiWarning <> " Subject with ID " <> T.pack (show dsceSubjectId) <> " not found."
    Just _ -> do
      runDb pipeDbPool $ update subjectKey [SubjectEnabled =. False]
      sendInteractionMessage pipeDcCfg $
        emojiApprove <> " Subject " <> T.pack (show dsceSubjectId) <> " disabled."

-- | Post a Markdown table of all enabled subjects to the interaction channel.
listEnabledSubjects :: PipelineEnv -> IO ()
listEnabledSubjects PipelineEnv {..} = do
  subjects <- runDb pipeDbPool $ selectList [SubjectEnabled ==. True] []
  let content = T.unlines [formatRow s | s <- subjects]
  sendInteractionFile pipeDcCfg "subjects.md" "**Enabled subjects:**" content
  where
    formatRow (Entity k v) =
      T.pack (show (fromSqlKey k :: Int64))
        <> ". "
        <> subjectName v

-- | Create a new 'Subject' with interest score 3.
-- Posts a confirmation or duplicate-warning message to the interaction channel.
createSubject :: PipelineEnv -> SubjectCommandEvent -> IO ()
createSubject PipelineEnv {..} SubjectCommandEvent {..} = do
  now <- getCurrentTime
  let score = either (error . T.unpack) id (mkInterestScore 3)
  mKey <-
    runDb pipeDbPool $
      insertUnique
        Subject
          { subjectName = sceSubjectName,
            subjectInterestScore = score,
            subjectEnabled = True,
            subjectCreatedAt = now,
            subjectUpdatedAt = now
          }
  case mKey of
    Nothing ->
      sendInteractionMessage pipeDcCfg $
        emojiWarning <> " Subject \"" <> sceSubjectName <> "\" already exists."
    Just _ ->
      sendInteractionMessage pipeDcCfg $
        emojiApprove <> " Subject \"" <> sceSubjectName <> "\" added with interest score 3."

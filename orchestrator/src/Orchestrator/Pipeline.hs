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
    persistDraftAnalysis,
    renderDraftFiles,
    RenderedDraft (..),
    PublishDraftRequest (..),
    DraftingStats (..),
    createSubject,
    disableSubject,
    listEnabledSubjects,
  )
where

import Control.Exception (SomeException, displayException, try)
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
import Orchestrator.Discord.Bot (ApproveReviewEvent (..), DisableSubjectCommandEvent (..), DiscordConfig, RejectReviewEvent (..), ReviewRequest (..), ReviseReviewEvent (..), RevisionResult (..), SubjectCommandEvent (..), registerForReview, sendInteractionFile, sendInteractionMessage, sendThreadMessage)
import Orchestrator.GitHub.Client (GitHubConfig, commitPost, triggerDeploy)
import Orchestrator.Posts.Generator (HugoPostMeta (..), renderHugoPost)
import Orchestrator.TextUtils (emojiApprove, emojiQueue, emojiReject, emojiSearch, emojiWarning, splitTitle, toSlug, truncateText)
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

-- | Called by the Discord bot when an approve reaction or approval message
-- arrives.  Looks up the active 'PostDraft' by thread ID, reads the current
-- bodies from the DB, and calls 'publishDraft'.
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
      srcRows <- runDb pipeDbPool $ selectList [PostDraftSourcePostDraftId ==. pdKey] []
      case listToMaybe srcRows of
        Nothing ->
          putStrLn $
            "[Drafts] No source content for draft '"
              <> T.unpack (postDraftTitle pd)
              <> "'"
        Just (Entity _ src) -> do
          let rcKey = postDraftSourceRawContentId src
              bodyEn = fromMaybe "" (postDraftContentMarkdownEn pd)
              bodyPtBr = fromMaybe "" (postDraftContentMarkdownPtBr pd)
              tags = unTagList (postDraftSuggestedTags pd)
          insertComment env pdKey CommentAuthorUser aprTriggerText
          sendThreadMessage pipeDcCfg aprThreadId $
            emojiApprove <> " **Draft approved!** Publishing now — this thread is closed."
          publishDraft
            env
            PublishDraftRequest
              { pubRcKey = rcKey,
                pubDraftKey = pdKey,
                pubCreatedAt = postDraftCreatedAt pd,
                pubBodyEn = bodyEn,
                pubBodyPtBr = bodyPtBr,
                pubTags = tags,
                pubThreadId = Just aprThreadId
              }

-- | Called by the Discord bot when a reject reaction arrives.
-- Looks up the active 'PostDraft' by thread ID and calls 'rejectDraft'.
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
      srcRows <- runDb pipeDbPool $ selectList [PostDraftSourcePostDraftId ==. pdKey] []
      case listToMaybe srcRows of
        Nothing -> putStrLn "[Drafts] No source content for draft"
        Just (Entity _ src) -> do
          let rcKey = postDraftSourceRawContentId src
          insertComment env pdKey CommentAuthorUser rejReason
          rejectDraft env rcKey pdKey rejReason
          sendThreadMessage pipeDcCfg rejThreadId $
            emojiReject <> " **Draft rejected.** This thread has been closed."

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
        try (reviseDraft pipeAiCfg ReviseRequest {rvBodyEn = bodyEn, rvBodyPtBr = bodyPtBr, rvFeedback = rvsFeedback}) ::
          IO (Either SomeException GeneratedDraft)
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
                    { perRcKey = rcKey,
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
  { perRcKey :: !(Key RawContent),
    perDraft :: !GeneratedDraft,
    perCreatedAt :: !UTCTime,
    perThreadId :: !Text
  }

-- | Atomically persist the 'PostDraft' together with its Discord thread ID.
--
-- Runs a single database transaction that:
-- * Marks the source 'RawContent' as 'ContentDrafted'.
-- * Inserts the 'PostDraft' with 'DraftReviewing' status and the supplied
--   @threadId@ already set, so the record never exists without a thread ID.
-- * Copies subject associations from the source content item.
-- * Inserts the initial 'DraftAiAnalysis' telemetry row.
atomicPersistDraft :: PipelineEnv -> PersistDraftRequest -> IO (Key PostDraft)
atomicPersistDraft PipelineEnv {..} PersistDraftRequest {..} = do
  now <- getCurrentTime
  runDb pipeDbPool $ do
    update
      perRcKey
      [ RawContentStatus =. ContentDrafted,
        RawContentUpdatedAt =. now
      ]
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
    -- Copy subject associations from the source content item.
    rcSubjects <- selectList [RawContentSubjectRawContentId ==. perRcKey] []
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
          postDraftSourceRawContentId = perRcKey
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
  { pubRcKey :: !(Key RawContent),
    pubDraftKey :: !(Key PostDraft),
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
    try (commitRenderedFiles env rendered) :: IO (Either SomeException ())
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
  deployResult <- try (triggerDeploy pipeGhCfg) :: IO (Either SomeException ())
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
      srcRows <- runDb pipeDbPool $ selectList [PostDraftSourcePostDraftId ==. pdKey] []
      case listToMaybe srcRows of
        Nothing -> putStrLn $ "[Retry] No source content for draft '" <> T.unpack (postDraftTitle pd) <> "'; skipping."
        Just (Entity _ src) -> do
          let rcKey = postDraftSourceRawContentId src
              bodyEn = fromMaybe "" (postDraftContentMarkdownEn pd)
              bodyPtBr = fromMaybe "" (postDraftContentMarkdownPtBr pd)
              tags = unTagList (postDraftSuggestedTags pd)
          putStrLn $ "[Retry] Retrying publish for: '" <> T.unpack (postDraftTitle pd) <> "'"
          result <-
            try
              ( publishDraft
                  env
                  PublishDraftRequest
                    { pubRcKey = rcKey,
                      pubDraftKey = pdKey,
                      pubCreatedAt = postDraftCreatedAt pd,
                      pubBodyEn = bodyEn,
                      pubBodyPtBr = bodyPtBr,
                      pubTags = tags,
                      pubThreadId = postDraftDiscordThreadId pd
                    }
              ) ::
              IO (Either SomeException ())
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

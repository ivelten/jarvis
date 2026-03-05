{-# LANGUAGE RecordWildCards #-}

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
  )
where

import Control.Exception (SomeException, displayException, try)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist (entityKey, entityVal, insert, insert_, update, (=.))
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
import Orchestrator.Discord.Bot (DiscordConfig, ReviewRequest (..), registerForReview)
import Orchestrator.GitHub.Client (GitHubConfig, commitPost, triggerDeploy)
import Orchestrator.Posts.Generator (renderHugoPost)
import Orchestrator.TextUtils (splitTitle, toSlug, truncateText)
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
runDiscovery :: PipelineEnv -> IO ()
runDiscovery PipelineEnv {..} = do
  putStrLn "[Discovery] Discovering content via Gemini..."
  runDb pipeDbPool (ingestDiscoveredContent pipeAiCfg)
  putStrLn "[Discovery] Done."

-- | Pick the next pending content item, generate a Markdown draft, and
-- register it for Discord review.  The approve/reject callbacks fire as
-- soon as the reviewer reacts or types an approval in the Discord thread.
runDraftGeneration :: PipelineEnv -> IO ()
runDraftGeneration env@PipelineEnv {..} = do
  pending <- runDb pipeDbPool pendingContent
  case pending of
    [] -> putStrLn "[Drafts] No pending content to process. Skipping."
    (item : _) -> processDraft env (entityKey item) (entityVal item)

-- ---------------------------------------------------------------------------
-- Internal pipeline steps
-- ---------------------------------------------------------------------------

-- | Full lifecycle for a single content item: generate → persist → review.
processDraft :: PipelineEnv -> Key RawContent -> RawContent -> IO ()
processDraft env@PipelineEnv {..} rcKey rc = do
  putStrLn $ "[Drafts] Generating draft for: " <> T.unpack (rawContentTitle rc)
  draft <- generateDraft pipeAiCfg [rcToDiscovered rc]
  now <- getCurrentTime
  markAsDrafted env rcKey now
  postDraftKey <- persistInitialDraft env rcKey rc draft now
  putStrLn "[Drafts] Draft sent to Discord for review."
  registerForReview pipeDcCfg (mkReviewRequest env rcKey postDraftKey now draft)

-- | Flip the raw-content status to 'ContentDrafted' immediately so a
-- restart does not re-pick the same item.
markAsDrafted :: PipelineEnv -> Key RawContent -> UTCTime -> IO ()
markAsDrafted PipelineEnv {..} rcKey now =
  runDb pipeDbPool $
    update
      rcKey
      [ RawContentStatus =. ContentDrafted,
        RawContentUpdatedAt =. now
      ]

-- | Insert the initial 'PostDraft', 'PostDraftSource', and 'AiAnalysis' rows.
-- Returns the new 'PostDraft' key.
persistInitialDraft ::
  PipelineEnv ->
  Key RawContent ->
  RawContent ->
  GeneratedDraft ->
  UTCTime ->
  IO (Key PostDraft)
persistInitialDraft PipelineEnv {..} rcKey rc draft now =
  runDb pipeDbPool $ do
    pdKey <-
      insert
        PostDraft
          { postDraftTitle = gdTitle draft,
            postDraftGitBranch = gdBranch draft,
            postDraftSubjectId = rawContentSubjectId rc,
            postDraftSuggestedTags = TagList (gdTags draft),
            postDraftStatus = DraftReviewing,
            postDraftDiscordThreadId = Nothing,
            postDraftContentMarkdown = Just (gdBody draft),
            postDraftPublishedAt = Nothing,
            postDraftPublishedUrl = Nothing,
            postDraftCreatedAt = now,
            postDraftUpdatedAt = now
          }
    insert_
      PostDraftSource
        { postDraftSourcePostDraftId = pdKey,
          postDraftSourceRawContentId = rcKey
        }
    insert_
      AiAnalysis
        { aiAnalysisPostDraftId = pdKey,
          aiAnalysisSummary = truncateText 500 (gdBody draft),
          aiAnalysisTokensUsed = gdTokensUsed draft,
          aiAnalysisAnalyzedAt = now
        }
    pure pdKey

-- | Build the 'ReviewRequest' record that wires the bot callbacks back into
-- the pipeline.  All callbacks close over the pipeline environment and the
-- relevant DB keys, keeping 'processDraft' free of callback scaffolding.
mkReviewRequest ::
  PipelineEnv ->
  Key RawContent ->
  Key PostDraft ->
  UTCTime ->
  GeneratedDraft ->
  ReviewRequest
mkReviewRequest env rcKey postDraftKey createdAt draft =
  ReviewRequest
    { rrTitle = gdTitle draft,
      rrBody = gdBody draft,
      rrTags = gdTags draft,
      rrRevise = revise,
      rrApprove = publishDraft env rcKey postDraftKey createdAt,
      rrReject = rejectDraft env rcKey postDraftKey,
      rrOnUserMessage = insertComment env postDraftKey CommentAuthorUser,
      rrOnBotMessage = insertComment env postDraftKey CommentAuthorJarvis . truncateText 500,
      rrOnThreadCreated = recordThreadCreated env postDraftKey
    }
  where
    revise currentBody feedback = do
      revisedDraft <- reviseDraft (pipeAiCfg env) currentBody feedback
      recordRevision env postDraftKey revisedDraft
      pure (gdBody revisedDraft, gdTags revisedDraft)

-- | Insert an 'AiAnalysis' row for a revision step and update the stored body.
recordRevision :: PipelineEnv -> Key PostDraft -> GeneratedDraft -> IO ()
recordRevision PipelineEnv {..} postDraftKey draft = do
  revisedAt <- getCurrentTime
  runDb pipeDbPool $ do
    update
      postDraftKey
      [ PostDraftContentMarkdown =. Just (gdBody draft),
        PostDraftUpdatedAt =. revisedAt
      ]
    insert_
      AiAnalysis
        { aiAnalysisPostDraftId = postDraftKey,
          aiAnalysisSummary = truncateText 500 (gdBody draft),
          aiAnalysisTokensUsed = gdTokensUsed draft,
          aiAnalysisAnalyzedAt = revisedAt
        }

-- | Persist the Discord thread ID once the forum thread has been created.
recordThreadCreated :: PipelineEnv -> Key PostDraft -> Text -> IO ()
recordThreadCreated PipelineEnv {..} postDraftKey threadId = do
  now <- getCurrentTime
  runDb pipeDbPool $
    update
      postDraftKey
      [ PostDraftDiscordThreadId =. Just threadId,
        PostDraftUpdatedAt =. now
      ]

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
  -- | Final body (may differ from original draft after revisions).
  Text ->
  -- | Final tags.
  [Text] ->
  IO ()
publishDraft PipelineEnv {..} rcKey postDraftKey createdAt finalBody tags = do
  let (finalTitle, _) = splitTitle finalBody
      finalSlug = toSlug finalTitle
      finalFilename = finalSlug <> ".md"
      mdContent = renderHugoPost finalTitle finalSlug createdAt tags finalBody
  putStrLn $ "[Drafts] Approved! Committing " <> T.unpack finalFilename <> " to GitHub..."
  approvedAt <- getCurrentTime
  runDb pipeDbPool $
    update
      postDraftKey
      [ PostDraftStatus =. DraftApproved,
        PostDraftUpdatedAt =. approvedAt
      ]
  commitResult <-
    (try (commitPost pipeGhCfg finalTitle finalFilename mdContent)) ::
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
            PostDraftContentMarkdown =. Just mdContent,
            PostDraftPublishedAt =. Just publishedAt',
            PostDraftUpdatedAt =. publishedAt'
          ]
      putStrLn "[Drafts] Post committed to GitHub. Triggering deploy workflow..."
      deployResult <-
        (try (triggerDeploy pipeGhCfg)) ::
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
  putStrLn $ "[Drafts] Draft rejected: " <> T.unpack reason

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
      dcSubject = Nothing
    }

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
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist (entityKey, entityVal, insert, insert_, selectList, update, (=.), (==.))
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
  postDraftKey <- persistInitialDraft env rcKey draft now
  putStrLn "[Drafts] Draft sent to Discord for review."
  rr <- mkReviewRequest env rcKey postDraftKey now draft
  registerForReview pipeDcCfg rr

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
  GeneratedDraft ->
  UTCTime ->
  IO (Key PostDraft)
persistInitialDraft PipelineEnv {..} rcKey draft now =
  runDb pipeDbPool $ do
    pdKey <-
      insert
        PostDraft
          { postDraftTitle = gdTitle draft,
            postDraftGitBranch = gdBranch draft,
            postDraftSuggestedTags = TagList (gdTags draft),
            postDraftStatus = DraftReviewing,
            postDraftDiscordThreadId = Nothing,
            postDraftContentMarkdownEn = Just (gdBodyEn draft),
            postDraftContentMarkdownPtBr = Just (gdBodyPtBr draft),
            postDraftPublishedAt = Nothing,
            postDraftPublishedUrl = Nothing,
            postDraftCreatedAt = now,
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
    insert_
      AiAnalysis
        { aiAnalysisPostDraftId = pdKey,
          aiAnalysisSummary = truncateText 500 (gdBodyEn draft),
          aiAnalysisTokensUsed = gdTokensUsed draft,
          aiAnalysisAnalyzedAt = now
        }
    pure pdKey

-- | Build the 'ReviewRequest' record that wires the bot callbacks back into
-- the pipeline.  All callbacks close over the pipeline environment and the
-- relevant DB keys, keeping 'processDraft' free of callback scaffolding.
-- The Brazilian Portuguese body is tracked privately via an 'IORef' so that
-- 'ReviewRequest' (and the Discord bot) only ever see the English body.
mkReviewRequest ::
  PipelineEnv ->
  Key RawContent ->
  Key PostDraft ->
  UTCTime ->
  GeneratedDraft ->
  IO ReviewRequest
mkReviewRequest env rcKey postDraftKey createdAt draft = do
  ptBrRef <- newIORef (gdBodyPtBr draft)
  pure
    ReviewRequest
      { rrTitle = gdTitle draft,
        rrBodyEn = gdBodyEn draft,
        rrTags = gdTags draft,
        rrRevise = revise ptBrRef,
        rrApprove = approve ptBrRef,
        rrReject = rejectDraft env rcKey postDraftKey,
        rrOnUserMessage = insertComment env postDraftKey CommentAuthorUser,
        rrOnBotMessage = insertComment env postDraftKey CommentAuthorJarvis . truncateText 500,
        rrOnThreadCreated = recordThreadCreated env postDraftKey
      }
  where
    revise ptBrRef currentBodyEn feedback = do
      currentBodyPtBr <- readIORef ptBrRef
      revisedDraft <- reviseDraft (pipeAiCfg env) currentBodyEn currentBodyPtBr feedback
      writeIORef ptBrRef (gdBodyPtBr revisedDraft)
      recordRevision env postDraftKey revisedDraft
      pure (gdBodyEn revisedDraft, gdTags revisedDraft)
    approve ptBrRef finalBodyEn tags = do
      finalBodyPtBr <- readIORef ptBrRef
      publishDraft env rcKey postDraftKey createdAt finalBodyEn finalBodyPtBr tags

-- | Insert an 'AiAnalysis' row for a revision step and update the stored bodies.
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
    insert_
      AiAnalysis
        { aiAnalysisPostDraftId = postDraftKey,
          aiAnalysisSummary = truncateText 500 (gdBodyEn draft),
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

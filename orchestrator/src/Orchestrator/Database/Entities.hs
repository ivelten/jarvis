{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Orchestrator.Database.Entities where

import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )
import Orchestrator.Database.Models (CommentAuthor, ContentStatus, DraftStatus, TagList)

-- | All database entities for the orchestrator.
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

-- | A curated subject of interest for the blog.
-- The interest score controls how eagerly new content on this subject is drafted.
Subject
  name          Text          -- e.g. "concurrency", "type system"
  description   Text Maybe
  interestScore Int           -- 1 (low) – 5 (high); curated by the user
  createdAt     UTCTime
  updatedAt     UTCTime
  UniqueSubjectName name
  deriving Show Eq

-- | A piece of web content discovered by the AI as a potential blog source.
RawContent
  title           Text
  url             Text
  summary         Text
  rawHtml         Text Maybe  -- archived page HTML; preserved in case the original URL goes dead
  subjectId       SubjectId Maybe  -- matched to a known Subject by name at ingest time
  status          ContentStatus
  rejectionReason Text Maybe
  createdAt       UTCTime
  updatedAt       UTCTime
  UniqueContentUrl url
  deriving Show Eq

-- | A blog post draft generated from one or more RawContent items.
-- The actual Markdown body lives in the Git branch; only metadata is stored here.
PostDraft
  title            Text
  gitBranch        Text       -- e.g. "draft/haskell-everyday-2025-02-24"
  subjectId        SubjectId Maybe
  suggestedTags    TagList    -- PostgreSQL text[]
  status           DraftStatus
  discordThreadId  Text Maybe -- Forum thread ID; set when the bot creates the review thread
  publishedAt      UTCTime Maybe
  publishedUrl     Text Maybe
  createdAt        UTCTime
  updatedAt        UTCTime
  UniqueGitBranch gitBranch
  deriving Show Eq

-- | Join table linking a PostDraft to the RawContent items it was built from.
PostDraftSource
  postDraftId   PostDraftId
  rawContentId  RawContentId
  UniquePostDraftSource postDraftId rawContentId
  deriving Show Eq

-- | A message in the Discord review forum thread for a PostDraft.
ReviewComment
  postDraftId PostDraftId
  author      CommentAuthor
  message     Text
  createdAt   UTCTime
  deriving Show Eq

-- | AI analysis produced for a PostDraft iteration.
AiAnalysis
  postDraftId    PostDraftId
  summary        Text
  tokensUsed     Int
  analyzedAt     UTCTime
  deriving Show Eq
|]

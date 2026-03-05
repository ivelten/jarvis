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
import Orchestrator.Database.Models (CommentAuthor, ContentStatus, DraftStatus, InterestScore, TagList)

-- | All database entities for the orchestrator.
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

-- | A curated subject of interest for the blog.
-- The interest score controls how eagerly new content on this subject is drafted.
Subject
  name          Text
  description   Text Maybe
  interestScore InterestScore
  createdAt     UTCTime
  updatedAt     UTCTime
  UniqueSubjectName name
  deriving Show Eq

-- | A piece of web content discovered by the AI as a potential blog source.
RawContent
  title           Text
  url             Text
  summary         Text
  subjectId       SubjectId Maybe
  status          ContentStatus
  rejectionReason Text Maybe
  createdAt       UTCTime
  updatedAt       UTCTime
  UniqueContentUrl url
  deriving Show Eq

-- | A blog post draft generated from one or more RawContent items.
PostDraft
  title            Text
  gitBranch        Text
  subjectId        SubjectId Maybe
  suggestedTags    TagList
  status           DraftStatus
  discordThreadId  Text Maybe
  contentMarkdown  Text Maybe
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

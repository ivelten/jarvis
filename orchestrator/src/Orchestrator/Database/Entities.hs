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
  status          ContentStatus
  createdAt       UTCTime
  updatedAt       UTCTime
  UniqueContentUrl url
  deriving Show Eq

-- | Join table: a RawContent item may belong to many subjects.
RawContentSubject
  rawContentId RawContentId
  subjectId    SubjectId
  UniqueRawContentSubject rawContentId subjectId
  deriving Show Eq

-- | A blog post draft generated from one or more RawContent items.
PostDraft
  title            Text
  gitBranch        Text
  suggestedTags    TagList
  status           DraftStatus
  discordThreadId  Text Maybe
  contentMarkdownEn    Text Maybe
  contentMarkdownPtBr  Text Maybe
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

-- | Join table: a PostDraft may cover many subjects.
PostDraftSubject
  postDraftId PostDraftId
  subjectId   SubjectId
  UniquePostDraftSubject postDraftId subjectId
  deriving Show Eq

-- | A message in the Discord review forum thread for a PostDraft.
ReviewComment
  postDraftId PostDraftId
  author      CommentAuthor
  message     Text
  createdAt   UTCTime
  deriving Show Eq

-- | AI analysis produced for a PostDraft creation or revision iteration.
DraftAiAnalysis
  postDraftId    PostDraftId
  summary        Text
  tokensUsed     Int
  analyzedAt     UTCTime
  deriving Show Eq

-- | Telemetry record for a single content-discovery run.
ContentSearchAiAnalysis
  totalItemsFound  Int
  itemsIngested    Int
  tokensUsed       Int
  searchedAt       UTCTime
  deriving Show Eq
|]

{-# LANGUAGE TemplateHaskell #-}

module Orchestrator.Database.Migrations
  ( createEnumTypes,
    createConstraints,
    createTriggers,
    createIndexes,
  )
where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.Text (pack)
import Database.Persist.Sql (SqlPersistT, rawExecute)

-- | Create PostgreSQL enum types if they do not already exist.
--
-- SQL is embedded at compile-time from the @sql/types/@ directory.
-- Must be called before 'migrateAll' so the types are available when
-- table DDL referencing them is executed.
createEnumTypes :: SqlPersistT IO ()
createEnumTypes = do
  rawExecute (pack $(makeRelativeToProject "sql/types/content_status.sql" >>= embedStringFile)) []
  rawExecute (pack $(makeRelativeToProject "sql/types/draft_status.sql" >>= embedStringFile)) []
  rawExecute (pack $(makeRelativeToProject "sql/types/comment_author.sql" >>= embedStringFile)) []

-- | Add database constraints that require tables to already exist.
--
-- Must be called after 'migrateAll'. Safe to call multiple times.
createConstraints :: SqlPersistT IO ()
createConstraints =
  rawExecute (pack $(makeRelativeToProject "sql/constraints/subject.sql" >>= embedStringFile)) []

-- | Install @BEFORE UPDATE@ triggers that maintain @updated_at@, preserve
-- @created_at@, and guard @analyzed_at@ immutability.
--
-- Must be called after 'migrateAll'. Safe to call multiple times.
createTriggers :: SqlPersistT IO ()
createTriggers = do
  rawExecute (pack $(makeRelativeToProject "sql/triggers/subject.sql" >>= embedStringFile)) []
  rawExecute (pack $(makeRelativeToProject "sql/triggers/raw_content.sql" >>= embedStringFile)) []
  rawExecute (pack $(makeRelativeToProject "sql/triggers/post_draft.sql" >>= embedStringFile)) []
  rawExecute (pack $(makeRelativeToProject "sql/triggers/review_comment.sql" >>= embedStringFile)) []
  rawExecute (pack $(makeRelativeToProject "sql/triggers/ai_analysis.sql" >>= embedStringFile)) []

-- | Create indexes on status columns, FK columns, and the Discord thread ID.
--
-- Must be called after 'migrateAll'. Safe to call multiple times.
createIndexes :: SqlPersistT IO ()
createIndexes = do
  rawExecute (pack $(makeRelativeToProject "sql/indexes/raw_content.sql" >>= embedStringFile)) []
  rawExecute (pack $(makeRelativeToProject "sql/indexes/post_draft.sql" >>= embedStringFile)) []
  rawExecute (pack $(makeRelativeToProject "sql/indexes/post_draft_source.sql" >>= embedStringFile)) []
  rawExecute (pack $(makeRelativeToProject "sql/indexes/review_comment.sql" >>= embedStringFile)) []
  rawExecute (pack $(makeRelativeToProject "sql/indexes/ai_analysis.sql" >>= embedStringFile)) []

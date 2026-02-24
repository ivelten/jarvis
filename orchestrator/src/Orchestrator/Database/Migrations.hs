{-# LANGUAGE TemplateHaskell #-}

module Orchestrator.Database.Migrations
  ( createEnumTypes,
    createConstraints,
    createTriggers,
    createIndexes,
  )
where

import Database.Persist.Sql (SqlPersistT)
import Orchestrator.Database.TH (executeSqlFile)

-- | Create PostgreSQL enum types if they do not already exist.
--
-- SQL is embedded at compile-time from the @sql/types/@ directory.
-- Must be called before 'migrateAll' so the types are available when
-- table DDL referencing them is executed.
createEnumTypes :: SqlPersistT IO ()
createEnumTypes = do
  $(executeSqlFile "sql/types/content_status.sql")
  $(executeSqlFile "sql/types/draft_status.sql")
  $(executeSqlFile "sql/types/comment_author.sql")

-- | Add database constraints that require tables to already exist.
--
-- Must be called after 'migrateAll'. Safe to call multiple times.
createConstraints :: SqlPersistT IO ()
createConstraints =
  $(executeSqlFile "sql/constraints/subject.sql")

-- | Install @BEFORE UPDATE@ triggers that maintain @updated_at@, preserve
-- @created_at@, and guard @analyzed_at@ immutability.
--
-- Must be called after 'migrateAll'. Safe to call multiple times.
createTriggers :: SqlPersistT IO ()
createTriggers = do
  $(executeSqlFile "sql/triggers/subject.sql")
  $(executeSqlFile "sql/triggers/raw_content.sql")
  $(executeSqlFile "sql/triggers/post_draft.sql")
  $(executeSqlFile "sql/triggers/review_comment.sql")
  $(executeSqlFile "sql/triggers/ai_analysis.sql")

-- | Create indexes on status columns, FK columns, and the Discord thread ID.
--
-- Must be called after 'migrateAll'. Safe to call multiple times.
createIndexes :: SqlPersistT IO ()
createIndexes = do
  $(executeSqlFile "sql/indexes/raw_content.sql")
  $(executeSqlFile "sql/indexes/post_draft.sql")
  $(executeSqlFile "sql/indexes/post_draft_source.sql")
  $(executeSqlFile "sql/indexes/review_comment.sql")
  $(executeSqlFile "sql/indexes/ai_analysis.sql")

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
-- SQL is embedded at compile-time from the @sql/@ directory.
-- Must be called before 'migrateAll' so the types are available when
-- table DDL referencing them is executed.
createEnumTypes :: SqlPersistT IO ()
createEnumTypes = do
  rawExecute (pack $(makeRelativeToProject "sql/create_content_status.sql" >>= embedStringFile)) []
  rawExecute (pack $(makeRelativeToProject "sql/create_draft_status.sql" >>= embedStringFile)) []
  rawExecute (pack $(makeRelativeToProject "sql/create_comment_author.sql" >>= embedStringFile)) []

-- | Add database constraints that require tables to already exist.
--
-- Must be called after 'migrateAll'. Safe to call multiple times.
createConstraints :: SqlPersistT IO ()
createConstraints =
  rawExecute (pack $(makeRelativeToProject "sql/add_interest_score_check.sql" >>= embedStringFile)) []

-- | Install @BEFORE UPDATE@ triggers that keep @updated_at@ current automatically.
--
-- Must be called after 'migrateAll'. Safe to call multiple times.
createTriggers :: SqlPersistT IO ()
createTriggers =
  rawExecute (pack $(makeRelativeToProject "sql/create_updated_at_trigger.sql" >>= embedStringFile)) []

-- | Create indexes on status columns used in common queries.
--
-- Must be called after 'migrateAll'. Safe to call multiple times.
createIndexes :: SqlPersistT IO ()
createIndexes =
  rawExecute (pack $(makeRelativeToProject "sql/create_status_indexes.sql" >>= embedStringFile)) []

{-# LANGUAGE TemplateHaskell #-}

module Orchestrator.Database.Migrations
  ( createEnumTypes,
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

-- | Shared helpers for the orchestrator test suite.
--
-- Prerequisites:
--   The 'jarvis_test' database must exist:
--     psql -h db -U postgres -c "CREATE DATABASE jarvis_test;"
module TestHelpers
  ( testConnStr,
    setupTestPool,
    truncateTestTables,
  )
where

import Control.Monad.Logger (runNoLoggingT)
import Data.ByteString (ByteString)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (rawExecute)
import Orchestrator.Database.Connection (DbPool, migrateDatabase, runDb)

-- | Connection string for the isolated integration-test database.
testConnStr :: ByteString
testConnStr = "host=db port=5432 user=postgres password=postgres dbname=jarvis_test"

-- | Create a pool against the test database and apply all migrations.
--
-- Safe to call multiple times; both enum-type creation and 'migrateAll' are
-- idempotent.
setupTestPool :: IO DbPool
setupTestPool = do
  pool <- runNoLoggingT $ createPostgresqlPool testConnStr 1
  migrateDatabase pool
  return pool

-- | Remove all rows from every data table and reset sequences.
--
-- Call this in a 'before_' block to give each test group a clean slate.
truncateTestTables :: DbPool -> IO ()
truncateTestTables pool =
  runDb pool $
    rawExecute
      "TRUNCATE ai_analysis, review_comment, post_draft_source, \
      \post_draft, raw_content, subject RESTART IDENTITY CASCADE"
      []

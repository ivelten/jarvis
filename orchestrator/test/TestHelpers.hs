-- | Shared helpers for the orchestrator test suite.
--
-- Prerequisites:
--   The 'jarvis_test' database must exist:
--     psql -h db -U postgres -c "CREATE DATABASE jarvis_test;"
module TestHelpers
  ( testConnStr,
    setupTestPool,
    truncateTestTables,
    createTestDatabase,
    dropTestDatabase,
  )
where

import Control.Monad.Logger (runNoLoggingT)
import Data.ByteString (ByteString)
import Database.Persist.Postgresql (createPostgresqlPool)
import Database.Persist.Sql (rawExecute)
import Database.PostgreSQL.Simple (close, connectPostgreSQL, execute_)
import Orchestrator.Database.Connection (DbPool, migrateDatabase, runDb)

-- ---------------------------------------------------------------------------
-- Connection strings
-- ---------------------------------------------------------------------------

-- | Connection string targeting the PostgreSQL maintenance database,
-- used to create and drop 'jarvis_test'.
adminConnStr :: ByteString
adminConnStr = "host=db port=5432 user=postgres password=postgres dbname=postgres"

-- | Connection string for the isolated integration-test database.
testConnStr :: ByteString
testConnStr = "host=db port=5432 user=postgres password=postgres dbname=jarvis_test"

-- ---------------------------------------------------------------------------
-- Database lifecycle
-- ---------------------------------------------------------------------------

-- | Drop (if it exists) and re-create the 'jarvis_test' database.
--
-- Must be called before any connections are made to 'jarvis_test'.
-- Uses the @postgres@ maintenance database so the command runs outside
-- any transaction (PostgreSQL requires DDL on databases to be issued
-- outside a transaction block).
createTestDatabase :: IO ()
createTestDatabase = do
  conn <- connectPostgreSQL adminConnStr
  _ <- execute_ conn "DROP DATABASE IF EXISTS jarvis_test"
  _ <- execute_ conn "CREATE DATABASE jarvis_test"
  close conn

-- | Drop the 'jarvis_test' database unconditionally.
--
-- Called automatically by the test suite after all specs have run.
dropTestDatabase :: IO ()
dropTestDatabase = do
  conn <- connectPostgreSQL adminConnStr
  _ <- execute_ conn "DROP DATABASE IF EXISTS jarvis_test"
  close conn

-- ---------------------------------------------------------------------------
-- Pool setup
-- ---------------------------------------------------------------------------

-- | Create a pool against the test database and apply all migrations.
--
-- Safe to call multiple times; both enum-type creation and 'migrateAll' are
-- idempotent.
setupTestPool :: IO DbPool
setupTestPool = do
  pool <- runNoLoggingT $ createPostgresqlPool testConnStr 1
  migrateDatabase pool
  return pool

-- ---------------------------------------------------------------------------
-- Data helpers
-- ---------------------------------------------------------------------------

-- | Remove all rows from every data table and reset sequences.
--
-- Call this in a 'before_' block to give each test group a clean slate.
truncateTestTables :: DbPool -> IO ()
truncateTestTables pool =
  runDb pool $
    rawExecute
      "TRUNCATE content_search_ai_analysis, draft_ai_analysis, review_comment, post_draft_subject, post_draft_source, \
      \post_draft, raw_content_subject, raw_content, subject RESTART IDENTITY CASCADE"
      []

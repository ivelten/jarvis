module Orchestrator.Database.Connection
  ( DbPool,
    createPool,
    runDb,
    withDb,
    migrateDatabase,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (ReaderT)
import qualified Data.Pool as Pool
import Database.Persist.Postgresql
  ( ConnectionString,
    SqlBackend,
    runMigration,
    runSqlPool,
  )
import qualified Database.Persist.Postgresql as PG
import Database.Persist.Sql (SqlPersistT)
import Orchestrator.Database.Entities (migrateAll)
import Orchestrator.Database.Migrations (createEnumTypes)

-- | A pool of PostgreSQL connections.
type DbPool = Pool.Pool SqlBackend

-- | Create a connection pool from a PostgreSQL connection string.
--
-- Example connection string:
-- @"host=localhost port=5432 dbname=jarvis user=jarvis password=secret"@
createPool :: ConnectionString -> IO DbPool
createPool connStr =
  runStdoutLoggingT $
    PG.createPostgresqlPool connStr 10

-- | Run a database action against the pool.
runDb :: (MonadIO m) => DbPool -> SqlPersistT IO a -> m a
runDb pool action = liftIO $ runSqlPool action pool

-- | Alias kept for symmetry with other frameworks.
withDb :: (MonadIO m) => DbPool -> SqlPersistT IO a -> m a
withDb = runDb

-- | Apply all pending migrations.
--
-- Creates PostgreSQL enum types first, then runs the schema migration.
migrateDatabase :: DbPool -> IO ()
migrateDatabase pool = runDb pool $ do
  createEnumTypes
  runMigration migrateAll

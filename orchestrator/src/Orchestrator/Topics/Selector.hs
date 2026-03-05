module Orchestrator.Topics.Selector
  ( ingestDiscoveredContent,
    ingestContent,
    pendingContent,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime)
import Database.Persist.Sql
  ( Entity (..),
    SqlPersistT,
    getBy,
    selectList,
    upsertBy,
    (=.),
    (==.),
  )
import Orchestrator.AI.Client (AiConfig, DiscoveredContent (..), discoverContent)
import Orchestrator.Database.Entities
import Orchestrator.Database.Models

-- | Ask the AI to search the web and persist any newly discovered content.
ingestDiscoveredContent :: AiConfig -> SqlPersistT IO ()
ingestDiscoveredContent aiCfg = do
  discovered <- liftIO $ discoverContent aiCfg
  ingestContent discovered

-- | Persist a list of already-discovered content items (upsert by URL).
-- Triage fields (@status@, @rejectionReason@) are never overwritten on
-- conflict, so human decisions are preserved.
-- This is separated from 'ingestDiscoveredContent' to allow testing without
-- a real AI call.
ingestContent :: [DiscoveredContent] -> SqlPersistT IO ()
ingestContent discovered = do
  now <- liftIO getCurrentTime
  rows <- mapM (toRawContent now) discovered
  mapM_ upsertRow rows
  where
    upsertRow rc =
      upsertBy
        (UniqueContentUrl (rawContentUrl rc))
        rc
        [ RawContentTitle =. rawContentTitle rc,
          RawContentSummary =. rawContentSummary rc,
          RawContentSubjectId =. rawContentSubjectId rc,
          RawContentUpdatedAt =. rawContentUpdatedAt rc
        ]
    toRawContent now dc = do
      mSubject <- case dcSubject dc of
        Nothing -> pure Nothing
        Just name -> fmap entityKey <$> getBy (UniqueSubjectName name)
      pure
        RawContent
          { rawContentTitle = dcTitle dc,
            rawContentUrl = dcUrl dc,
            rawContentSummary = dcSummary dc,
            rawContentSubjectId = mSubject,
            rawContentStatus = ContentNew,
            rawContentRejectionReason = Nothing,
            rawContentCreatedAt = now,
            rawContentUpdatedAt = now
          }

-- | Return all content items that have not yet been triaged.
pendingContent :: SqlPersistT IO [Entity RawContent]
pendingContent = selectList [RawContentStatus ==. ContentNew] []

module Orchestrator.Topics.Selector
  ( ingestDiscoveredContent,
    ingestContent,
    recordDiscovery,
    pendingContent,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes)
import Data.Time (getCurrentTime)
import Database.Persist.Sql
  ( Entity (..),
    SqlPersistT,
    getBy,
    insertUnique,
    insert_,
    selectList,
    upsertBy,
    (=.),
    (==.),
  )
import Orchestrator.AI.Client (AiConfig, DiscoveredContent (..), discoverContent)
import Orchestrator.Database.Entities
import Orchestrator.Database.Models

-- | Ask the AI to search the web and persist any newly discovered content.
-- Subject names are read from the database at call time, so the discovery
-- search scope always matches what is in the Subject table.
ingestDiscoveredContent :: AiConfig -> SqlPersistT IO ()
ingestDiscoveredContent aiCfg = do
  subjects <- selectList [] []
  let names = map (subjectName . entityVal) (subjects :: [Entity Subject])
  (tokensUsed, discovered) <- liftIO $ discoverContent aiCfg names
  recordDiscovery tokensUsed discovered

-- | Persist a list of discovered content items and record a 'ContentSearchAiAnalysis'
-- telemetry row.  Separated from 'ingestDiscoveredContent' so it can be tested
-- without a real AI call.
recordDiscovery :: Int -> [DiscoveredContent] -> SqlPersistT IO ()
recordDiscovery tokensUsed discovered = do
  ingestContent discovered
  now <- liftIO getCurrentTime
  insert_
    ContentSearchAiAnalysis
      { contentSearchAiAnalysisTotalItemsFound = length discovered,
        contentSearchAiAnalysisItemsIngested = length discovered,
        contentSearchAiAnalysisTokensUsed = tokensUsed,
        contentSearchAiAnalysisSearchedAt = now
      }

-- | Persist a list of already-discovered content items (upsert by URL).
-- Triage fields (@status@, @rejectionReason@) are never overwritten on
-- conflict, so human decisions are preserved.
-- Subject associations are additive on re-ingest: new links are inserted but
-- existing associations are never removed.
-- This is separated from 'ingestDiscoveredContent' to allow testing without
-- a real AI call.
ingestContent :: [DiscoveredContent] -> SqlPersistT IO ()
ingestContent = mapM_ ingestOne
  where
    ingestOne dc = do
      now <- liftIO getCurrentTime
      rcEntity <-
        upsertBy
          (UniqueContentUrl (dcUrl dc))
          RawContent
            { rawContentTitle = dcTitle dc,
              rawContentUrl = dcUrl dc,
              rawContentSummary = dcSummary dc,
              rawContentStatus = ContentNew,
              rawContentCreatedAt = now,
              rawContentUpdatedAt = now
            }
          [ RawContentTitle =. dcTitle dc,
            RawContentSummary =. dcSummary dc,
            RawContentUpdatedAt =. now
          ]
      linkSubjects (entityKey rcEntity) (dcSubjects dc)

    linkSubjects rcKey names = do
      subjectKeys <- catMaybes <$> mapM lookupSubject names
      mapM_ (insertUnique . mkLink rcKey) subjectKeys

    mkLink rcKey sid =
      RawContentSubject
        { rawContentSubjectRawContentId = rcKey,
          rawContentSubjectSubjectId = sid
        }

    lookupSubject name = fmap entityKey <$> getBy (UniqueSubjectName name)

-- | Return all content items that have not yet been triaged.
pendingContent :: SqlPersistT IO [Entity RawContent]
pendingContent = selectList [RawContentStatus ==. ContentNew] []

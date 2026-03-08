module Orchestrator.Topics.Selector
  ( ingestDiscoveredContent,
    ingestContent,
    recordDiscovery,
    topSubjects,
    pendingContent,
    DiscoveryStats (..),
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes)
import Data.Time (getCurrentTime)
import Database.Persist.Sql
  ( Entity (..),
    SelectOpt (..),
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

-- | Statistics returned by a content-discovery run.
data DiscoveryStats = DiscoveryStats
  { -- | Total items returned by the AI.
    dsItemsFound :: !Int,
    -- | Total items persisted (inserted or upserted).
    dsItemsIngested :: !Int,
    -- | Tokens consumed by the AI call.
    dsTokensUsed :: !Int
  }
  deriving (Show, Eq)

-- | Ask the AI to search the web and persist any newly discovered content.
-- Subjects are read from the database at call time, sorted by interest score
-- descending, capped at the top 10, so higher-priority subjects always get
-- included in the discovery prompt.
ingestDiscoveredContent :: AiConfig -> SqlPersistT IO DiscoveryStats
ingestDiscoveredContent aiCfg = do
  subjects <- topSubjects
  let names = map (subjectName . entityVal) subjects
  (tokensUsed, discovered) <- liftIO $ discoverContent aiCfg names
  recordDiscovery tokensUsed discovered

-- | Return up to 10 subjects ordered by interest score descending.
-- This is the set passed to the AI discovery prompt on each run.
topSubjects :: SqlPersistT IO [Entity Subject]
topSubjects = selectList [] [Desc SubjectInterestScore, LimitTo 10]

-- | Persist a list of discovered content items and record a 'ContentSearchAiAnalysis'
-- telemetry row.  Separated from 'ingestDiscoveredContent' so it can be tested
-- without a real AI call.
recordDiscovery :: Int -> [DiscoveredContent] -> SqlPersistT IO DiscoveryStats
recordDiscovery tokensUsed discovered = do
  ingestContent discovered
  now <- liftIO getCurrentTime
  let found = length discovered
  insert_
    ContentSearchAiAnalysis
      { contentSearchAiAnalysisTotalItemsFound = found,
        contentSearchAiAnalysisItemsIngested = found,
        contentSearchAiAnalysisTokensUsed = tokensUsed,
        contentSearchAiAnalysisSearchedAt = now
      }
  pure DiscoveryStats {dsItemsFound = found, dsItemsIngested = found, dsTokensUsed = tokensUsed}

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

{-# LANGUAGE RecordWildCards #-}

module Convert.EntryTree where

import Convert.HashMap
import Convert.Logbook
import Convert.Time

import Smos.Data.Types

import Data.OrgMode.Types

import Data.Time.Clock

toEntryTree :: Headline -> IO (Tree Entry)
toEntryTree headline@Headline {..} = do
    logbook <- getLogbook $ sectionLogbook section
    timestamps <- getTimestamps $ sectionPlannings section
    history <- stateHistory headline
    let entry =
            Entry
            { entryHeader = Header title
            , entryContents = Just . Contents $ sectionParagraph section
            , entryTimestamps = timestamps
            , entryProperties = getProperties $ sectionProperties section
            , entryStateHistory = history
            , entryTags = Tag <$> tags
            , entryLogbook = logbook
            }
    forrest <- sequence $ toEntryTree <$> subHeadlines
    pure $ Node entry forrest

stateHistory :: Headline -> IO StateHistory
stateHistory Headline {..} = do
    time <-
        returnTimeIfPresent timestamp $
        returnTimeIfPresent (sectionTimestamp section) getCurrentTime
    pure $
        StateHistory
            [ StateHistoryEntry
              { stateHistoryEntryNewState = toState <$> stateKeyword
              , stateHistoryEntryTimestamp = time
              }
            ]

returnTimeIfPresent ::
       Maybe Data.OrgMode.Types.Timestamp -> IO UTCTime -> IO UTCTime
returnTimeIfPresent mStamp dflt = maybe dflt (toUTCTime . tsTime) mStamp

toState :: StateKeyword -> TodoState
toState (StateKeyword text) = TodoState text

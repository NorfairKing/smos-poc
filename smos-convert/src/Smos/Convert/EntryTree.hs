{-# LANGUAGE RecordWildCards #-}

module Smos.Convert.EntryTree where

import Import

import Smos.Convert.HashMap
import Smos.Convert.Logbook
import Smos.Convert.Time

import Smos.Data.Types

import Data.OrgMode.Types

import Data.Time.Clock
import Data.Time.LocalTime

toEntryTree :: TimeZone -> UTCTime -> Headline -> Tree Entry
toEntryTree timezone now headline@Headline {..} =
    let entry =
            Entry
            { entryHeader = Header title
            , entryContents = Just . Contents $ sectionParagraph section
            , entryTimestamps =
                  getTimestamps timezone $ sectionPlannings section
            , entryProperties = getProperties $ sectionProperties section
            , entryStateHistory = stateHistory timezone now headline
            , entryTags = Tag <$> tags
            , entryLogbook = getLogbook timezone $ sectionLogbook section
            }
        forest = toEntryTree timezone now <$> subHeadlines
    in Node entry forest

stateHistory :: TimeZone -> UTCTime -> Headline -> StateHistory
stateHistory timezone now Headline {..} =
    StateHistory
        [ StateHistoryEntry
          { stateHistoryEntryNewState = toState <$> stateKeyword
          , stateHistoryEntryTimestamp =
                toUTC timestamp $ toUTC (sectionTimestamp section) now
          }
        ]
  where
    toUTC maybeStamp stamp2 =
        fromMaybe stamp2 $ toUTCTime timezone . tsTime <$> maybeStamp

toState :: StateKeyword -> TodoState
toState (StateKeyword text) = TodoState text

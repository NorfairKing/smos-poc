{-# LANGUAGE RecordWildCards #-}

module Convert.EntryTree where

import Convert.Logbook

import Smos.Data.Types

import qualified Data.HashMap.Lazy as HM
import Data.OrgMode.Types

toEntryTree :: Headline -> Tree Entry
toEntryTree Headline {..} =
    let entry =
            Entry
            { entryHeader = Header title
            , entryContents = Just . Contents $ sectionParagraph section
            , entryTimestamps = HM.empty
            , entryProperties = HM.empty
            , entryStateHistory = StateHistory []
            , entryTags = Tag <$> tags
            , entryLogbook = getLogbook $ sectionLogbook section
            }
        forrest = toEntryTree <$> subHeadlines
    in Node entry forrest

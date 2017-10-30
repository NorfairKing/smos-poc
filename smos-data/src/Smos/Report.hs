{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report
    ( EntryReport(..)
    , entryReport
    , stateIs
    , hasTag
    , (&&&)
    , (|||)
    ) where

import Import

import Data.Yaml

import Smos.Data

-- | A report of entries, made by filtering using an element-wise predicate
newtype EntryReport = EntryReport
    { entryReportEntries :: [(Path Abs File, [Entry])]
    } deriving (Show, Eq, Generic)

instance Validity EntryReport

instance FromJSON EntryReport where
    parseJSON = withObject "EntryReport" $ \o -> EntryReport <$> o .: "entries"

instance ToJSON EntryReport where
    toJSON EntryReport {..} = object ["entries" .= entryReportEntries]

entryReport :: [(Path Abs File, SmosFile)] -> (Entry -> Bool) -> EntryReport
entryReport fs p =
    EntryReport {entryReportEntries = [(f, go sf) | (f, sf) <- fs]}
  where
    go :: SmosFile -> [Entry]
    go SmosFile {..} = gof smosFileForest
    gof SmosForest {..} = concatMap got smosTrees
    got SmosTree {..} = [treeEntry | p treeEntry]

stateIs :: TodoState -> Entry -> Bool
stateIs s e = entryState e == Just s

hasTag :: Tag -> Entry -> Bool
hasTag t e = t `elem` entryTags e

(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) p1 p2 a = p1 a && p2 a

(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) p1 p2 a = p1 a || p2 a

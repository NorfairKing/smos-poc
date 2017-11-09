{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Entry
    ( EntryReport
    , entryReport
    , prettyEntryReport
    ) where

import Import

import Data.Tree

import Smos.Data

import Smos.Report.Entry.Pretty
import Smos.Report.Entry.Types

entryReport :: (Entry -> Bool) -> [(Path Abs File, SmosFile)] -> EntryReport
entryReport p fs =
    EntryReport {entryReportEntries = [(f, go sf) | (f, sf) <- fs]}
  where
    go :: SmosFile -> [Entry]
    go SmosFile {..} = gof smosFileForest
    gof = concatMap got
    got Node {..} = [rootLabel | p rootLabel]

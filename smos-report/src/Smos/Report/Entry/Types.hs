{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Entry.Types
    ( EntryReport(..)
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

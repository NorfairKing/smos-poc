{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Convert.SmosFile where

import Smos.Convert.EntryTree

import Smos.Data.Types

import Data.OrgMode.Types

import Data.Time.Clock
import Data.Time.LocalTime

toSmosFile :: Document -> IO SmosFile
toSmosFile Document {..} = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let forest = toEntryTree timezone now <$> documentHeadlines
    SmosFile <$>
        case documentText of
            "" -> pure forest
            text ->
                let tree = Node (newEntry $ Header text) []
                in pure $ tree : forest

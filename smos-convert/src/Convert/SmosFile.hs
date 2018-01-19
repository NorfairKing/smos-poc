{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Convert.SmosFile where

import Convert.EntryTree

import Smos.Data.Types

import Data.OrgMode.Types

toSmosFile :: Document -> IO SmosFile
toSmosFile Document {..} = do
    forrest <- sequence $ toEntryTree <$> documentHeadlines
    SmosFile <$>
        case documentText of
            "" -> pure forrest
            text ->
                let tree = Node (newEntry $ Header text) []
                in pure $ tree : forrest

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Convert.SmosFile where

import Convert.EntryTree

import Smos.Data.Types

import Data.OrgMode.Types

toSmosFile :: Document -> SmosFile
toSmosFile Document {..} =
    let forrest = toEntryTree <$> documentHeadlines
    in SmosFile $
       case documentText of
           "" -> forrest
           text ->
               let tree = Node (newEntry $ Header text) []
               in tree : forrest

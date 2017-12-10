{-# LANGUAGE TypeApplications #-}

module Smos.Data.TypesSpec
    ( spec
    ) where

import TestImport

import Smos.Data.Gen ()
import Smos.Data.Types

spec :: Spec
spec = do
    eqSpec @Contents
    genValidSpec @Contents
    jsonSpecOnValid @Contents
    eqSpec @TimestampName
    genValidSpec @TimestampName
    jsonSpecOnValid @TimestampName
    eqSpec @Timestamp
    genValidSpec @Timestamp
    jsonSpecOnValid @Timestamp
    eqSpec @TodoState
    genValidSpec @TodoState
    jsonSpecOnValid @TodoState
    eqSpec @StateHistory
    genValidSpec @StateHistory
    jsonSpecOnValid @StateHistory
    eqSpec @StateHistoryEntry
    genValidSpec @StateHistoryEntry
    jsonSpecOnValid @StateHistoryEntry
    eqSpec @Tag
    genValidSpec @Tag
    jsonSpecOnValid @Tag
    eqSpec @Logbook
    genValidSpec @Logbook
    jsonSpecOnValid @Logbook
    eqSpec @LogbookEntry
    genValidSpec @LogbookEntry
    jsonSpecOnValid @LogbookEntry
    eqSpec @Entry
    genValidSpec @Entry
    jsonSpecOnValid @Entry
    eqSpec @(ForYaml (Tree Entry))
    genValidSpec @(ForYaml (Tree Entry))
    jsonSpecOnValid @(ForYaml (Tree Entry))
    eqSpec @(ForYaml (Forest Entry))
    genValidSpec @(ForYaml (Forest Entry))
    jsonSpecOnValid @(ForYaml (Forest Entry))
    eqSpec @SmosFile
    genValidSpec @SmosFile
    jsonSpecOnValid @SmosFile

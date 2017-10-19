{-# LANGUAGE TypeApplications #-}

module Smos.Data.TypesSpec
    ( spec
    ) where

import TestImport

import Smos.Data.Gen ()
import Smos.Data.Types

spec :: Spec
spec = do
    eqSpec @SmosFile
    genValidSpec @SmosFile
    jsonSpecOnValid @SmosFile
    eqSpec @SmosForest
    genValidSpec @SmosForest
    jsonSpecOnValid @SmosForest
    eqSpec @SmosTree
    genValidSpec @SmosTree
    jsonSpecOnValid @SmosTree
    eqSpec @Entry
    genValidSpec @Entry
    jsonSpecOnValid @Entry
    eqSpec @Contents
    genValidSpec @Contents
    jsonSpecOnValid @Contents
    eqSpec @TimestampName
    genValidSpec @TimestampName
    jsonSpecOnValid @TimestampName
    eqSpec @TodoState
    genValidSpec @TodoState
    jsonSpecOnValid @TodoState
    eqSpec @Tag
    genValidSpec @Tag
    jsonSpecOnValid @Tag
    eqSpec @Logbook
    genValidSpec @Logbook
    jsonSpecOnValid @Logbook

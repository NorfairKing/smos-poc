{-# LANGUAGE TypeApplications #-}

module Smos.Report.EntrySpec
    ( spec
    ) where

import TestImport

import Smos.Report.Entry
import Smos.Report.Entry.Gen ()

spec :: Spec
spec = do
    eqSpec @EntryReport
    genValidSpec @EntryReport
    jsonSpecOnValid @EntryReport
    describe "entryReport" $
        it "produces valid entries for the universal predicate" $
        producesValidsOnValids $ entryReport (const True)

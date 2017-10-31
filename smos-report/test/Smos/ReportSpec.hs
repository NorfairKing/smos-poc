{-# LANGUAGE TypeApplications #-}

module Smos.ReportSpec
    ( spec
    ) where

import TestImport

import Smos.Report
import Smos.Report.Gen ()

spec :: Spec
spec = do
    eqSpec @EntryReport
    genValidSpec @EntryReport
    jsonSpecOnValid @EntryReport

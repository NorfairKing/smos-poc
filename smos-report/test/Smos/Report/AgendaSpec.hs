{-# LANGUAGE TypeApplications #-}

module Smos.Report.AgendaSpec
    ( spec
    ) where

import TestImport

import Smos.Report.Agenda
import Smos.Report.Agenda.Gen ()

spec :: Spec
spec = do
    eqSpec @AgendaReport
    genValidSpec @AgendaReport
    jsonSpecOnValid @AgendaReport
    describe "agendaReport" $
        it "produces valid entries for the universal predicate" $
        producesValidsOnValids $ agendaReport (const True)

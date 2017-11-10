{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Agenda.Gen where

import TestImport

import Smos.Report.Agenda.Types

import Smos.Data.Gen ()
import Smos.Report.Entry.Gen ()

instance GenUnchecked AgendaReport

instance GenValid AgendaReport where
    genValid = AgendaReport <$> genValid

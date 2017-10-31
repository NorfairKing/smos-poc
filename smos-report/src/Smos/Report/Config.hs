{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.Config
    ( SmosReportConfig(..)
    ) where

import Import

import Smos.Data
import Text.PrettyPrint.ANSI.Leijen (Doc)

data SmosReportConfig = SmosReportConfig
    { reportConfigAgendaFiles :: IO [Path Abs File]
    , reportConfigReports :: [(String, [(Path Abs File, SmosFile)] -> Doc)]
    } deriving (Generic)

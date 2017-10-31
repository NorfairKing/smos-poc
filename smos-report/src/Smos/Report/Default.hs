{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Default
    ( defaultSmosReport
    , defaultReportConfig
    ) where

import Import

import Smos.Report
import Smos.Report.Config
import Smos.Report.Entry.Pretty

defaultSmosReport :: IO ()
defaultSmosReport = pure ()

defaultReportConfig :: SmosReportConfig
defaultReportConfig =
    SmosReportConfig
    { configAgendaFiles =
          do home <- getHomeDir
             d <- resolveDir home "smos"
             fromMaybe [] <$> forgivingAbsence (snd <$> listDirRecur d)
    , configReports =
          [("todo", prettyEntryReport . entryReport (stateIs "TODO"))]
    }

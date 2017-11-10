{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Default
    ( defaultSmosReport
    , defaultReportConfig
    ) where

import Import

import Smos.Report
import Smos.Report.Config

defaultSmosReport :: IO ()
defaultSmosReport = smosReport defaultReportConfig

defaultReportConfig :: SmosReportConfig
defaultReportConfig =
    SmosReportConfig
    { reportConfigAgendaFiles =
          do home <- getHomeDir
             d <- resolveDir home "smos"
             fromMaybe [] <$> forgivingAbsence (snd <$> listDirRecur d)
    , reportConfigReports =
          [ ("todo", prettyEntryReport . entryReport (stateIs "TODO"))
          , ("next", prettyEntryReport . entryReport (stateIs "NEXT"))
          , ("done", prettyEntryReport . entryReport (stateIs "DONE"))
          , ("waiting", prettyEntryReport . entryReport (stateIs "WAITING"))
          , ("agenda", prettyAgendaReport . agendaReport (const True))
          ]
    }

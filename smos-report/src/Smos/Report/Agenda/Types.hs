{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Agenda.Types
    ( AgendaReport(..)
    ) where

import Import

import Data.Time
import Data.Yaml

import Smos.Data

-- | A report of an agenda
newtype AgendaReport = AgendaReport
    { agendaReportAgenda :: [(Day, [(Path Abs File, [Entry])])]
    } deriving (Show, Eq, Generic)

instance Validity AgendaReport

instance FromJSON AgendaReport where
    parseJSON = withObject "Agenda" $ \o -> AgendaReport <$> o .: "agenda"

instance ToJSON AgendaReport where
    toJSON AgendaReport {..} = object ["agenda" .= agendaReportAgenda]

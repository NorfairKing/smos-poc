{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Agenda
    ( AgendaReport
    , agendaReport
    , prettyAgendaReport
    ) where

import Import

import qualified Data.HashMap.Strict as HM
import Data.List (head)
import Data.Time

import Smos.Data

import Smos.Report.Agenda.Pretty
import Smos.Report.Agenda.Types

agendaReport :: (Entry -> Bool) -> [(Path Abs File, SmosFile)] -> AgendaReport
agendaReport pred_ fs =
    AgendaReport
    {agendaReportAgenda = transformTrips [(f, go sf) | (f, sf) <- fs]}
  where
    transformTrips ::
           [(Path Abs File, [(Day, Entry)])]
        -> [(Day, [(Path Abs File, [Entry])])]
    transformTrips tups = map (second groupSame) (groupSame trips)
      where
        trips = [(d, (p, e)) | (p, des) <- tups, (d, e) <- des]
    go :: SmosFile -> [(Day, Entry)]
    go SmosFile {..} = gof smosFileForest
    gof = concatMap got
    got Node {..} =
        [ ( case ts of
                TimestampDay d -> d
                TimestampTime ut -> utctDay ut
          , rootLabel)
        | pred_ rootLabel
        , (_, ts) <- HM.toList entryTimestamps
        ]
      where
        Entry {..} = rootLabel

groupSame :: Eq a => [(a, b)] -> [(a, [b])]
groupSame ts =
    let ls = groupBy ((==) `on` fst) ts
    in [(fst $ head l, map snd l) | l <- ls]

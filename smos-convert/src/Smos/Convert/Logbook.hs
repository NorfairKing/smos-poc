module Smos.Convert.Logbook where

import Import

import Smos.Convert.Time

import qualified Smos.Data.Types as Smos

import qualified Data.OrgMode.Types as Org

import Data.Time.Clock
import Data.Time.LocalTime

getLogbook :: TimeZone -> Org.Logbook -> Smos.Logbook
getLogbook timezone logbook =
    Smos.LogClosed . catMaybes $
    toLogbookEntry timezone <$> Org.unLogbook logbook

toLogbookEntry :: TimeZone -> Org.Clock -> Maybe Smos.LogbookEntry
toLogbookEntry timezone clock =
    case Org.unClock clock of
        (Nothing, _) -> Nothing
        (_, Nothing) -> Nothing
        (Just start, Just duration) ->
            let entryStart = toUTCTime timezone $ Org.tsTime start
                entryEnd = addUTCTime (toNominalDiffTime duration) entryStart
            in Just $ Smos.LogbookEntry entryStart entryEnd

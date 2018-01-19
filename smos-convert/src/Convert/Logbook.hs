module Convert.Logbook where

import Import

import Convert.Time

import qualified Smos.Data.Types as Smos

import qualified Data.OrgMode.Types as Org

import Data.Time.Clock

getLogbook :: Org.Logbook -> IO Smos.Logbook
getLogbook logbook =
    fmap Smos.LogClosed . sequence $ toLogbookEntry <$> Org.unLogbook logbook

toLogbookEntry :: Org.Clock -> IO Smos.LogbookEntry
toLogbookEntry clock = do
    let (maybeStart, maybeDuration) = Org.unClock clock
    logbookEntryStart <-
        case maybeStart of
            Nothing -> die "This is a clock without a start time"
            Just start -> toUTCTime $ Org.tsTime start
    let logbookEntryEnd =
            case maybeDuration of
                Nothing -> error "This is a clock without a duration"
                Just duration ->
                    addUTCTime (toNominalDiffTime duration) logbookEntryStart
    pure $ Smos.LogbookEntry logbookEntryStart logbookEntryEnd

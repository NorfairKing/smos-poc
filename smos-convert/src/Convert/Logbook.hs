module Convert.Logbook where

import qualified Smos.Data.Types as Smos

import qualified Data.OrgMode.Types as Org

import Data.Time.Clock

getLogbook :: Org.Logbook -> Smos.Logbook
getLogbook logbook = Smos.LogClosed $ toLogbookEntry <$> Org.unLogbook logbook

toLogbookEntry :: Org.Clock -> Smos.LogbookEntry
toLogbookEntry clock =
    let (start, duration) = Org.unClock clock
        logbookEntryStart =
            case start of
                Nothing -> error "This is a clock without a start time"
                Just time -> toUTCTime $ Org.tsTime time
        logbookEntryEnd =
            case duration of
                Nothing -> error "This is a clock without a duration"
                Just _ -> undefined
    in Smos.LogbookEntry logbookEntryStart logbookEntryEnd

toUTCTime :: Org.DateTime -> UTCTime
toUTCTime = undefined

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Convert.Time where

import Smos.Data.Types

import qualified Data.OrgMode.Types as Org

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

toUTCTime :: Org.DateTime -> IO UTCTime
toUTCTime datetime = do
    timezone <- getCurrentTimeZone
    let localtime = toLocalTime datetime
    pure $ localTimeToUTC timezone localtime

toLocalTime :: Org.DateTime -> LocalTime
toLocalTime datetime =
    LocalTime (toDay $ Org.yearMonthDay datetime) $ toLocalTimeOfDay datetime

toDay :: Org.YearMonthDay -> Day
toDay Org.YearMonthDay {..} = fromGregorian (toInteger ymdYear) ymdMonth ymdDay

toLocalTimeOfDay :: Org.DateTime -> TimeOfDay
toLocalTimeOfDay Org.DateTime {..} =
    case hourMinute of
        Nothing -> midnight
        Just (hour, minute) ->
            TimeOfDay {todHour = hour, todMin = minute, todSec = 0}

toNominalDiffTime :: Org.Duration -> NominalDiffTime
toNominalDiffTime (hours, minutes) =
    intToDiffTime hours * secInH + intToDiffTime minutes * secInMin

intToDiffTime :: Int -> NominalDiffTime
intToDiffTime = fromInteger . toInteger

secInH :: NominalDiffTime
secInH = 3600

secInMin :: NominalDiffTime
secInMin = 60

toTimestampName :: Org.PlanningKeyword -> TimestampName
toTimestampName Org.SCHEDULED = TimestampName "SCHEDULED"
toTimestampName Org.DEADLINE = TimestampName "DEADLINE"
toTimestampName Org.CLOSED = TimestampName "CLOSED"

toTimestamp :: TimeZone -> Org.Timestamp -> Timestamp
toTimestamp timezone Org.Timestamp {..} =
    case Org.hourMinute tsTime of
        Nothing -> TimestampDay . toDay $ Org.yearMonthDay tsTime
        Just _ -> TimestampTime . localTimeToUTC timezone $ toLocalTime tsTime

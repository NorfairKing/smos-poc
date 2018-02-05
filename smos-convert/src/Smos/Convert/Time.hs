{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Convert.Time where

import Smos.Data.Types

import qualified Data.OrgMode.Types as Org

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

toUTCTime :: TimeZone -> Org.DateTime -> UTCTime
toUTCTime timezone datetime = localTimeToUTC timezone $ toLocalTime datetime

toLocalTime :: Org.DateTime -> LocalTime
toLocalTime datetime =
    LocalTime (toDay $ Org.yearMonthDay datetime) $ toLocalTimeOfDay datetime

toDay :: Org.YearMonthDay -> Day
toDay Org.YearMonthDay {..} = fromGregorian (toInteger ymdYear) ymdMonth ymdDay

toLocalTimeOfDay :: Org.DateTime -> TimeOfDay
toLocalTimeOfDay Org.DateTime {..} =
    flip (maybe midnight) hourMinute $ \(h, m) -> TimeOfDay h m 0

toNominalDiffTime :: Org.Duration -> NominalDiffTime
toNominalDiffTime (h, m) = diffTime h * hour + diffTime m * minute

diffTime :: Int -> NominalDiffTime
diffTime = fromInteger . toInteger

hour :: NominalDiffTime
hour = 3600

minute :: NominalDiffTime
minute = 60

toTimestampName :: Org.PlanningKeyword -> TimestampName
toTimestampName Org.SCHEDULED = TimestampName "SCHEDULED"
toTimestampName Org.DEADLINE = TimestampName "DEADLINE"
toTimestampName Org.CLOSED = TimestampName "CLOSED"

toTimestamp :: TimeZone -> Org.Timestamp -> Timestamp
toTimestamp timezone Org.Timestamp {..} =
    case Org.hourMinute tsTime of
        Nothing -> TimestampDay . toDay $ Org.yearMonthDay tsTime
        Just _ -> TimestampTime . localTimeToUTC timezone $ toLocalTime tsTime

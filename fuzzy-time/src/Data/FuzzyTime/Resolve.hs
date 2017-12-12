module Data.FuzzyTime.Resolve
    ( resolveDateTime
    , resolveDay
    ) where

import Data.Time

import Data.FuzzyTime.FuzzyTypes

resolveDateTime :: ZonedTime -> FuzzyDateTime -> UTCTime
resolveDateTime zt DateTimeNow = zonedTimeToUTC zt

resolveDay :: Day -> FuzzyDay -> Day
resolveDay d fd =
    case fd of
        Yesterday -> addDays (-1) d
        Now -> d
        Today -> d
        Tomorrow -> addDays 1 d
        ExactDay d_ -> d_

module Data.FuzzyTime.Resolve
    ( resolveZonedTime
    , resolveDay
    ) where

import Data.Time

import Data.FuzzyTime.FuzzyTypes

resolveZonedTime :: ZonedTime -> FuzzyZonedTime -> ZonedTime
resolveZonedTime zt ZonedNow = zt

resolveDay :: Day -> FuzzyDay -> Day
resolveDay d fd =
    case fd of
        Yesterday -> addDays (-1) d
        Now -> d
        Today -> d
        Tomorrow -> addDays 1 d
        ExactDay d_ -> d_

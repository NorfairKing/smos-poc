{-# LANGUAGE DeriveGeneric #-}

module Data.FuzzyTime.FuzzyTypes
    ( FuzzyZonedTime(..)
    , FuzzyDay(..)
    , FuzzyDayOfTheWeek(..)
    ) where

import Import

import Data.Time

data FuzzyZonedTime =
    ZonedNow
    deriving (Show, Eq, Generic)

data FuzzyDay
    = Yesterday
    | Now
    | Today
    | Tomorrow
    | ExactDay Day
    deriving (Show, Eq, Generic)

instance Validity FuzzyDay

data FuzzyDayOfTheWeek
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    deriving (Show, Eq, Generic)

instance Validity FuzzyDayOfTheWeek

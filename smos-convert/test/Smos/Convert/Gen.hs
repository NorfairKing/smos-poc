{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Convert.Gen where

import TestImport

import Data.OrgMode.Types

import Data.GenValidity.Time.Calendar ()
import Data.GenValidity.Time.LocalTime ()

import Data.Thyme.LocalTime (Hour, Minute)
import Data.Time.Calendar
import Data.Time.LocalTime

instance Validity DateTime where
    isValid DateTime {..} =
        and
            [ isNothing repeater
            , isNothing delay
            , isNothing dayName
            , validHourMinute hourMinute
            , validDay yearMonthDay
            ]
    validate = validateByCheckingName "DateTime"

validHourMinute :: Maybe (Hour, Minute) -> Bool
validHourMinute Nothing = True
validHourMinute (Just (h, m)) = 0 <= h && h < 24 && 0 <= m && m < 60

validDay :: YearMonthDay -> Bool
validDay YearMonthDay {..} =
    isJust $ fromGregorianValid (toInteger ymdYear) ymdMonth ymdDay

valIfNothing :: Maybe a -> String -> Validation -> Validation
valIfNothing Nothing _ val = val
valIfNothing (Just _) s _ = Validation [Violated s]

genYMD :: Gen YearMonthDay
genYMD = do
    (y, m, d) <- toGregorian <$> genValid
    pure $ YearMonthDay (fromInteger y) m d

genHM :: Gen (Maybe (Hour, Minute))
genHM =
    oneof
        [ pure Nothing
        , do (TimeOfDay h m _) <- genValid
             pure $ Just (h, m)
        ]

instance GenUnchecked DateTime where
    genUnchecked = do
        yearMonthDay <- YearMonthDay <$> arbitrary <*> arbitrary <*> arbitrary
        hourMinute <- genUnchecked
        pure $ DateTime yearMonthDay Nothing hourMinute Nothing Nothing
    shrinkUnchecked _ = []

instance GenValid DateTime where
    genValid = do
        ymd <- genYMD
        hm <- genHM
        pure $ DateTime ymd Nothing hm Nothing Nothing

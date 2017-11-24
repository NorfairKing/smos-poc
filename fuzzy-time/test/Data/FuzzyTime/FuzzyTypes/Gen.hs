{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.FuzzyTime.FuzzyTypes.Gen where

import TestImport

import Data.FuzzyTime.FuzzyTypes

instance GenUnchecked FuzzyDay

instance GenValid FuzzyDay

instance GenUnchecked FuzzyDayOfTheWeek

instance GenValid FuzzyDayOfTheWeek

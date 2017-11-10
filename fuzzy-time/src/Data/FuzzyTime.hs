module Data.FuzzyTime where

import Import

import Data.Time

parseFuzzyDay :: UTCTime -> String -> Maybe Day
parseFuzzyDay now "now" = Just $ utctDay now
parseFuzzyDay _ _ = Nothing

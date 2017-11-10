{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Data.FuzzyTimeSpec
    ( spec
    ) where

import TestImport

import Data.Time

import Data.FuzzyTime

spec :: Spec
spec =
    describe "parseFuzzyDay" $
    it "parses 'now' as the day of the UTCTime" $
    forAll genValid $ \now ->
        parseFuzzyDay now "now" `shouldBe` Just (utctDay now)

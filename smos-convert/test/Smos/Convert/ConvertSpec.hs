{-# LANGUAGE OverloadedStrings #-}

module Smos.Convert.ConvertSpec where

import TestImport

import Smos.Convert.EntryTree
import Smos.Convert.Gen ()
import Smos.Convert.Time
import Smos.Data.Types

import Data.Time.Calendar
import Data.Time.LocalTime

import qualified Data.HashMap.Lazy as HM
import Data.OrgMode.Types

getYMD :: Day -> YearMonthDay
getYMD day =
    let (y, m, d) = toGregorian day
    in YearMonthDay (fromInteger y) m d

spec :: Spec
spec = do
    describe "toLocalTime" $ do
        it "produces valid instances" $ producesValidsOnValids toLocalTime
        it "properly converts orgmode's datetimes into localtimes" $
            forAll genValid $ \day ->
                let ymd = getYMD day
                    hm = Nothing
                    datetime = DateTime ymd Nothing hm Nothing Nothing
                    localtime = LocalTime day $ TimeOfDay 0 0 0
                in toLocalTime datetime `shouldBe` localtime
    describe "toEntryTree" $
        it "unit test" $
        forAll genValid $ \myTitle ->
            forAll genValid $ \myTags ->
                forAll genValid $ \myState ->
                    forAll genValid $ \now ->
                        forAll genValid $ \myContents ->
                            forAll genValid $ \myPropertyName ->
                                forAll genValid $ \myPropertyValue ->
                                    let mySection =
                                            Section
                                            { sectionTimestamp = Nothing
                                            , sectionPlannings = Plns HM.empty
                                            , sectionClocks = []
                                            , sectionProperties =
                                                  Properties $
                                                  HM.singleton
                                                      myPropertyName
                                                      myPropertyValue
                                            , sectionLogbook = Logbook []
                                            , sectionDrawers = []
                                            , sectionParagraph = myContents
                                            }
                                        headline =
                                            Headline
                                            { depth = Depth 2
                                            , stateKeyword =
                                                  Just $ StateKeyword myState
                                            , priority = Just A
                                            , title = myTitle
                                            , timestamp = Nothing
                                            , stats = Nothing
                                            , tags = myTags
                                            , section = mySection
                                            , subHeadlines = []
                                            }
                                        entry =
                                            Entry
                                            { entryHeader = Header myTitle
                                            , entryContents =
                                                  Just $ Contents myContents
                                            , entryTimestamps = HM.empty
                                            , entryProperties =
                                                  HM.singleton
                                                      (PropertyName
                                                           myPropertyName) $
                                                  PropertyValue myPropertyValue
                                            , entryStateHistory =
                                                  StateHistory
                                                      [ StateHistoryEntry
                                                            (Just $
                                                             TodoState myState)
                                                            now
                                                      ]
                                            , entryTags = Tag <$> myTags
                                            , entryLogbook = LogClosed []
                                            }
                                        entryTree = Node entry []
                                    in toEntryTree utc now headline `shouldBe`
                                       entryTree

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.Cursor.EntrySpec
    ( spec
    ) where

import TestImport

import qualified Data.Text as T

import Lens.Micro

import Smos.Cursor
import Smos.Cursor.Gen ()
import Smos.Cursor.TestUtils

import Smos.Data
import Smos.Data.Gen ()
import Smos.View

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

{-# ANN module ("HLint: ignore Functor law" :: String) #-}

-- TODO this can probably be done better.
-- mLens :: Lens' a b -> Lens' a (Maybe b)
-- mLens l = lens getter setter
--   where
--     getter a = Just $ a ^. l
--     setter a mb =
--         case mb of
--             Nothing -> a
--             Just b -> a & l .~ b
sameCAfterSettingAAndThenBLMM ::
       (Show c, Eq c)
    => EntryCursor
    -> Lens' EntryCursor (Maybe a)
    -> (a -> a)
    -> Lens' EntryCursor (Maybe b)
    -> (b -> b)
    -> (EntryCursor -> c)
    -> Expectation
sameCAfterSettingAAndThenBLMM ec la am lb bm cf =
    let ec' = ec & la %~ fmap am
        ec'' = ec' & lb %~ fmap bm
    in cf ec' `shouldBe` cf ec''

sameCAfterSettingAAndThenBLM ::
       (Show c, Eq c)
    => EntryCursor
    -> Lens' EntryCursor a
    -> (a -> a)
    -> Lens' EntryCursor (Maybe b)
    -> (b -> b)
    -> (EntryCursor -> c)
    -> Expectation
sameCAfterSettingAAndThenBLM ec la am lb bm cf =
    let ec' = ec & la %~ am
        ec'' = ec' & lb %~ fmap bm
    in cf ec' `shouldBe` cf ec''

sameCAfterSettingAAndThenBML ::
       (Show c, Eq c)
    => EntryCursor
    -> Lens' EntryCursor (Maybe a)
    -> (a -> a)
    -> Lens' EntryCursor b
    -> (b -> b)
    -> (EntryCursor -> c)
    -> Expectation
sameCAfterSettingAAndThenBML ec la am lb bm cf =
    let ec' = ec & la %~ fmap am
        ec'' = ec' & lb %~ bm
    in cf ec' `shouldBe` cf ec''

sameCAfterSettingAAndThenBL ::
       (Show c, Eq c)
    => EntryCursor
    -> Lens' EntryCursor a
    -> (a -> a)
    -> Lens' EntryCursor b
    -> (b -> b)
    -> (EntryCursor -> c)
    -> Expectation
sameCAfterSettingAAndThenBL ec la am lb bm cf =
    let ec' = ec & la %~ am
        ec'' = ec' & lb %~ bm
    in cf ec' `shouldBe` cf ec''

spec :: Spec
spec = do
    describe "EntryCursor" $ do
        describe "entryCursorParent" $
            it "rebuilds to the same" $ rebuildsToTheSame entryCursorParent
        describe "entryCursorHeader" $
            it "rebuilds to the same" $ rebuildsToTheSame entryCursorHeader
        describe "entryCursorContents" $
            it "rebuilds to the same" $
            rebuildsToTheSameIfSuceeds entryCursorContents
        describe "entryCursorState" $
            it "rebuilds to the same" $ rebuildsToTheSame entryCursorState
        describe "setting one after the other" $ do
            describe "state" $ do
                describe "header" $
                    it
                        "has the same state after setting the state and then changing the header" $
                    forAll genValid $ \ts ->
                        forAll genValid $ \h ->
                            forAll genValid $ \ec ->
                                forAll genValid $ \now ->
                                    sameCAfterSettingAAndThenBL
                                        ec
                                        entryCursorStateL
                                        (stateCursorSetState now ts)
                                        entryCursorHeaderL
                                        (headerCursorSetHeader h)
                                        (build . entryCursorState)
                describe "tags" $
                    it
                        "has the same state after setting the state and then changing the tags" $
                    forAll genValid $ \ts ->
                        forAll genValid $ \now ->
                            forAll genValid $ \tgs ->
                                forAll genValid $ \ec ->
                                    sameCAfterSettingAAndThenBLM
                                        ec
                                        entryCursorStateL
                                        (stateCursorSetState ts now)
                                        entryCursorTagsL
                                        (tagsCursorSetTags tgs)
                                        (build . entryCursorHeader)
                describe "contents" $
                    it
                        "has the same state after setting the state and then changing the contents" $
                    forAll genValid $ \ts ->
                        forAll genValid $ \cs ->
                            forAll genValid $ \ec ->
                                forAll genValid $ \now ->
                                    sameCAfterSettingAAndThenBLM
                                        ec
                                        entryCursorStateL
                                        (stateCursorSetState now ts)
                                        entryCursorContentsL
                                        (contentsCursorSetContents cs)
                                        (build . entryCursorState)
                describe "timestamps" $
                    it
                        "has the same state after setting the state and then changing the timestamps" $
                    forAll genValid $ \ts ->
                        forAll genValid $ \tss ->
                            forAll genValid $ \ec ->
                                forAll genValid $ \now ->
                                    sameCAfterSettingAAndThenBLM
                                        ec
                                        entryCursorStateL
                                        (stateCursorSetState now ts)
                                        entryCursorTimestampsL
                                        (timestampsCursorSetTimestamps tss)
                                        (build . entryCursorState)
            describe "header" $ do
                describe "state" $
                    it
                        "has the same header after setting the header and then changing the state" $
                    forAll genValid $ \ts ->
                        forAll genValid $ \h ->
                            forAll genValid $ \ec ->
                                forAll genValid $ \now ->
                                    sameCAfterSettingAAndThenBL
                                        ec
                                        entryCursorHeaderL
                                        (headerCursorSetHeader h)
                                        entryCursorStateL
                                        (stateCursorSetState now ts)
                                        (build . entryCursorHeader)
                describe "tags" $
                    it
                        "has the same header after setting the header and then changing the tags" $
                    forAll genValid $ \h ->
                        forAll genValid $ \tgs ->
                            forAll genValid $ \ec ->
                                sameCAfterSettingAAndThenBLM
                                    ec
                                    entryCursorHeaderL
                                    (headerCursorSetHeader h)
                                    entryCursorTagsL
                                    (tagsCursorSetTags tgs)
                                    (build . entryCursorHeader)
                describe "contents" $
                    it
                        "has the same header after setting the header and then changing the contents" $
                    forAll genValid $ \h ->
                        forAll genValid $ \cts ->
                            forAll genValid $ \ec ->
                                sameCAfterSettingAAndThenBLM
                                    ec
                                    entryCursorHeaderL
                                    (headerCursorSetHeader h)
                                    entryCursorContentsL
                                    (contentsCursorSetContents cts)
                                    (build . entryCursorHeader)
                describe "timestamps" $
                    it
                        "has the same header after setting the header and then changing the timestamps" $
                    forAll genValid $ \h ->
                        forAll genValid $ \tss ->
                            forAll genValid $ \ec ->
                                sameCAfterSettingAAndThenBLM
                                    ec
                                    entryCursorHeaderL
                                    (headerCursorSetHeader h)
                                    entryCursorTimestampsL
                                    (timestampsCursorSetTimestamps tss)
                                    (build . entryCursorHeader)
            describe "tags" $ do
                describe "state" $
                    it
                        "has the same tags after setting the tags and then changing the state" $
                    forAll genValid $ \tgs ->
                        forAll genValid $ \ts ->
                            forAll genValid $ \now ->
                                forAll genValid $ \ec ->
                                    sameCAfterSettingAAndThenBML
                                        ec
                                        entryCursorTagsL
                                        (tagsCursorSetTags tgs)
                                        entryCursorStateL
                                        (stateCursorSetState now ts)
                                        (fmap build . entryCursorTags)
                describe "header" $
                    it
                        "has the same tags after setting the tags and then changing the header" $
                    forAll genValid $ \tgs ->
                        forAll genValid $ \h ->
                            forAll genValid $ \ec ->
                                sameCAfterSettingAAndThenBML
                                    ec
                                    entryCursorTagsL
                                    (tagsCursorSetTags tgs)
                                    entryCursorHeaderL
                                    (headerCursorSetHeader h)
                                    (fmap build . entryCursorTags)
                describe "contents" $
                    it
                        "has the same tags after setting the tags and then changing the contents" $
                    forAll genValid $ \tgs ->
                        forAll genValid $ \cts ->
                            forAll genValid $ \ec ->
                                sameCAfterSettingAAndThenBLMM
                                    ec
                                    entryCursorTagsL
                                    (tagsCursorSetTags tgs)
                                    entryCursorContentsL
                                    (contentsCursorSetContents cts)
                                    (fmap build . entryCursorTags)
                describe "timestamps" $
                    it
                        "has the same tags after setting the tags and then changing the timestamps" $
                    forAll genValid $ \tgs ->
                        forAll genValid $ \tss ->
                            forAll genValid $ \ec ->
                                sameCAfterSettingAAndThenBLMM
                                    ec
                                    entryCursorTagsL
                                    (tagsCursorSetTags tgs)
                                    entryCursorTimestampsL
                                    (timestampsCursorSetTimestamps tss)
                                    (fmap build . entryCursorTags)
            describe "contents" $ do
                describe "header" $
                    it
                        "has the same contents after setting the contents and then changing the header" $
                    forAll genValid $ \cts ->
                        forAll genValid $ \h ->
                            forAll genValid $ \ec ->
                                sameCAfterSettingAAndThenBML
                                    ec
                                    entryCursorContentsL
                                    (contentsCursorSetContents cts)
                                    entryCursorHeaderL
                                    (headerCursorSetHeader h)
                                    (fmap build . entryCursorContents)
                describe "state" $
                    it
                        "has the same contents after setting the contents and then changing the state" $
                    forAll genValid $ \cs ->
                        forAll genValid $ \ts ->
                            forAll genValid $ \ec ->
                                forAll genValid $ \now ->
                                    sameCAfterSettingAAndThenBML
                                        ec
                                        entryCursorContentsL
                                        (contentsCursorSetContents cs)
                                        entryCursorStateL
                                        (stateCursorSetState now ts)
                                        (fmap build . entryCursorContents)
                describe "tags" $
                    it
                        "has the same contents after setting the contents and then changing the tags" $
                    forAll genValid $ \cs ->
                        forAll genValid $ \tgs ->
                            forAll genValid $ \ec ->
                                sameCAfterSettingAAndThenBLMM
                                    ec
                                    entryCursorContentsL
                                    (contentsCursorSetContents cs)
                                    entryCursorTagsL
                                    (tagsCursorSetTags tgs)
                                    (fmap build . entryCursorContents)
                describe "timestamps" $
                    it
                        "has the same contents after setting the contents and then changing the timestamps" $
                    forAll genValid $ \cs ->
                        forAll genValid $ \tss ->
                            forAll genValid $ \ec ->
                                sameCAfterSettingAAndThenBLMM
                                    ec
                                    entryCursorContentsL
                                    (contentsCursorSetContents cs)
                                    entryCursorTimestampsL
                                    (timestampsCursorSetTimestamps tss)
                                    (fmap build . entryCursorContents)
            describe "timestamps" $ do
                describe "state" $
                    it
                        "has the same timestamps after setting the timestamps and then changing the state" $
                    forAll genValid $ \tss ->
                        forAll genValid $ \ts ->
                            forAll genValid $ \now ->
                                forAll genValid $ \ec ->
                                    sameCAfterSettingAAndThenBML
                                        ec
                                        entryCursorTimestampsL
                                        (timestampsCursorSetTimestamps tss)
                                        entryCursorStateL
                                        (stateCursorSetState now ts)
                                        (fmap build . entryCursorTimestamps)
                describe "header" $
                    it
                        "has the same timestamps after setting the timestamps and then changing the header" $
                    forAll genValid $ \tss ->
                        forAll genValid $ \h ->
                            forAll genValid $ \ec ->
                                sameCAfterSettingAAndThenBML
                                    ec
                                    entryCursorTimestampsL
                                    (timestampsCursorSetTimestamps tss)
                                    entryCursorHeaderL
                                    (headerCursorSetHeader h)
                                    (fmap build . entryCursorTimestamps)
                describe "tags" $
                    it
                        "has the same timestamps after setting the timestamps and then changing the tags" $
                    forAll genValid $ \tss ->
                        forAll genValid $ \tgs ->
                            forAll genValid $ \ec ->
                                sameCAfterSettingAAndThenBLMM
                                    ec
                                    entryCursorTimestampsL
                                    (timestampsCursorSetTimestamps tss)
                                    entryCursorTagsL
                                    (tagsCursorSetTags tgs)
                                    (fmap build . entryCursorTimestamps)
                describe "contents" $
                    it
                        "has the same timestamps after setting the timestamps and then changing the contents" $
                    forAll genValid $ \tss ->
                        forAll genValid $ \cts ->
                            forAll genValid $ \ec ->
                                sameCAfterSettingAAndThenBLMM
                                    ec
                                    entryCursorTimestampsL
                                    (timestampsCursorSetTimestamps tss)
                                    entryCursorContentsL
                                    (contentsCursorSetContents cts)
                                    (fmap build . entryCursorTimestamps)
    describe "HeaderCursor" $ do
        describe "headerCursorParent" $
            it "rebuilds to the same" $ rebuildsToTheSame headerCursorParent
        describe "headerCursorInsert" $
            it "inserts a character at the front" $
            forAll genUnchecked $ \c ->
                forAll genValid $ \hc ->
                    source
                        (headerViewHeader
                             (selectValue (build (headerCursorInsert c hc)))) `shouldBe`
                    T.cons
                        c
                        (source (headerViewHeader (selectValue (build hc))))
        describe "headerCursorRemove" $
            it "removes a character at the front" $
            forAll genValid $ \hc ->
                case headerCursorRemove hc of
                    Nothing -> pure ()
                    Just hc' ->
                        case T.uncons $
                             source $ headerViewHeader $ selectValue $ build hc' of
                            Nothing ->
                                expectationFailure "Something went wrong."
                            Just (_, t) ->
                                t `shouldBe`
                                source
                                    (headerViewHeader (selectValue (build hc)))
        describe "headerCursorDelete" $
            it "removes a character at the end" $
            forAll genValid $ \hc ->
                case headerCursorRemove hc of
                    Nothing -> pure ()
                    Just hc' ->
                        case T.uncons $
                             T.reverse $
                             source $ headerViewHeader $ selectValue $ build hc' of
                            Nothing ->
                                expectationFailure "Something went wrong."
                            Just (_, t) ->
                                T.reverse t `shouldBe`
                                source
                                    (headerViewHeader (selectValue (build hc)))
        describe "headerCursorLeft" $
            it "rebuilds to the same" $
            rebuildsToTheSameIfSuceeds headerCursorLeft
        describe "headerCursorRight" $
            it "rebuilds to the same" $
            rebuildsToTheSameIfSuceeds headerCursorRight
        describe "headerCursorStart" $
            it "rebuilds to the same" $ rebuildsToTheSame headerCursorStart
        describe "headerCursorEnd" $
            it "rebuilds to the same" $ rebuildsToTheSame headerCursorEnd
    describe "ContentsCursor" $ do
        describe "contentsCursorParent" $
            it "rebuilds to the same" $ rebuildsToTheSame contentsCursorParent
        describe "contentsCursorSetContents" $
            it "builds to the given contents" $
            forAll genValid $ \cs ->
                forAll genValid $ \cc ->
                    source
                        (contentsViewContents
                             (selectValue
                                  (build (contentsCursorSetContents cs cc)))) `shouldBe`
                    contentsText cs
    describe "StateCursor" $ do
        describe "stateCursorParent" $
            it "rebuilds to the same" $ rebuildsToTheSame stateCursorParent
        describe "stateCursorClear" $
            it "builds to an empty state" $
            forAll genValid $ \sc ->
                forAll genValid $ \now ->
                    stateHistoryState
                        (todostateViewTodostate
                             (selectValue (build (stateCursorClear now sc)))) `shouldBe`
                    Nothing
        describe "stateCursorSetState" $
            it "builds to a state view with the given contents" $
            forAll genValid $ \ts ->
                forAll genValid $ \sc ->
                    forAll genValid $ \now ->
                        stateHistoryState
                            (todostateViewTodostate
                                 (selectValue
                                      (build (stateCursorSetState now ts sc)))) `shouldBe`
                        Just ts

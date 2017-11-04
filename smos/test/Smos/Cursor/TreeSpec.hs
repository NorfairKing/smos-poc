{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.Cursor.TreeSpec
    ( spec
    ) where

import TestImport

import Data.Tree

import Smos.Cursor
import Smos.Cursor.Gen ()
import Smos.Cursor.TestUtils
import Smos.Cursor.Tree
import Smos.Data
import Smos.Data.Gen ()

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

{-# ANN module ("HLint: ignore Functor law" :: String) #-}

spec :: Spec
spec = do
    describe "ForestCursor" $ do
        describe "makeForestCurser" $
            it "is the inverse of 'build'" $
            inverseFunctionsOnValid
                (makeForestCursor :: Forest Entry -> ForestCursor EntryCursor)
                build
        describe "forestCursorSelectIx" $
            it "rebuilds to the same" $
            forAll genUnchecked $ \i ->
                rebuildsToTheSameIfSuceeds
                    ((`forestCursorSelectIx` i) :: ForestCursor EntryCursor -> Maybe (TreeCursor EntryCursor))
        describe "forestCursorSelectFirst" $
            it "rebuilds to the same" $
            rebuildsToTheSameIfSuceeds
                (forestCursorSelectFirst :: ForestCursor EntryCursor -> Maybe (TreeCursor EntryCursor))
        describe "forestCursorSelectLast" $
            it "rebuilds to the same" $
            rebuildsToTheSameIfSuceeds
                (forestCursorSelectLast :: ForestCursor EntryCursor -> Maybe (TreeCursor EntryCursor))
        describe "forestCursorInsertAtStart" $
            it "rebuilds to something valid" $
            forAll genValid $ \st ->
                rebuildsToValid
                    (forestCursorInsertAtStart st :: ForestCursor EntryCursor -> ForestCursor EntryCursor)
        describe "forestCursorInsertAtEnd" $
            it "rebuilds to something valid" $
            forAll genValid $ \st ->
                rebuildsToValid
                    (forestCursorInsertAtEnd st :: ForestCursor EntryCursor -> ForestCursor EntryCursor)
    describe "TreeCursor" $ do
        describe "treeCursorSelectPrev" $
            it "rebuilds to the same" $
            rebuildsToTheSameIfSuceeds
                (treeCursorSelectPrev :: TreeCursor EntryCursor -> Maybe (TreeCursor EntryCursor))
        describe "treeCursorSelectNext" $
            it "rebuilds to the same" $
            rebuildsToTheSameIfSuceeds
                (treeCursorSelectNext :: TreeCursor EntryCursor -> Maybe (TreeCursor EntryCursor))
        describe "treeCursorInsertAbove" $
            it "rebuilds to something valid" $
            forAll genValid $ \st ->
                rebuildsToValid
                    ((`treeCursorInsertAbove` st) :: TreeCursor EntryCursor -> TreeCursor EntryCursor)
        describe "treeCursorInsertBelow" $
            it "rebuilds to something valid" $
            forAll genValid $ \st ->
                rebuildsToValid
                    ((`treeCursorInsertBelow` st) :: TreeCursor EntryCursor -> TreeCursor EntryCursor)
        describe "treeCursorInsertChildAt" $
            it "rebuilds to something valid" $
            forAll genUnchecked $ \ix_ ->
                forAll genValid $ \st ->
                    rebuildsToValid
                        (treeCursorInsertChildAt ix_ st :: TreeCursor EntryCursor -> TreeCursor EntryCursor)
        describe "treeCursorInsertChildAtStart" $
            it "rebuilds to something valid" $
            forAll genValid $ \st ->
                rebuildsToValid
                    (treeCursorInsertChildAtStart st :: TreeCursor EntryCursor -> TreeCursor EntryCursor)
        describe "treeCursorInsertChildAtEnd" $
            it "rebuilds to something valid" $
            forAll genValid $ \st ->
                rebuildsToValid
                    (treeCursorInsertChildAtEnd st :: TreeCursor EntryCursor -> TreeCursor EntryCursor)
        describe "treeCursorDeleteCurrent" $
            it "rebuilds to something valid" $
            forAll genValid $ \tc ->
                case treeCursorDeleteCurrent tc :: Either (ForestCursor EntryCursor) (TreeCursor EntryCursor) of
                    Left fc' -> shouldBeValid fc'
                    Right tc' -> shouldBeValid tc'

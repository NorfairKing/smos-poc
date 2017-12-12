{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.TreeSpec
    ( spec
    ) where

import TestImport

import Data.Tree

import Cursor.Class
import Cursor.Select
import Cursor.TestUtils
import Cursor.Tree
import Cursor.Tree.Gen ()

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

{-# ANN module ("HLint: ignore Functor law" :: String) #-}

newtype CharView = CharView
    { charViewChar :: Char
    } deriving (Show, Eq, Generic)

instance Validity CharView

instance View CharView where
    type Source CharView = Char
    source = charViewChar
    view = CharView

data CharCursor = CharCursor
    { charTreeCursor :: TreeCursor CharCursor
    , charValue :: Char
    } deriving (Show, Eq, Generic)

instance Validity CharCursor

instance GenUnchecked CharCursor

instance GenValid CharCursor

instance Build CharCursor where
    type Building CharCursor = CharView
    build = view . charValue

instance Rebuild CharCursor where
    type ReBuilding CharCursor = Select (ForestView CharView)
    rebuild = rebuild . charTreeCursor
    selection CharCursor {..} = 0 : selection charTreeCursor

instance BuiltFrom CharCursor CharView where
    type Parent CharCursor = TreeCursor CharCursor
    makeWith tc i = ic'
      where
        ic' =
            CharCursor
            {charTreeCursor = tc {treeCursorValue = ic'}, charValue = source i}

spec :: Spec
spec = do
    describe "ForestCursor" $ do
        describe "makeForestCurser" $
            it "is the inverse of 'build'" $
            inverseFunctionsOnValid
                (makeForestCursor' :: Forest Char -> ForestCursor CharCursor)
                (source . selectValue . build)
        describe "forestCursorSelectIx" $
            it "rebuilds to the same" $
            forAll genUnchecked $ \i ->
                rebuildsToTheSameIfSuceeds
                    (forestCursorSelectIx i :: ForestCursor CharCursor -> Maybe (TreeCursor CharCursor))
        describe "forestCursorSelectFirst" $
            it "rebuilds to the same" $
            rebuildsToTheSameIfSuceeds
                (forestCursorSelectFirst :: ForestCursor CharCursor -> Maybe (TreeCursor CharCursor))
        describe "forestCursorSelectLast" $
            it "rebuilds to the same" $
            rebuildsToTheSameIfSuceeds
                (forestCursorSelectLast :: ForestCursor CharCursor -> Maybe (TreeCursor CharCursor))
        describe "forestCursorInsertAtStart" $
            it "rebuilds to something valid" $
            forAll genValid $ \st ->
                rebuildsToValid
                    (forestCursorInsertAtStart st :: ForestCursor CharCursor -> ForestCursor CharCursor)
        describe "forestCursorInsertAtEnd" $
            it "rebuilds to something valid" $
            forAll genValid $ \st ->
                rebuildsToValid
                    (forestCursorInsertAtEnd st :: ForestCursor CharCursor -> ForestCursor CharCursor)
    describe "TreeCursor" $ do
        describe "treeCursorSelectPrev" $
            it "rebuilds to the same" $
            rebuildsToTheSameIfSuceeds
                (treeCursorSelectPrev :: TreeCursor CharCursor -> Maybe (TreeCursor CharCursor))
        describe "treeCursorSelectNext" $
            it "rebuilds to the same" $
            rebuildsToTheSameIfSuceeds
                (treeCursorSelectNext :: TreeCursor CharCursor -> Maybe (TreeCursor CharCursor))
        describe "treeCursorInsertAbove" $
            it "rebuilds to something valid" $
            forAll genValid $ \st ->
                rebuildsToValid
                    ((`treeCursorInsertAbove` st) :: TreeCursor CharCursor -> TreeCursor CharCursor)
        describe "treeCursorInsertBelow" $
            it "rebuilds to something valid" $
            forAll genValid $ \st ->
                rebuildsToValid
                    ((`treeCursorInsertBelow` st) :: TreeCursor CharCursor -> TreeCursor CharCursor)
        describe "treeCursorInsertChildAt" $
            it "rebuilds to something valid" $
            forAll genUnchecked $ \ix_ ->
                forAll genValid $ \st ->
                    rebuildsToValid
                        (treeCursorInsertChildAt ix_ st :: TreeCursor CharCursor -> TreeCursor CharCursor)
        describe "treeCursorInsertChildAtStart" $
            it "rebuilds to something valid" $
            forAll genValid $ \st ->
                rebuildsToValid
                    (treeCursorInsertChildAtStart st :: TreeCursor CharCursor -> TreeCursor CharCursor)
        describe "treeCursorInsertChildAtEnd" $
            it "rebuilds to something valid" $
            forAll genValid $ \st ->
                rebuildsToValid
                    (treeCursorInsertChildAtEnd st :: TreeCursor CharCursor -> TreeCursor CharCursor)
        describe "treeCursorDeleteCurrent" $
            it "rebuilds to something valid" $
            forAll genValid $ \tc ->
                case treeCursorDeleteCurrent tc :: Either (ForestCursor CharCursor) (TreeCursor CharCursor) of
                    Left fc' -> shouldBeValid fc'
                    Right tc' -> shouldBeValid tc'
        describe "treeCursorMoveUp" $
            it "rebuilds to something valid if it succeeds" $
            rebuildsToValidIfSucceeds
                (treeCursorMoveUp :: TreeCursor CharCursor -> Maybe (TreeCursor CharCursor))
        describe "treeCursorMoveDown" $
            it "rebuilds to something valid if it succeeds" $
            rebuildsToValidIfSucceeds
                (treeCursorMoveDown :: TreeCursor CharCursor -> Maybe (TreeCursor CharCursor))
        describe "treeCursorMoveLeft" $
            it "rebuilds to something valid if it succeeds" $
            rebuildsToValidIfSucceeds
                (treeCursorMoveLeft :: TreeCursor CharCursor -> Maybe (TreeCursor CharCursor))
        describe "treeCursorMoveRight" $
            it "rebuilds to something valid if it succeeds" $
            rebuildsToValidIfSucceeds
                (treeCursorMoveRight :: TreeCursor CharCursor -> Maybe (TreeCursor CharCursor))

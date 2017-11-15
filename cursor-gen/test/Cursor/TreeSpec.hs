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
import Cursor.TestUtils
import Cursor.Tree
import Cursor.Tree.Gen ()

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

{-# ANN module ("HLint: ignore Functor law" :: String) #-}

-- A degenerate cursor
newtype IntView = IntView
    { intViewInt :: Int
    } deriving (Show, Eq, Generic)

instance Validity IntView

instance View IntView where
    type Source IntView = Int
    source = intViewInt
    view = IntView

data IntCursor = IntCursor
    { intTreeCursor :: TreeCursor IntCursor
    , intValue :: Int
    } deriving (Show, Eq, Generic)

instance Validity IntCursor

instance GenUnchecked IntCursor

instance GenValid IntCursor

instance Build IntCursor where
    type Building IntCursor = IntView
    build = view . intValue

instance Rebuild IntCursor where
    type ReBuilding IntCursor = ForestView IntView
    rebuild = rebuild . intTreeCursor
    selection IntCursor {..} = 0 : selection intTreeCursor

instance BuiltFrom IntCursor IntView where
    type Parent IntCursor = TreeCursor IntCursor
    makeWith tc i = ic'
      where
        ic' =
            IntCursor
            {intTreeCursor = tc {treeCursorValue = ic'}, intValue = source i}

spec :: Spec
spec = do
    describe "ForestCursor" $ do
        describe "makeForestCurser" $
            it "is the inverse of 'build'" $
            inverseFunctionsOnValid
                (makeForestCursor' :: Forest Int -> ForestCursor IntCursor)
                (source . build)
        describe "forestCursorSelectIx" $
            it "rebuilds to the same" $
            forAll genUnchecked $ \i ->
                rebuildsToTheSameIfSuceeds
                    (forestCursorSelectIx i :: ForestCursor IntCursor -> Maybe (TreeCursor IntCursor))
        describe "forestCursorSelectFirst" $
            it "rebuilds to the same" $
            rebuildsToTheSameIfSuceeds
                (forestCursorSelectFirst :: ForestCursor IntCursor -> Maybe (TreeCursor IntCursor))
        describe "forestCursorSelectLast" $
            it "rebuilds to the same" $
            rebuildsToTheSameIfSuceeds
                (forestCursorSelectLast :: ForestCursor IntCursor -> Maybe (TreeCursor IntCursor))
        describe "forestCursorInsertAtStart" $
            it "rebuilds to something valid" $
            forAll genValid $ \st ->
                rebuildsToValid
                    (forestCursorInsertAtStart st :: ForestCursor IntCursor -> ForestCursor IntCursor)
        describe "forestCursorInsertAtEnd" $
            it "rebuilds to something valid" $
            forAll genValid $ \st ->
                rebuildsToValid
                    (forestCursorInsertAtEnd st :: ForestCursor IntCursor -> ForestCursor IntCursor)
    describe "TreeCursor" $ do
        describe "treeCursorSelectPrev" $
            it "rebuilds to the same" $
            rebuildsToTheSameIfSuceeds
                (treeCursorSelectPrev :: TreeCursor IntCursor -> Maybe (TreeCursor IntCursor))
        describe "treeCursorSelectNext" $
            it "rebuilds to the same" $
            rebuildsToTheSameIfSuceeds
                (treeCursorSelectNext :: TreeCursor IntCursor -> Maybe (TreeCursor IntCursor))
        describe "treeCursorInsertAbove" $
            it "rebuilds to something valid" $
            forAll genValid $ \st ->
                rebuildsToValid
                    ((`treeCursorInsertAbove` st) :: TreeCursor IntCursor -> TreeCursor IntCursor)
        describe "treeCursorInsertBelow" $
            it "rebuilds to something valid" $
            forAll genValid $ \st ->
                rebuildsToValid
                    ((`treeCursorInsertBelow` st) :: TreeCursor IntCursor -> TreeCursor IntCursor)
        describe "treeCursorInsertChildAt" $
            it "rebuilds to something valid" $
            forAll genUnchecked $ \ix_ ->
                forAll genValid $ \st ->
                    rebuildsToValid
                        (treeCursorInsertChildAt ix_ st :: TreeCursor IntCursor -> TreeCursor IntCursor)
        describe "treeCursorInsertChildAtStart" $
            it "rebuilds to something valid" $
            forAll genValid $ \st ->
                rebuildsToValid
                    (treeCursorInsertChildAtStart st :: TreeCursor IntCursor -> TreeCursor IntCursor)
        describe "treeCursorInsertChildAtEnd" $
            it "rebuilds to something valid" $
            forAll genValid $ \st ->
                rebuildsToValid
                    (treeCursorInsertChildAtEnd st :: TreeCursor IntCursor -> TreeCursor IntCursor)
        describe "treeCursorDeleteCurrent" $
            it "rebuilds to something valid" $
            forAll genValid $ \tc ->
                case treeCursorDeleteCurrent tc :: Either (ForestCursor IntCursor) (TreeCursor IntCursor) of
                    Left fc' -> shouldBeValid fc'
                    Right tc' -> shouldBeValid tc'
        describe "treeCursorMoveUp" $
            it "rebuilds to something valid if it succeeds" $
            rebuildsToValidIfSucceeds
                (treeCursorMoveUp :: TreeCursor IntCursor -> Maybe (TreeCursor IntCursor))
        describe "treeCursorMoveDown" $
            it "rebuilds to something valid if it succeeds" $
            rebuildsToValidIfSucceeds
                (treeCursorMoveDown :: TreeCursor IntCursor -> Maybe (TreeCursor IntCursor))
        describe "treeCursorMoveLeft" $
            it "rebuilds to something valid if it succeeds" $
            rebuildsToValidIfSucceeds
                (treeCursorMoveLeft :: TreeCursor IntCursor -> Maybe (TreeCursor IntCursor))
        describe "treeCursorMoveRight" $
            it "rebuilds to something valid if it succeeds" $
            rebuildsToValidIfSucceeds
                (treeCursorMoveRight :: TreeCursor IntCursor -> Maybe (TreeCursor IntCursor))

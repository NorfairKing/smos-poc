{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.Cursor.TagsSpec
    ( spec
    ) where

import TestImport

import Smos.Cursor
import Smos.Cursor.Gen ()

import Smos.Data.Gen ()

spec :: Spec
spec = do
    describe "newTagsCursor" $
        it "produces valid tags cursors" $ producesValidsOnValids newTagsCursor
    describe "makeNewTagsCursor" $
        it "produces valid tags cursors" $
        producesValidsOnValids2 makeNewTagsCursor
    describe "makeTagsCursor" $
        it "produces valid tags cursors" $
        producesValidsOnValids2 makeTagsCursor
    describe "tagsCursorSetTags" $
        it "produces valid tags cursors" $
        producesValidsOnValids2 tagsCursorSetTags
    describe "tagsCursorSelectNext" $
        it "produces valids on valids" $
        producesValidsOnValids tagsCursorSelectNext
    describe "tagsCursorSelectPrev" $
        it "produces valids on valids" $
        producesValidsOnValids tagsCursorSelectPrev
    describe "tagsCursorSelectFirst" $
        it "produces valids on valids" $
        producesValidsOnValids tagsCursorSelectFirst
    describe "tagsCursorSelectLast" $
        it "produces valids on valids" $
        producesValidsOnValids tagsCursorSelectLast
    describe "tagsCursorSelectFirst" $
        it "produces valids on valids" $
        forAllValid $ \t -> producesValidsOnValids $ tagsCursorInsertAndSelect t
    describe "tagsCursorSelectLast" $
        it "produces valids on valids" $
        forAllValid $ \t -> producesValidsOnValids $ tagsCursorAppendAndSelect t
    describe "tagsCursorSet" $
        it "produces valids on valids" $ producesValidsOnValids2 tagsCursorSet
    describe "tagsCursorUnset" $
        it "produces valids on valids" $ producesValidsOnValids2 tagsCursorUnset
    describe "tagsCursorToggle" $
        it "produces valids on valids" $
        producesValidsOnValids2 tagsCursorToggle

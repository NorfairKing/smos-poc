{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.Cursor.TimestampsSpec
    ( spec
    ) where

import TestImport

import Smos.Cursor
import Smos.Cursor.Gen ()

import Smos.Data.Gen ()

spec :: Spec
spec = do
    describe "newTimestampsCursor" $
        it "produces valid tags cursors" $
        producesValidsOnValids2 newTimestampsCursor
    describe "makeNewTimestampsCursor" $
        it "produces valid tags cursors" $
        producesValidsOnValids3 makeNewTimestampsCursor
    describe "makeTimestampsCursor" $
        it "produces valid tags cursors" $
        producesValidsOnValids2 makeTimestampsCursor
    describe "timestampsCursorSetTimestamps" $
        it "produces valid tags cursors" $
        producesValidsOnValids2 timestampsCursorSetTimestamps
    describe "timestampsCursorSelectPrev" $
        it "produces valid timestamp cursors" $
        producesValidsOnValids timestampsCursorSelectPrev
    describe "timestampsCursorSelectNext" $
        it "produces valid timestamp cursors" $
        producesValidsOnValids timestampsCursorSelectNext
    describe "timestampsCursorSelectFirst" $
        it "produces valid timestamp cursors" $
        producesValidsOnValids timestampsCursorSelectFirst
    describe "timestampsCursorSelectLast" $
        it "produces valid timestamp cursors" $
        producesValidsOnValids timestampsCursorSelectLast
    describe "timestampsCursorInsert" $
        it "produces valid timestamp cursors" $
        producesValidsOnValids3 timestampsCursorInsert
    describe "timestampsCursorAppend" $
        it "produces valid timestamp cursors" $
        producesValidsOnValids3 timestampsCursorAppend
    describe "timestampsCursorInsertAndSelect" $
        it "produces valid timestamp cursors" $
        producesValidsOnValids3 timestampsCursorInsertAndSelect
    describe "timestampsCursorAppendAndSelect" $
        it "produces valid timestamp cursors" $
        producesValidsOnValids3 timestampsCursorAppendAndSelect
    describe "timestampsCursorRemoveElemAndSelectPrev" $
        it "produces valid timestamp cursors" $
        producesValidsOnValids timestampsCursorRemoveElemAndSelectPrev
    describe "timestampsCursorDeleteElemAndSelectNext" $
        it "produces valid timestamp cursors" $
        producesValidsOnValids timestampsCursorDeleteElemAndSelectNext
    describe "timestampsCursorRemoveElem" $
        it "produces valid timestamp cursors" $
        producesValidsOnValids timestampsCursorRemoveElem
    describe "timestampsCursorDeleteElem" $
        it "produces valid timestamp cursors" $
        producesValidsOnValids timestampsCursorDeleteElem
    describe "timestampsCursorRemove" $
        it "produces valid timestamp cursors" $
        producesValidsOnValids timestampsCursorRemove
    describe "timestampsCursorDelete" $
        it "produces valid timestamp cursors" $
        producesValidsOnValids timestampsCursorDelete

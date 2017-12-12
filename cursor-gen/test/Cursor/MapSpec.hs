{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module Cursor.MapSpec
    ( spec
    ) where

import TestImport

import Data.HashMap.Lazy (HashMap)
import Data.List.NonEmpty (NonEmpty(..))

import Cursor.Class
import Cursor.Map

import Cursor.Map.Gen ()
import Cursor.TestUtils

spec :: Spec
spec = do
    genValidSpec @(MapCursor IntCursor IntCursor)
    genValidSpec @(KeyValueCursor IntCursor IntCursor)
    genValidSpec @(KeyCursor IntCursor IntCursor)
    genValidSpec @(ValueCursor IntCursor IntCursor)
    describe "makeMapCursor" $
        it "produces valid map cursors" $
        producesValidsOnValids
            (makeMapCursor :: NonEmpty (IntCursor, IntCursor) -> MapCursor IntCursor IntCursor)
    describe "makeMapCursorFromMap" $
        it "produces valid map cursors" $
        producesValidsOnValids
            (makeMapCursorFromMap :: HashMap IntCursor IntCursor -> Maybe (MapCursor IntCursor IntCursor))
    describe "rebuildHashmapFromMapView" $
        it "produces valid hashmaps" $
        producesValidsOnValids
            (rebuildHashmapFromMapView :: MapView Int Char -> HashMap Int Char)
    describe "rebuildMapCursor" $
        it "produces valid lists" $
        producesValidsOnValids (rebuildMapCursor @IntCursor @IntCursor)
    describe "mapCursorSelectPrev" $ do
        it "produces valid map cursors" $
            producesValidsOnValids (mapCursorSelectPrev @IntCursor @IntCursor)
        it "is a movement" $ isMovementM mapCursorSelectPrev
    describe "mapCursorSelectNext" $ do
        it "produces valid map cursors" $
            producesValidsOnValids (mapCursorSelectNext @IntCursor @IntCursor)
        it "is a movement" $ isMovementM mapCursorSelectNext
    describe "mapCursorSelectFirst" $ do
        it "produces valid map cursors" $
            producesValidsOnValids (mapCursorSelectFirst @IntCursor @IntCursor)
        it "is a movement" $ isMovement mapCursorSelectFirst
    describe "mapCursorSelectLast" $ do
        it "produces valid map cursors" $
            producesValidsOnValids (mapCursorSelectLast @IntCursor @IntCursor)
        it "is a movement" $ isMovement mapCursorSelectLast
    describe "mapCursorInsert" $
        it "produces valid map cursors" $
        producesValidsOnValids3 (mapCursorInsert @IntCursor @IntCursor)
    describe "mapCursorAppend" $
        it "produces valid map cursors" $
        producesValidsOnValids3 (mapCursorAppend @IntCursor @IntCursor)
    describe "mapCursorInsertAndSelect" $
        it "produces valid map cursors" $
        producesValidsOnValids3 (mapCursorInsertAndSelect @IntCursor @IntCursor)
    describe "mapCursorAppendAndSelect" $
        it "produces valid map cursors" $
        producesValidsOnValids3 (mapCursorAppendAndSelect @IntCursor @IntCursor)
    describe "mapCursorRemoveElemAndSelectPrev" $
        it "produces valid map cursors" $
        producesValidsOnValids
            (mapCursorRemoveElemAndSelectPrev @IntCursor @IntCursor)
    describe "mapCursorDeleteElemAndSelectNext" $
        it "produces valid map cursors" $
        producesValidsOnValids
            (mapCursorDeleteElemAndSelectNext @IntCursor @IntCursor)
    describe "mapCursorRemoveElem" $
        it "produces valid map cursors" $
        producesValidsOnValids (mapCursorRemoveElem @IntCursor @IntCursor)
    describe "mapCursorDeleteElem" $
        it "produces valid map cursors" $
        producesValidsOnValids (mapCursorDeleteElem @IntCursor @IntCursor)
    describe "keyCursorSelectValue" $
        it "is a movement" $
        forAllValid $ \kc -> do
            let vc = keyCursorSelectValue (kc :: KeyCursor IntCursor IntCursor)
            source (rebuild kc) `shouldBe` source (rebuild vc)
    describe "valueCursorSelectKey" $
        it "is a movement" $
        forAllValid $ \vc -> do
            let kc =
                    valueCursorSelectKey (vc :: ValueCursor IntCursor IntCursor)
            source (rebuild kc) `shouldBe` source (rebuild vc)

isMovement :: (forall a b. MapCursor a b -> MapCursor a b) -> Property
isMovement func =
    forAllValid $ \mc ->
        rebuildMapCursor mc `shouldBe`
        rebuildMapCursor (func mc :: MapCursor IntCursor IntCursor)

isMovementM :: (forall a b. MapCursor a b -> Maybe (MapCursor a b)) -> Property
isMovementM func =
    forAllValid $ \mc ->
        case func (mc :: MapCursor IntCursor IntCursor) of
            Nothing -> pure () -- fine
            Just mc' -> rebuildMapCursor mc `shouldBe` rebuildMapCursor mc'

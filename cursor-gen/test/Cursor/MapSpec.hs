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

spec :: Spec
spec = do
    genValidSpec @(MapCursor Int Int)
    genValidSpec @(KeyValueCursor Int Int)
    genValidSpec @(KeyCursor Int Int)
    genValidSpec @(ValueCursor Int Int)
    describe "makeMapCursor" $
        it "produces valid map cursors" $
        producesValidsOnValids
            (makeMapCursor :: NonEmpty (Int, Char) -> MapCursor Int Char)
    describe "makeMapCursorFromMap" $
        it "produces valid map cursors" $
        producesValidsOnValids
            (makeMapCursorFromMap :: HashMap Int Char -> Maybe (MapCursor Int Char))
    describe "rebuildHashmapFromMapView" $
        it "produces valid hashmaps" $
        producesValidsOnValids
            (rebuildHashmapFromMapView :: MapView Int Char -> HashMap Int Char)
    describe "rebuildMapCursor" $
        it "produces valid lists" $
        producesValidsOnValids (rebuildMapCursor @Int @Char)
    describe "mapCursorSelectPrev" $ do
        it "produces valid map cursors" $
            producesValidsOnValids (mapCursorSelectPrev @Int @Char)
        it "is a movement" $ isMovementM mapCursorSelectPrev
    describe "mapCursorSelectNext" $ do
        it "produces valid map cursors" $
            producesValidsOnValids (mapCursorSelectNext @Int @Char)
        it "is a movement" $ isMovementM mapCursorSelectNext
    describe "mapCursorSelectFirst" $ do
        it "produces valid map cursors" $
            producesValidsOnValids (mapCursorSelectFirst @Int @Char)
        it "is a movement" $ isMovement mapCursorSelectFirst
    describe "mapCursorSelectLast" $ do
        it "produces valid map cursors" $
            producesValidsOnValids (mapCursorSelectLast @Int @Char)
        it "is a movement" $ isMovement mapCursorSelectLast
    describe "mapCursorInsert" $
        it "produces valid map cursors" $
        producesValidsOnValids3 (mapCursorInsert @Int @Char)
    describe "mapCursorAppend" $
        it "produces valid map cursors" $
        producesValidsOnValids3 (mapCursorAppend @Int @Char)
    describe "mapCursorInsertAndSelect" $
        it "produces valid map cursors" $
        producesValidsOnValids3 (mapCursorInsertAndSelect @Int @Char)
    describe "mapCursorAppendAndSelect" $
        it "produces valid map cursors" $
        producesValidsOnValids3 (mapCursorAppendAndSelect @Int @Char)
    describe "mapCursorRemoveElemAndSelectPrev" $
        it "produces valid map cursors" $
        producesValidsOnValids (mapCursorRemoveElemAndSelectPrev @Int @Char)
    describe "mapCursorDeleteElemAndSelectNext" $
        it "produces valid map cursors" $
        producesValidsOnValids (mapCursorDeleteElemAndSelectNext @Int @Char)
    describe "mapCursorRemoveElem" $
        it "produces valid map cursors" $
        producesValidsOnValids (mapCursorRemoveElem @Int @Char)
    describe "mapCursorDeleteElem" $
        it "produces valid map cursors" $
        producesValidsOnValids (mapCursorDeleteElem @Int @Char)
    describe "keyCursorSelectValue" $
        it "is a movement" $
        forAllValid $ \kc -> do
            let vc = keyCursorSelectValue (kc :: KeyCursor Int Int)
            source (rebuild kc) `shouldBe` source (rebuild vc)
    describe "valueCursorSelectKey" $
        it "is a movement" $
        forAllValid $ \vc -> do
            let kc = valueCursorSelectKey (vc :: ValueCursor Int Int)
            source (rebuild kc) `shouldBe` source (rebuild vc)

isMovement :: (forall a b. MapCursor a b -> MapCursor a b) -> Property
isMovement func =
    forAllValid $ \mc ->
        rebuildMapCursor mc `shouldBe`
        rebuildMapCursor (func mc :: MapCursor Int Char)

isMovementM :: (forall a b. MapCursor a b -> Maybe (MapCursor a b)) -> Property
isMovementM func =
    forAllValid $ \mc ->
        case func (mc :: MapCursor Int Char) of
            Nothing -> pure () -- fine
            Just mc' -> rebuildMapCursor mc `shouldBe` rebuildMapCursor mc'

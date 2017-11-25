{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cursor.ListSpec
    ( spec
    ) where

import TestImport

import Cursor.List
import Cursor.List.Gen ()

spec :: Spec
spec = do
    describe "emptyListCursor" $
        it "is valid" $ shouldBeValid (emptyListCursor :: ListCursor Int)
    describe "rebuildListCursor" $
        it "is the inverse of makeListCursor" $
        inverseFunctions
            (makeListCursor :: [Int] -> ListCursor Int)
            rebuildListCursor
    describe "listCursorSelectPrev" $
        it "is a movement" $ isMovementM listCursorSelectPrev
    describe "listCursorSelectNext" $
        it "is a movement" $ isMovementM listCursorSelectNext
    describe "listCursorSelectPrevChar" $
        it "produces valids" $
        producesValidsOnValids
            (listCursorSelectPrevChar :: ListCursor Double -> Maybe Double)
    describe "listCursorSelectNextChar" $
        it "produces valids" $
        producesValidsOnValids
            (listCursorSelectNextChar :: ListCursor Double -> Maybe Double)
    describe "listCursorSelectStart" $
        it "is a movement" $ isMovement listCursorSelectStart
    describe "listCursorSelectEnd" $
        it "is a movement" $ isMovement listCursorSelectEnd
    describe "listCursorInsert" $
        it "produces valids" $
        forAllValid $ \d ->
            producesValidsOnValids
                (listCursorInsert d :: ListCursor Double -> ListCursor Double)
    describe "listCursorAppend" $
        it "produces valids" $
        forAllValid $ \d ->
            producesValidsOnValids
                (listCursorAppend d :: ListCursor Double -> ListCursor Double)
    describe "listCursorRemove" $
        it "produces valids" $
        validIfSucceedsOnValid
            (listCursorRemove :: ListCursor Double -> Maybe (ListCursor Double))
    describe "listCursorDelete" $
        it "produces valids" $
        validIfSucceedsOnValid
            (listCursorDelete :: ListCursor Double -> Maybe (ListCursor Double))
    describe "listCursorDelete" $
        it "produces valids" $
        producesValidsOnValids
            (listCursorSplit :: ListCursor Double -> ( ListCursor Double
                                                     , ListCursor Double))
    describe "listCursorCombine" $
        it "produces valids" $
        producesValidsOnValids2
            (listCursorCombine :: ListCursor Double -> ListCursor Double -> ListCursor Double)

isMovementM :: (forall a. ListCursor a -> Maybe (ListCursor a)) -> Property
isMovementM func =
    forAllValid $ \lec ->
        case func (lec :: ListCursor Int) of
            Nothing -> pure () -- Fine
            Just lec' ->
                let ne = rebuildListCursor lec
                    ne' = rebuildListCursor lec'
                in unless (ne == ne') $
                   expectationFailure $
                   unlines
                       [ "Cursor before:\n" ++ show lec
                       , "List before:  \n" ++ show ne
                       , "Cursor after: \n" ++ show lec'
                       , "List after:   \n" ++ show ne'
                       ]

isMovement :: (forall a. ListCursor a -> ListCursor a) -> Property
isMovement func =
    forAllValid $ \lec ->
        rebuildListCursor (lec :: ListCursor Int) `shouldBe`
        rebuildListCursor (func lec)

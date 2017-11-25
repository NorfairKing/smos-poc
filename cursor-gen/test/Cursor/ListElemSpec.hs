{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cursor.ListElemSpec
    ( spec
    ) where

import TestImport

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)

import Cursor.Class
import Cursor.ListElem
import Cursor.ListElem.Gen ()

spec :: Spec
spec = do
    describe "nonemptyPrepend" $
        it "is equivalent to regular prepend" $
        equivalentWhenFirstSucceeds
            (\(ls1, ls2) ->
                 (NE.toList . nonemptyPrepend ls1) <$> NE.nonEmpty ls2)
            (uncurry (++) :: ([Int], [Int]) -> [Int])
    describe "nonemptyAppend" $
        it "is equivalent to regular append" $
        equivalentWhenFirstSucceeds
            (\(ls1, ls2) ->
                 (NE.toList . (`nonemptyAppend` ls2)) <$> NE.nonEmpty ls1)
            (uncurry (++) :: ([Int], [Int]) -> [Int])
    describe "rebuildListElemCursor" $ do
        it "is the inverse of makeListElemCursor for integers" $
            inverseFunctions
                (makeListElemCursor :: NonEmpty Int -> ListElemCursor Int)
                rebuildListElemCursor
        it "is the inverse of makeNonEmptyListElemCursor for integers" $
            inverseFunctionsIfFirstSucceeds
                (makeNonEmptyListElemCursor :: [Int] -> Maybe (ListElemCursor Int))
                (NE.toList . rebuildListElemCursor)
        it
            "is the inverse of makeListElemCursorWithSelection for integers, for any index" $
            forAll genUnchecked $ \i ->
                inverseFunctions
                    (makeListElemCursorWithSelection i :: NonEmpty Int -> ListElemCursor Int)
                    rebuildListElemCursor
    describe "listElemCursorSelectPrev" $
        it "is a movement" $ isMovementM listElemCursorSelectPrev
    describe "listElemCursorSelectNext" $
        it "is a movement" $ isMovementM listElemCursorSelectNext
    describe "listElemCursorSelectFirst" $
        it "is a movement" $ isMovement listElemCursorSelectFirst
    describe "listElemCursorSelectLast" $
        it "is a movement" $ isMovement listElemCursorSelectLast
    describe "listElemCursorInsert" $ do
        it "produces valids" $
            forAllValid $ \d ->
                producesValidsOnValids
                    (listElemCursorInsert d :: ListElemCursor Double -> ListElemCursor Double)
        it "inserts a character before the cursor" $
            forAllValid $ \c ->
                forAllValid $ \lc ->
                    let lc' = listElemCursorInsert c lc
                        lv = rebuild lc
                        lv' = rebuild lc'
                    in listElemViewPrev lv ++
                       [c :: Int] `shouldBe` listElemViewPrev lv'
    describe "listElemCursorAppend" $ do
        it "produces valids" $
            forAllValid $ \d ->
                producesValidsOnValids
                    (listElemCursorAppend d :: ListElemCursor Double -> ListElemCursor Double)
        it "inserts a character after the cursor" $
            forAllValid $ \c ->
                forAllValid $ \lc ->
                    let lc' = listElemCursorAppend c lc
                        lv = rebuild lc
                        lv' = rebuild lc'
                    in (c :: Int) :
                       listElemViewNext lv `shouldBe` listElemViewNext lv'
    describe "listElemCursorInsertAndSelect" $ do
        it "produces valids" $
            forAllValid $ \d ->
                producesValidsOnValids
                    (listElemCursorInsertAndSelect d :: ListElemCursor Double -> ListElemCursor Double)
        it "inserts a character before the cursor and selects it" $
            forAllValid $ \c ->
                forAllValid $ \lc ->
                    let lc' = listElemCursorInsertAndSelect c lc
                        lv = rebuild lc
                        lv' = rebuild lc'
                    in do listElemViewCurrent lv' `shouldBe` (c :: Int)
                          listElemViewNext lv' `shouldBe` listElemViewCurrent lv :
                              listElemViewNext lv
    describe "listElemCursorAppendAndSelect" $ do
        it "produces valids" $
            forAllValid $ \d ->
                producesValidsOnValids
                    (listElemCursorAppendAndSelect d :: ListElemCursor Double -> ListElemCursor Double)
        it "appends a character before the cursor and selects it" $
            forAllValid $ \c ->
                forAllValid $ \lc ->
                    let lc' = listElemCursorAppendAndSelect c lc
                        lv = rebuild lc
                        lv' = rebuild lc'
                    in do listElemViewCurrent lv' `shouldBe` (c :: Int)
                          listElemViewPrev lv' `shouldBe` listElemViewPrev lv ++
                              [listElemViewCurrent lv]

isMovementM ::
       (forall a. ListElemCursor a -> Maybe (ListElemCursor a)) -> Property
isMovementM func =
    forAllValid $ \lec ->
        case func (lec :: ListElemCursor Int) of
            Nothing -> pure () -- Fine
            Just lec' ->
                let ne = rebuildListElemCursor lec
                    ne' = rebuildListElemCursor lec'
                in unless (ne == ne') $
                   expectationFailure $
                   unlines
                       [ "Cursor before:\n" ++ show lec
                       , "List before:  \n" ++ show ne
                       , "Cursor after: \n" ++ show lec'
                       , "List after:   \n" ++ show ne'
                       ]

isMovement :: (forall a. ListElemCursor a -> ListElemCursor a) -> Property
isMovement func =
    forAllValid $ \lec ->
        rebuildListElemCursor (lec :: ListElemCursor Int) `shouldBe`
        rebuildListElemCursor (func lec)

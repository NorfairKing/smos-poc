{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cursor.TextSpec
    ( spec
    ) where

import TestImport

import qualified Data.Text as T

import Cursor.Class
import Cursor.TestUtils
import Cursor.Text
import Cursor.Text.Gen ()

spec :: Spec
spec = do
    describe "makeTextCursor" $ do
        it "is the inverse of 'rebuildTextCursor' for this simple example" $
            rebuildTextCursor (makeTextCursor "abc") `shouldBe` "abc"
        it "is the inverse of 'rebuildTextCursor'" $
            inverseFunctionsOnValid makeTextCursor rebuildTextCursor
    describe "reselect" $
        it "reselects to the same selection" $
        reselectsToTheSameSelection @TextCursor
    describe "textCursorSelectPrev" $ do
        it "is a movement" $ isMovementM textCursorSelectPrev
        it "is a movement when applied twice" $
            isMovementM (textCursorSelectPrev >=> textCursorSelectPrev)
    describe "textCursorSelectNext" $ do
        it "is a movement" $ isMovementM textCursorSelectNext
        it "is a movement when applied twice" $
            isMovementM (textCursorSelectNext >=> textCursorSelectNext)
    describe "textCursorSelectIndex" $
        it "is a movement" $
        forAllUnchecked $ \ix_ -> isMovement (textCursorSelectIndex ix_)
    describe "textCursorSelectStart" $
        it "is a movement" $ isMovement textCursorSelectStart
    describe "textCursorSelectEnd" $
        it "is a movement" $ isMovement textCursorSelectEnd
    describe "textCursorInsert" $ do
        it "rebuilds to the right character when inserting into an empty cursor" $
            forAll genValid $ \c ->
                rebuildTextCursor (textCursorInsert c emptyTextCursor) `shouldBe`
                T.pack [c]
        it
            "rebuilds to the right two character when inserting into an empty cursor twice" $
            forAllValid $ \(c1, c2) ->
                let tc = emptyTextCursor
                    tc' = textCursorInsert c1 tc
                    tc'' = textCursorInsert c2 tc'
                    t' = rebuildTextCursor tc''
                in unless (t' == T.pack [c1, c2]) $
                   expectationFailure $
                   unlines
                       [ "Initial text: " ++ show T.empty
                       , "built text cursor: " ++ show tc
                       , "changed text cursor after first insertion: " ++
                         show tc'
                       , "changed text cursor after second insertion: " ++
                         show tc''
                       , "Final text: " ++ show t'
                       ]
        it "rebuilds to a text that is one longer" $
            forAllValid $ \(t, c) ->
                let tc = makeTextCursor t
                    tc' = textCursorInsert c tc
                    t' = rebuildTextCursor tc'
                in T.length t' `shouldBe` T.length t + 1
        it "builds to the inserted character" $
            forAllValid $ \(t, c) ->
                let tc = makeTextCursor t
                in build (textCursorInsert c tc) `shouldBe` Just c

isMovementM :: (TextCursor -> Maybe TextCursor) -> Property
isMovementM func =
    forAllValid $ \tc ->
        case func tc of
            Nothing -> pure ()
            Just tc' ->
                let t = rebuildTextCursor tc
                    t' = rebuildTextCursor tc'
                in unless (t' == t) $
                   expectationFailure $
                   unlines
                       [ "Initial Text: " ++ show t
                       , "Built cursor: " ++ show tc
                       , "Changed cursor: " ++ show tc'
                       , "Final Text: " ++ show t'
                       ]

isMovement :: (TextCursor -> TextCursor) -> Property
isMovement func =
    forAllValid $ \tc ->
        let t = rebuildTextCursor tc
            tc' = func tc
            t' = rebuildTextCursor tc'
        in unless (t' == t) $
           expectationFailure $
           unlines
               [ "Initial Text: " ++ show t
               , "Built cursor: " ++ show tc
               , "Changed cursor: " ++ show tc'
               , "Final Text: " ++ show t'
               ]

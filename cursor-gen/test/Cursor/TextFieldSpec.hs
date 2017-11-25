{-# LANGUAGE OverloadedStrings #-}

module Cursor.TextFieldSpec
    ( spec
    ) where

import TestImport

import Data.Text (Text)
import qualified Data.Text as T

import Cursor.Class
import Cursor.TextField
import Cursor.TextField.Gen ()

spec :: Spec
spec = do
    describe "emptyTextFieldCursor" $
        it "is valid" $ shouldBeValid emptyTextFieldCursor
    describe "makeTextFieldCursor" $ do
        it "is the inverse of 'rebuildTextFieldCursor' for this simple example" $
            rebuildTextFieldCursor (makeTextFieldCursor "abc\ndef") `shouldBe`
            "abc\ndef"
        it
            "is the inverse of 'rebuildTextFieldCursor' for this simple example ending in a newline" $
            rebuildTextFieldCursor (makeTextFieldCursor "abc\ndef\n") `shouldBe`
            "abc\ndef\n"
        it "is the inverse of 'rebuildTextFieldCursor'" $
            inverseFunctionsOnValid makeTextFieldCursor rebuildTextFieldCursor
    describe "view" $
        it "is the inverse of 'source" $
        inverseFunctionsOnValid view (source :: TextFieldView -> Text)
    describe "textFieldCursorSelectPrev" $ do
        it "is a line movement" $ isLineMovementM textFieldCursorSelectPrev
        it "is a line movement when applied twice" $
            isLineMovementM
                (textFieldCursorSelectPrev >=> textFieldCursorSelectPrev)
        it "is a movement" $ isMovementM textFieldCursorSelectPrev
        it "is a movement when applied twice" $
            isMovementM
                (textFieldCursorSelectPrev >=> textFieldCursorSelectPrev)
    describe "textFieldCursorSelectNext" $ do
        it "is a line movement" $ isLineMovementM textFieldCursorSelectNext
        it "is a line movement when applied twice" $
            isLineMovementM
                (textFieldCursorSelectNext >=> textFieldCursorSelectNext)
        it "is a movement" $ isMovementM textFieldCursorSelectNext
        it "is a movement when applied twice" $
            isMovementM
                (textFieldCursorSelectNext >=> textFieldCursorSelectNext)
    describe "textFieldCursorSelectIndex" $ do
        it "is a line movement" $
            forAllUnchecked $ \ix_ ->
                isLineMovement (textFieldCursorSelectIndex ix_)
        it "is a movement" $
            forAllUnchecked $ \ix_ ->
                isMovement (textFieldCursorSelectIndex ix_)
    describe "textFieldCursorSelectUp" $ do
        it "is a movement" $ isMovementM textFieldCursorSelectUp
        it "is a movement when applied twice" $
            isMovementM (textFieldCursorSelectUp >=> textFieldCursorSelectUp)
    describe "textFieldCursorSelectDown" $ do
        it "is a movement" $ isMovementM textFieldCursorSelectDown
        it "is a movement when applied twice" $
            isMovementM
                (textFieldCursorSelectDown >=> textFieldCursorSelectDown)
    describe "textCursorInsert" $ do
        it "builds to the right character when inserting into an empty cursor" $
            forAll genValid $ \c ->
                (source . build) (textFieldCursorInsert c emptyTextFieldCursor) `shouldBe`
                T.pack [c]
        it
            "builds to the right two character when inserting into an empty cursor twice" $
            forAll genValid $ \(c1, c2) ->
                let tc = emptyTextFieldCursor
                    tc' = textFieldCursorInsert c1 tc
                    tc'' = textFieldCursorInsert c2 tc'
                    t' = rebuildTextFieldCursor tc''
                in unless (t' == T.pack [c1, c2]) $
                   expectationFailure $
                   unlines
                       [ "Initial text: " ++ show T.empty
                       , "built textfield cursor: " ++ show tc
                       , "changed text cursor after first insertion: " ++
                         show tc'
                       , "changed text cursor after second insertion: " ++
                         show tc''
                       , "Final text: " ++ show t'
                       ]
    describe "textFieldCursorSelectStart" $ do
        it "is a line movement" $ isLineMovement textFieldCursorSelectStart
        it "is a line movement when applied twice" $
            isLineMovement
                (textFieldCursorSelectStart . textFieldCursorSelectStart)
        it "is a movement" $ isMovement textFieldCursorSelectStart
        it "is a movement when applied twice" $
            isMovement (textFieldCursorSelectStart . textFieldCursorSelectStart)
    describe "textFieldCursorSelectEnd" $ do
        it "is a line movement" $ isLineMovement textFieldCursorSelectEnd
        it "is a line movement when applied twice" $
            isLineMovement (textFieldCursorSelectEnd . textFieldCursorSelectEnd)
        it "is a movement" $ isMovement textFieldCursorSelectEnd
        it "is a movement when applied twice" $
            isMovement (textFieldCursorSelectEnd . textFieldCursorSelectEnd)

isLineMovementM :: (TextFieldCursor -> Maybe TextFieldCursor) -> Property
isLineMovementM func =
    forAll genValid $ \tc ->
        case func tc of
            Nothing -> pure ()
            Just tc' ->
                let t = source $ build tc
                    t' = source $ build tc'
                in unless (t' == t) $
                   expectationFailure $
                   unlines
                       [ "Initial Text: " ++ show t
                       , "Built cursor: " ++ show tc
                       , "Changed cursor: " ++ show tc'
                       , "Final Text: " ++ show t'
                       ]

isLineMovement :: (TextFieldCursor -> TextFieldCursor) -> Property
isLineMovement func =
    forAll genValid $ \tc ->
        let t = source $ build tc
            tc' = func tc
            t' = source $ build tc'
        in unless (t' == t) $
           expectationFailure $
           unlines
               [ "Initial Text: " ++ show t
               , "Built cursor: " ++ show tc
               , "Changed cursor: " ++ show tc'
               , "Final Text: " ++ show t'
               ]

isMovementM :: (TextFieldCursor -> Maybe TextFieldCursor) -> Property
isMovementM func =
    forAll genValid $ \tc ->
        case func tc of
            Nothing -> pure ()
            Just tc' ->
                let t = rebuildTextFieldCursor tc
                    t' = rebuildTextFieldCursor tc'
                in unless (t' == t) $
                   expectationFailure $
                   unlines
                       [ "Initial Text: " ++ show t
                       , "Built cursor: " ++ show tc
                       , "Changed cursor: " ++ show tc'
                       , "Final Text: " ++ show t'
                       ]

isMovement :: (TextFieldCursor -> TextFieldCursor) -> Property
isMovement func =
    forAll genValid $ \tc ->
        let t = rebuildTextFieldCursor tc
            tc' = func tc
            t' = rebuildTextFieldCursor tc'
        in unless (t' == t) $
           expectationFailure $
           unlines
               [ "Initial Text: " ++ show t
               , "Built cursor: " ++ show tc
               , "Changed cursor: " ++ show tc'
               , "Final Text: " ++ show t'
               ]

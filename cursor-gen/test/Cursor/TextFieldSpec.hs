{-# LANGUAGE OverloadedStrings #-}

module Cursor.TextFieldSpec
    ( spec
    ) where

import TestImport

import Data.Text (Text)
import qualified Data.Text as T

import Cursor.Class
import Cursor.TestUtils
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
    describe "view" $ do
        it "is the inverse of 'source" $
            inverseFunctionsOnValid view (source :: TextFieldView -> Text)
    describe "textFieldCursorSelectPrev" $ do
        it "builds to the same text" $
            buildsToTheSameIfSuceeds textFieldCursorSelectPrev
        it "builds to the same text when applied twice" $
            buildsToTheSameIfSuceeds
                (textFieldCursorSelectPrev >=> textFieldCursorSelectPrev)
        it "rebuilds to the same text" $
            rebuildsToTheSameTextIfSucceeds textFieldCursorSelectPrev
        it "rebuilds to the same text when applied twice" $
            rebuildsToTheSameTextIfSucceeds
                (textFieldCursorSelectPrev >=> textFieldCursorSelectPrev)
    describe "textFieldCursorSelectNext" $ do
        it "builds to the same text" $
            buildsToTheSameIfSuceeds textFieldCursorSelectNext
        it "builds to the same text when applied twice" $
            buildsToTheSameIfSuceeds
                (textFieldCursorSelectNext >=> textFieldCursorSelectNext)
        it "rebuilds to the same text" $
            rebuildsToTheSameTextIfSucceeds textFieldCursorSelectNext
        it "rebuilds to the same text when applied twice" $
            rebuildsToTheSameTextIfSucceeds
                (textFieldCursorSelectNext >=> textFieldCursorSelectNext)
    describe "textFieldCursorSelectUp" $ do
        it "rebuilds to the same text" $
            rebuildsToTheSameTextIfSucceeds textFieldCursorSelectUp
        it "rebuilds to the same text when applied twice" $
            rebuildsToTheSameTextIfSucceeds
                (textFieldCursorSelectUp >=> textFieldCursorSelectUp)
    describe "textFieldCursorSelectDown" $ do
        it "rebuilds to the same text" $
            rebuildsToTheSameTextIfSucceeds textFieldCursorSelectDown
        it "rebuilds to the same text when applied twice" $
            rebuildsToTheSameTextIfSucceeds
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
        it "builds to the same text" $
            buildsToTheSame textFieldCursorSelectStart
        it "builds to the same text when applied twice" $
            buildsToTheSame
                (textFieldCursorSelectStart . textFieldCursorSelectStart)
        it "rebuilds to the same text" $
            rebuildsToTheSameText textFieldCursorSelectStart
        it "rebuilds to the same text when applied twice" $
            rebuildsToTheSameText
                (textFieldCursorSelectStart . textFieldCursorSelectStart)
    describe "textFieldCursorSelectEnd" $ do
        it "builds to the same text" $ buildsToTheSame textFieldCursorSelectEnd
        it "builds to the same text when applied twice" $
            buildsToTheSame
                (textFieldCursorSelectEnd . textFieldCursorSelectEnd)
        it "rebuilds to the same text" $
            rebuildsToTheSameText textFieldCursorSelectEnd
        it "rebuilds to the same text when applied twice" $
            rebuildsToTheSameText
                (textFieldCursorSelectEnd . textFieldCursorSelectEnd)

rebuildsToTheSameText :: (TextFieldCursor -> TextFieldCursor) -> Property
rebuildsToTheSameText func =
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

rebuildsToTheSameTextIfSucceeds ::
       (TextFieldCursor -> Maybe TextFieldCursor) -> Property
rebuildsToTheSameTextIfSucceeds func =
    forAll genValid $ \tc ->
        let t = rebuildTextFieldCursor tc
            mtc' = func tc
        in case mtc' of
               Nothing -> pure ()
               Just tc' ->
                   let t' = rebuildTextFieldCursor tc'
                   in unless (t' == t) $
                      expectationFailure $
                      unlines
                          [ "Initial Text: " ++ show t
                          , "Built cursor: " ++ show tc
                          , "Changed cursor: " ++ show tc'
                          , "Final Text: " ++ show t'
                          ]

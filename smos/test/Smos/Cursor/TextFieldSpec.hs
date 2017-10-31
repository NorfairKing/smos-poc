{-# LANGUAGE OverloadedStrings #-}

module Smos.Cursor.TextFieldSpec
    ( spec
    ) where

import TestImport

import Smos.Cursor.TestUtils
import Smos.Cursor.TextField
import Smos.Cursor.TextField.Gen ()

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
        it "is the inverse of 'rebuildTextCursor'" $
            inverseFunctionsOnValid makeTextFieldCursor rebuildTextFieldCursor
    describe "textCursorSelectPrev" $ do
        it "builds to the same text" $
            buildsToTheSameIfSuceeds textFieldCursorSelectPrev
        it "builds to the same text when applied twice" $
            buildsToTheSameIfSuceeds
                (textFieldCursorSelectPrev >=> textFieldCursorSelectPrev)
        it "rebuilds to the same text" $
            rebuildsToTheSameIfSuceeds textFieldCursorSelectPrev
        it "rebuilds to the same text when applied twice" $
            rebuildsToTheSameIfSuceeds
                (textFieldCursorSelectPrev >=> textFieldCursorSelectPrev)
    describe "textCursorSelectNext" $ do
        it "builds to the same text" $
            buildsToTheSameIfSuceeds textFieldCursorSelectNext
        it "builds to the same text when applied twice" $
            buildsToTheSameIfSuceeds
                (textFieldCursorSelectNext >=> textFieldCursorSelectNext)
        it "rebuilds to the same text" $
            rebuildsToTheSameIfSuceeds textFieldCursorSelectNext
        it "rebuilds to the same text when applied twice" $
            rebuildsToTheSameIfSuceeds
                (textFieldCursorSelectNext >=> textFieldCursorSelectNext)

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
        it "rebuilds to the same text" $
            rebuildsToTheSameIfSuceeds textCursorSelectPrev
        it "rebuilds to the same text when applied twice" $
            rebuildsToTheSameIfSuceeds
                (textCursorSelectPrev >=> textCursorSelectPrev)
    describe "textCursorSelectNext" $ do
        it "rebuilds to the same text" $
            rebuildsToTheSameIfSuceeds textCursorSelectNext
        it "rebuilds to the same text when applied twice" $
            rebuildsToTheSameIfSuceeds
                (textCursorSelectNext >=> textCursorSelectNext)
    describe "textCursorSelectStart" $
        it "rebuilds to the same text" $ rebuildsToTheSame textCursorSelectStart
    describe "textCursorSelectEnd" $
        it "rebuilds to the same text" $ rebuildsToTheSame textCursorSelectEnd
    describe "textCursorInsert" $ do
        it "rebuilds to the right character when inserting into an empty cursor" $
            forAll genValid $ \c ->
                rebuildTextCursor (textCursorInsert c emptyTextCursor) `shouldBe`
                T.pack [c]
        it
            "rebuilds to the right two character when inserting into an empty cursor twice" $
            forAll genValid $ \(c1, c2) ->
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
            forAll genValid $ \(t, c) ->
                let tc = makeTextCursor t
                    tc' = textCursorInsert c tc
                    t' = rebuildTextCursor tc'
                in T.length t' `shouldBe` T.length t + 1
        it "builds to the inserted character" $
            forAll genValid $ \(t, c) ->
                let tc = makeTextCursor t
                in build (textCursorInsert c tc) `shouldBe` Just c

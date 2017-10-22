{-# LANGUAGE OverloadedStrings #-}

module Smos.TextCursorSpec
    ( spec
    ) where

import TestImport

import qualified Data.Text as T

import Smos.TextCursor

spec :: Spec
spec = do
    describe "makeTextCursor" $ do
        it "is the inverse of 'rebuildTextCursor' for this simple example" $
            rebuildTextCursor (makeTextCursor "abc") `shouldBe` "abc"
        it "is the inverse of 'rebuildTextCursor'" $
            inverseFunctionsOnValid makeTextCursor rebuildTextCursor
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
        it "rebuilds to a text that is one longer" $ do
            forAll genValid $ \(t, c) ->
                let tc = makeTextCursor t
                    tc' = textCursorInsert c tc
                    t' = rebuildTextCursor tc'
                in T.length t' `shouldBe` T.length t + 1

rebuildsToTheSame :: (TextCursor -> TextCursor) -> Property
rebuildsToTheSame func =
    forAll genValid $ \t ->
        let tc = makeTextCursor t
            tc' = func tc
            t' = rebuildTextCursor tc'
        in unless (t' == t) $
           expectationFailure $
           unlines
               [ "Initial text: " ++ show t
               , "built text cursor: " ++ show tc
               , "changed text cursor: " ++ show tc'
               , "Final text: " ++ show t'
               ]

rebuildsToTheSameIfSuceeds :: (TextCursor -> Maybe TextCursor) -> Property
rebuildsToTheSameIfSuceeds func =
    forAll genValid $ \t ->
        let tc = makeTextCursor t
            mtc' = func tc
        in case mtc' of
               Nothing -> pure ()
               Just tc' ->
                   let t' = rebuildTextCursor tc'
                   in unless (rebuildTextCursor tc == t) $
                      expectationFailure $
                      unlines
                          [ "Initial text: " ++ show t
                          , "built text cursor: " ++ show tc
                          , "changed text cursor: " ++ show tc'
                          , "Final text: " ++ show t'
                          ]

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.CursorSpec
    ( spec
    ) where

import TestImport

import Cursor.Tree

import Smos.Cursor
import Smos.Cursor.Gen ()
import Smos.Data.Gen ()

{-# ANN module ("HLint: ignore Functor law" :: String) #-}

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec =
    describe "ACursor" $ do
        describe "selection" $ do
            it "returns the empty selection in a forest without a parent" $
                forAll genValid $ \sf ->
                    let fc = makeForestCursor' sf :: ForestCursor EntryCursor
                        cur = AnyForest fc
                    in selection cur `shouldBe` []
            it "returns the index of the tree we zoom in on" $ do
                let gen = do
                        sf <- genValid
                        let fc =
                                makeForestCursor' sf :: ForestCursor EntryCursor
                        case forestCursorElems fc of
                            [] -> scale (+ 1) gen
                            els -> elements els
                forAll gen $ \tc ->
                    let cur = AnyTree tc
                    in selection cur `shouldBe` [treeCursorIndex tc]
            it "returns the index of the tree we zoom in on, then a 1" $ do
                let gen = do
                        sf <- genValid
                        let fc =
                                makeForestCursor' sf :: ForestCursor EntryCursor
                        case forestCursorElems fc of
                            [] -> scale (+ 1) gen
                            els -> do
                                tc <- elements els
                                pure (treeCursorForest tc, treeCursorIndex tc)
                forAll gen $ \(fc, ix_) ->
                    let cur = AnyForest fc
                    in selection cur `shouldBe` [1, ix_]
        describe "reselectCursor" $ do
            it "selects the top level forrest for an empty list" $
                forAll genValid $ \sf ->
                    reselectCursor [] sf `shouldBe` makeAnyCursor sf
            it "selects the tree with the right index for a singleton selection" $ do
                let gen = do
                        sf <- genValid
                        let fc =
                                makeForestCursor' sf :: ForestCursor EntryCursor
                        case forestCursorElems fc of
                            [] -> scale (+ 1) gen
                            els -> elements els
                forAll gen $ \tc ->
                    reselectCursor
                        [treeCursorIndex tc]
                        (source $ SmosFileView $ rebuild tc) `shouldBe`
                    AnyTree tc
            it "returns the index of the tree we zoom in on, then a 1" $ do
                let gen = do
                        sf <- genValid
                        let fc =
                                makeForestCursor' sf :: ForestCursor EntryCursor
                        case forestCursorElems fc of
                            [] -> scale (+ 1) gen
                            els -> do
                                tc <- elements els
                                pure (treeCursorForest tc, treeCursorIndex tc)
                forAll gen $ \(fc, ix_) ->
                    reselectCursor [1, ix_] (source $ SmosFileView $ rebuild fc) `shouldBe`
                    AnyForest fc
            it "selects the cursor that was handed to selection" $
                forAll genValid $ \ac ->
                    let sel = selection ac
                        sf = source $ rebuild ac
                    in reselectCursor sel sf `shouldBe` ac

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.CursorSpec
    ( spec
    ) where

import TestImport

import qualified Data.Text as T

import Lens.Micro

import Smos.Cursor
import Smos.Cursor.Gen ()
import Smos.Cursor.TestUtils
import Smos.Cursor.Tree
import Smos.Data
import Smos.Data.Gen ()

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

{-# ANN module ("HLint: ignore Functor law" :: String) #-}

spec :: Spec
spec = do
    describe "ACursor" $ do
        describe "selection" $ do
            it "returns the empty selection in a forest without a parent" $
                forAll genValid $ \sf ->
                    let fc = makeForestCursor sf :: ForestCursor EntryCursor
                        cur = AnyForest fc
                    in selection cur `shouldBe` []
            it "returns the index of the tree we zoom in on" $ do
                let gen = do
                        sf <- genValid
                        let fc = makeForestCursor sf :: ForestCursor EntryCursor
                        case forestCursorElems fc of
                            [] -> scale (+ 1) gen
                            els -> elements els
                forAll gen $ \tc ->
                    let cur = AnyTree tc
                    in selection cur `shouldBe` [treeCursorIndex tc]
            it "returns the index of the tree we zoom in on, then a 1" $ do
                let gen = do
                        sf <- genValid
                        let fc = makeForestCursor sf :: ForestCursor EntryCursor
                        case forestCursorElems fc of
                            [] -> scale (+ 1) gen
                            els -> do
                                tc <- elements els
                                pure (treeCursorForest tc, treeCursorIndex tc)
                forAll gen $ \(fc, ix_) ->
                    let cur = AnyForest fc
                    in selection cur `shouldBe` [1, ix_]
        describe "reselect" $ do
            it "selects the top level forrest for an empty list" $
                forAll genValid $ \sf ->
                    reselect [] sf `shouldBe` makeAnyCursor sf
            it "selects the tree with the right index for a singleton selection" $ do
                let gen = do
                        sf <- genValid
                        let fc = makeForestCursor sf :: ForestCursor EntryCursor
                        case forestCursorElems fc of
                            [] -> scale (+ 1) gen
                            els -> elements els
                forAll gen $ \tc ->
                    reselect [treeCursorIndex tc] (SmosFile $ rebuild tc) `shouldBe`
                    AnyTree tc
            it "returns the index of the tree we zoom in on, then a 1" $ do
                let gen = do
                        sf <- genValid
                        let fc = makeForestCursor sf :: ForestCursor EntryCursor
                        case forestCursorElems fc of
                            [] -> scale (+ 1) gen
                            els -> do
                                tc <- elements els
                                pure (treeCursorForest tc, treeCursorIndex tc)
                forAll gen $ \(fc, ix_) ->
                    reselect [1, ix_] (SmosFile $ rebuild fc) `shouldBe`
                    AnyForest fc
            it "selects the cursor that was handed to selection" $
                forAll genValid $ \ac ->
                    let sel = selection ac
                        sf = rebuild ac
                    in reselect sel sf `shouldBe` ac
    describe "EntryCursor" $ do
        describe "entryCursorParent" $
            it "rebuilds to the same" $ rebuildsToTheSame entryCursorParent
        describe "entryCursorHeader" $
            it "rebuilds to the same" $ rebuildsToTheSame entryCursorHeader
        describe "entryCursorContents" $
            it "rebuilds to the same" $
            rebuildsToTheSameIfSuceeds entryCursorContents
        describe "entryCursorState" $
            it "rebuilds to the same" $ rebuildsToTheSame entryCursorState
        describe "entryCursorHeaderL and entryCursorStateL" $
            it
                "has the same state after setting the state and then changing the header" $
            forAll genValid $ \ts ->
                forAll genValid $ \c ->
                    forAll genValid $ \ec ->
                        let ec' =
                                ec & entryCursorStateL %~ stateCursorSetState ts
                            hc = entryCursorHeader ec'
                            hc' = headerCursorInsert c hc
                            ec'' = headerCursorParent hc'
                        in build (entryCursorState ec'') `shouldBe`
                           build (entryCursorState ec')
        describe "entryCursorHeaderL and entryCursorContentsL" $
            it
                "has the same contents after setting the contents and then changing the header" $
            forAll genValid $ \cs ->
                forAll genValid $ \c ->
                    forAll genValid $ \ec ->
                        let ec' =
                                ec & entryCursorContentsL %~
                                fmap (contentsCursorSetContents cs)
                            hc = entryCursorHeader ec'
                            hc' = headerCursorInsert c hc
                            ec'' = headerCursorParent hc'
                        in build <$> entryCursorContents ec'' `shouldBe` build <$>
                           entryCursorContents ec'
        describe "entryCursorStateL and entryCursorContentsL" $ do
            it
                "has the same contents after setting the contents and then changing the state" $
                forAll genValid $ \cs ->
                    forAll genValid $ \ts ->
                        forAll genValid $ \ec ->
                            let ec' =
                                    ec & entryCursorContentsL %~
                                    fmap (contentsCursorSetContents cs)
                                sc = entryCursorState ec'
                                sc' = stateCursorSetState ts sc
                                ec'' = stateCursorParent sc'
                            in build <$>
                               entryCursorContents ec'' `shouldBe` build <$>
                               entryCursorContents ec'
            it
                "has the same state after setting the state and then changing the contents" $
                forAll genValid $ \ts ->
                    forAll genValid $ \cs ->
                        forAll genValid $ \ec ->
                            let ec' =
                                    ec & entryCursorStateL %~
                                    stateCursorSetState ts
                            in case entryCursorContents ec' of
                                   Nothing -> pure () -- nevermind
                                   Just cc ->
                                       let cc' = contentsCursorSetContents cs cc
                                           ec'' = contentsCursorParent cc'
                                       in build (entryCursorState ec'') `shouldBe`
                                          build (entryCursorState ec')
    describe "HeaderCursor" $ do
        describe "headerCursorParent" $
            it "rebuilds to the same" $ rebuildsToTheSame headerCursorParent
        describe "headerCursorInsert" $
            it "inserts a character at the front" $
            forAll genUnchecked $ \c ->
                forAll genValid $ \hc ->
                    build (headerCursorInsert c hc) `shouldBe`
                    Header (T.cons c (headerText (build hc)))
        describe "headerCursorRemove" $
            it "removes a character at the front" $
            forAll genValid $ \hc ->
                case headerCursorRemove hc of
                    Nothing -> pure ()
                    Just hc' ->
                        case T.uncons $ headerText (build hc') of
                            Nothing ->
                                expectationFailure "Something went wrong."
                            Just (_, t) -> t `shouldBe` headerText (build hc)
        describe "headerCursorDelete" $
            it "removes a character at the end" $
            forAll genValid $ \hc ->
                case headerCursorRemove hc of
                    Nothing -> pure ()
                    Just hc' ->
                        case T.uncons $ T.reverse $ headerText (build hc') of
                            Nothing ->
                                expectationFailure "Something went wrong."
                            Just (_, t) ->
                                T.reverse t `shouldBe` headerText (build hc)
        describe "headerCursorLeft" $
            it "rebuilds to the same" $
            rebuildsToTheSameIfSuceeds headerCursorLeft
        describe "headerCursorRight" $
            it "rebuilds to the same" $
            rebuildsToTheSameIfSuceeds headerCursorRight
        describe "headerCursorStart" $
            it "rebuilds to the same" $ rebuildsToTheSame headerCursorStart
        describe "headerCursorEnd" $
            it "rebuilds to the same" $ rebuildsToTheSame headerCursorEnd
    describe "ContentsCursor" $ do
        describe "contentsCursorParent" $
            it "rebuilds to the same" $ rebuildsToTheSame contentsCursorParent
        describe "contentsCursorSetContents" $
            it "builds to the given contents" $
            forAll genValid $ \cs ->
                forAll genValid $ \cc ->
                    build (contentsCursorSetContents cs cc) `shouldBe` cs
    describe "StateCursor" $ do
        describe "stateCursorParent" $
            it "rebuilds to the same" $ rebuildsToTheSame stateCursorParent
        describe "stateCursorClear" $
            it "builds to an empty state" $
            forAll genValid $ \sc ->
                build (stateCursorClear sc) `shouldBe` Nothing
        describe "stateCursorSetState" $
            it "builds to a state with the given contents" $
            forAll genValid $ \ts ->
                forAll genValid $ \sc ->
                    build (stateCursorSetState ts sc) `shouldBe` Just ts
    describe "TagsCursor" $
        describe "tagsCursorParent" $
        it "rebuilds to the same" $ rebuildsToTheSame tagsCursorParent
    describe "TagCursor" $ do
        describe "tagCursorParent" $
            it "rebuilds to the same" $ rebuildsToTheSame tagCursorParent
        describe "tagCursorLeft" $
            it "rebuilds to the same" $ rebuildsToTheSameIfSuceeds tagCursorLeft
        describe "tagCursorRight" $
            it "rebuilds to the same" $
            rebuildsToTheSameIfSuceeds tagCursorRight
        describe "tagCursorStart" $
            it "rebuilds to the same" $ rebuildsToTheSame tagCursorStart
        describe "tagCursorEnd" $
            it "rebuilds to the same" $ rebuildsToTheSame tagCursorEnd
        describe "tagCursorSelectPrev" $
            it "rebuilds to the same" $
            rebuildsToTheSameIfSuceeds tagCursorSelectNext
        describe "tagCursorSelectNext" $
            it "rebuilds to the same" $
            rebuildsToTheSameIfSuceeds tagCursorSelectNext

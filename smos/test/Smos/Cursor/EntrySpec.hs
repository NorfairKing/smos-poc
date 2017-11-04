{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.Cursor.EntrySpec
    ( spec
    ) where

import TestImport

import qualified Data.Text as T

import Lens.Micro

import Smos.Cursor
import Smos.Cursor.Entry
import Smos.Cursor.Gen ()
import Smos.Cursor.TestUtils
import Smos.Data
import Smos.Data.Gen ()

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

{-# ANN module ("HLint: ignore Functor law" :: String) #-}

spec :: Spec
spec = do
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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
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

sameCAfterSettingAAndThenBMM ::
       (Show c, Eq c)
    => EntryCursor
    -> (EntryCursor -> a)
    -> (a -> Maybe a)
    -> (a -> EntryCursor)
    -> (EntryCursor -> b)
    -> (b -> Maybe b)
    -> (b -> EntryCursor)
    -> (EntryCursor -> Maybe c)
    -> Expectation
sameCAfterSettingAAndThenBMM ec af am ag bf bm bg cf =
    let ac = af ec
        mac' = am ac
    in case mac' of
           Nothing -> pure ()
           Just ac' ->
               let ec' = ag ac'
                   bc = bf ec'
                   mbc' = bm bc
               in case mbc' of
                      Nothing -> pure ()
                      Just bc' ->
                          let ec'' = bg bc'
                          in cf ec' `shouldBe` cf ec''

sameCAfterSettingAAndThenB ::
       (Show c, Eq c)
    => EntryCursor
    -> (EntryCursor -> a)
    -> (a -> a)
    -> (a -> EntryCursor)
    -> (EntryCursor -> b)
    -> (b -> b)
    -> (b -> EntryCursor)
    -> (EntryCursor -> c)
    -> Expectation
sameCAfterSettingAAndThenB ec af am ag bf bm bg cf =
    sameCAfterSettingAAndThenBMM
        ec
        af
        (Just . am)
        ag
        bf
        (Just . bm)
        bg
        (Just . cf)

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
        describe "setting one after the other" $ do
            describe "state" $ do
                let stateTest ts now ec a b c =
                        sameCAfterSettingAAndThenB
                            ec
                            entryCursorState
                            (stateCursorSetState now ts)
                            stateCursorParent
                            a
                            b
                            c
                            (build . entryCursorState)
                describe "header" $
                    it
                        "has the same state after setting the state and then changing the header" $
                    forAll genValid $ \ts ->
                        forAll genValid $ \h ->
                            forAll genValid $ \ec ->
                                forAll genValid $ \now ->
                                    stateTest
                                        ts
                                        now
                                        ec
                                        entryCursorHeader
                                        (headerCursorSetHeader h)
                                        headerCursorParent
            describe "header" $ do
                let headerTest h ec a b c =
                        sameCAfterSettingAAndThenB
                            ec
                            entryCursorHeader
                            (headerCursorSetHeader h)
                            headerCursorParent
                            a
                            b
                            c
                            (build . entryCursorHeader)
                describe "state" $
                    it
                        "has the same header after setting the header and then changing the state" $
                    forAll genValid $ \ts ->
                        forAll genValid $ \h ->
                            forAll genValid $ \ec ->
                                forAll genValid $ \now ->
                                    headerTest
                                        h
                                        ec
                                        entryCursorState
                                        (stateCursorSetState now ts)
                                        stateCursorParent
                describe "tags" $
                    it
                        "has the same header after setting the header and then changing the tags" $
                    forAll genValid $ \h ->
                        forAll genValid $ \tgs ->
                            forAll genValid $ \ec ->
                                headerTest
                                    h
                                    ec
                                    entryCursorTags
                                    (tagsCursorSetTags tgs)
                                    tagsCursorParent
            describe "tags" $ do
                let headerTest tgs ec a b c =
                        sameCAfterSettingAAndThenB
                            ec
                            entryCursorTags
                            (tagsCursorSetTags tgs)
                            tagsCursorParent
                            a
                            b
                            c
                            (build . entryCursorState)
                describe "header" $
                    it
                        "has the same tags after setting the tags and then changing the header" $
                    forAll genValid $ \tgs ->
                        forAll genValid $ \h ->
                            forAll genValid $ \ec ->
                                headerTest
                                    tgs
                                    ec
                                    entryCursorHeader
                                    (headerCursorSetHeader h)
                                    headerCursorParent
        describe "entryCursorHeaderL and entryCursorContentsL" $ do
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
                            in build <$>
                               entryCursorContents ec'' `shouldBe` build <$>
                               entryCursorContents ec'
            it
                "has the same header after setting the header and then changing the contents" $
                forAll genValid $ \h ->
                    forAll genValid $ \cs ->
                        forAll genValid $ \ec ->
                            let ec' =
                                    ec & entryCursorHeaderL %~
                                    headerCursorSetHeader h
                            in case entryCursorContents ec' of
                                   Nothing -> pure () -- nevermind
                                   Just cc ->
                                       let cc' = contentsCursorSetContents cs cc
                                           ec'' = contentsCursorParent cc'
                                       in build (entryCursorHeader ec'') `shouldBe`
                                          build (entryCursorHeader ec')
        describe "entryCursorStateL and entryCursorContentsL" $ do
            it
                "has the same contents after setting the contents and then changing the state" $
                forAll genValid $ \cs ->
                    forAll genValid $ \ts ->
                        forAll genValid $ \ec ->
                            forAll genValid $ \now ->
                                let ec' =
                                        ec & entryCursorContentsL %~
                                        fmap (contentsCursorSetContents cs)
                                    sc = entryCursorState ec'
                                    sc' = stateCursorSetState now ts sc
                                    ec'' = stateCursorParent sc'
                                in build <$>
                                   entryCursorContents ec'' `shouldBe` build <$>
                                   entryCursorContents ec'
            it
                "has the same state after setting the state and then changing the contents" $
                forAll genValid $ \ts ->
                    forAll genValid $ \cs ->
                        forAll genValid $ \ec ->
                            forAll genValid $ \now ->
                                let ec' =
                                        ec & entryCursorStateL %~
                                        stateCursorSetState now ts
                                in case entryCursorContents ec' of
                                       Nothing -> pure () -- nevermind
                                       Just cc ->
                                           let cc' =
                                                   contentsCursorSetContents
                                                       cs
                                                       cc
                                               ec'' = contentsCursorParent cc'
                                           in build (entryCursorState ec'') `shouldBe`
                                              build (entryCursorState ec')
        describe "entryCursorContents and entryCursorTagsL" $ do
            it
                "has the same contents after setting the contents and then changing the tags" $
                forAll genValid $ \cs ->
                    forAll genValid $ \tgs ->
                        forAll genValid $ \ec ->
                            let ec' =
                                    ec & entryCursorContentsL %~
                                    fmap (contentsCursorSetContents cs)
                                tsc = entryCursorTags ec'
                                tsc' = tagsCursorSetTags tgs tsc
                                ec'' = tagsCursorParent tsc'
                            in build <$>
                               entryCursorContents ec'' `shouldBe` build <$>
                               entryCursorContents ec'
            it
                "has the same tags after setting the tags and then changing the contents" $
                forAll genValid $ \tgs ->
                    forAll genValid $ \cs ->
                        forAll genValid $ \ec ->
                            let ec' =
                                    ec & entryCursorTagsL %~
                                    tagsCursorSetTags tgs
                            in case entryCursorContents ec' of
                                   Nothing -> pure () -- nevermind
                                   Just cc ->
                                       let cc' = contentsCursorSetContents cs cc
                                           ec'' = contentsCursorParent cc'
                                       in build (entryCursorTags ec'') `shouldBe`
                                          build (entryCursorTags ec')
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
                forAll genValid $ \now ->
                    stateHistoryState (build (stateCursorClear now sc)) `shouldBe`
                    Nothing
        describe "stateCursorSetState" $
            it "builds to a state with the given contents" $
            forAll genValid $ \ts ->
                forAll genValid $ \sc ->
                    forAll genValid $ \now ->
                        stateHistoryState
                            (build (stateCursorSetState now ts sc)) `shouldBe`
                        Just ts
    describe "TagsCursor" $
        describe "tagsCursorParent" $
        it "rebuilds to the same" $ rebuildsToTheSame tagsCursorParent
    describe "TagCursor" $ do
        describe "tagCursorParent" $
            it "rebuilds to the same" $ rebuildsToTheSame tagCursorParent
        describe "tagCursorModify" $ do
            it "does not change the index" $
                forAll genValid $ \tgc ->
                    forAll genValid $ \tc -> do
                        let tgc' = tagCursorModify (const tc) tgc
                        tagCursorIndex tgc' `shouldBe` tagCursorIndex tgc
            it
                "does not change the text cursor if the text cursor is not modified" $
                forAll genValid $ \tgc -> do
                    let tgc' = tagCursorModify id tgc
                    tagCursorTag tgc' `shouldBe` tagCursorTag tgc
            it
                "does not change the text cursor selection if the text cursor is not modified" $
                forAll genValid $ \tgc -> do
                    let tgc' = tagCursorModify id tgc
                    selection (tagCursorTag tgc') `shouldBe`
                        selection (tagCursorTag tgc)
            it "has the same resulting text cursor if we use it to modify" $
                forAll genValid $ \tgc ->
                    forAll genValid $ \tc -> do
                        let tgc' = tagCursorModify (const tc) tgc
                        tagCursorTag tgc' `shouldBe` tc
        describe "tagCursorInsert" $ do
            it "makes the resulting tag one longer" $
                forAll genValid $ \c ->
                    forAll genValid $ \tc ->
                        T.length (tagText (build (tagCursorInsert c tc))) `shouldBe`
                        T.length (tagText (build tc)) +
                        1
            it "adds an element to the front if we're at the front" $
                forAll genValid $ \c ->
                    forAll genValid $ \tc ->
                        tagText (build (tagCursorInsert c (tagCursorStart tc))) `shouldBe`
                        T.cons c (tagText (build tc))
        describe "tagCursorAppend" $ do
            it "makes the resulting tag one longer" $
                forAll genValid $ \c ->
                    forAll genValid $ \tc ->
                        T.length (tagText (build (tagCursorAppend c tc))) `shouldBe`
                        T.length (tagText (build tc)) +
                        1
            it "adds an element to the end if we're at the end" $
                forAll genValid $ \c ->
                    forAll genValid $ \tc ->
                        tagText (build (tagCursorAppend c (tagCursorEnd tc))) `shouldBe`
                        T.snoc (tagText (build tc)) c
        -- describe "tagCursorRemove" $
        -- describe "tagCursorDelete"
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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Smos.CursorSpec
    ( spec
    ) where

import TestImport

import qualified Data.Text as T

import Smos.Cursor
import Smos.Cursor.Gen ()
import Smos.Data
import Smos.Data.Gen ()

spec :: Spec
spec = do
    describe "ForestCursor" $ do
        describe "makeForestCurser" $
            it "is the inverse of 'build'" $
            inverseFunctionsOnValid makeForestCursor build
        describe "forestCursorSelectIx" $
            it "rebuilds to the same" $
            forAll genUnchecked $ \i ->
                rebuildsToTheSameIfSuceeds (`forestCursorSelectIx` i)
        describe "forestCursorSelectFirst" $
            it "rebuilds to the same" $
            rebuildsToTheSameIfSuceeds forestCursorSelectFirst
        describe "forestCursorSelectLast" $
            it "rebuilds to the same" $
            rebuildsToTheSameIfSuceeds forestCursorSelectLast
        describe "forestCursorInsertAtStart" $
            it "rebuilds to something valid" $
            forAll genValid $ \st ->
                rebuildsToValid (forestCursorInsertAtStart st)
        describe "forestCursorInsertAtEnd" $
            it "rebuilds to something valid" $
            forAll genValid $ \st ->
                rebuildsToValid (forestCursorInsertAtEnd st)
    describe "TreeCursor" $ do
        describe "treeCursorSelectPrev" $
            it "rebuilds to the same" $
            rebuildsToTheSameIfSuceeds treeCursorSelectPrev
        describe "treeCursorSelectNext" $
            it "rebuilds to the same" $
            rebuildsToTheSameIfSuceeds treeCursorSelectNext
        describe "treeCursorInsertAbove" $
            it "rebuilds to something valid" $
            forAll genValid $ \st -> rebuildsToValid (treeCursorInsertAbove st)
        describe "treeCursorInsertBelow" $
            it "rebuilds to something valid" $
            forAll genValid $ \st -> rebuildsToValid (treeCursorInsertBelow st)
        describe "treeCursorInsertChildAt" $
            it "rebuilds to something valid" $
            forAll genUnchecked $ \ix ->
                forAll genValid $ \st ->
                    rebuildsToValid (treeCursorInsertChildAt ix st)
        describe "treeCursorInsertChildAtStart" $
            it "rebuilds to something valid" $
            forAll genValid $ \st ->
                rebuildsToValid (treeCursorInsertChildAtStart st)
        describe "treeCursorInsertChildAtEnd" $
            it "rebuilds to something valid" $
            forAll genValid $ \st ->
                rebuildsToValid (treeCursorInsertChildAtEnd st)
        describe "treeCursorDeleteCurrent" $
            it "rebuilds to something valid" $
            forAll genValid $ \tc ->
                case treeCursorDeleteCurrent tc of
                    Left fc' -> shouldBeValid fc'
                    Right tc' -> shouldBeValid tc'
    describe "EntryCursor" $ do
        describe "entryCursorParent" $
            it "rebuilds to the same" $ rebuildsToTheSame entryCursorParent
        describe "entryCursorHeader" $
            it "rebuilds to the same" $ rebuildsToTheSame entryCursorHeader
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
    describe "stateCursor" $ do
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

rebuildsToTheSame ::
       (Show a, Show b, GenValid a, Rebuild a, Rebuild b)
    => (a -> b)
    -> Property
rebuildsToTheSame func =
    forAll genValid $ \tc ->
        let t = rebuild tc
            tc' = func tc
            t' = rebuild tc'
        in unless (t' == t) $
           expectationFailure $
           unlines
               [ "Initial data: " ++ show t
               , "Built cursor: " ++ show tc
               , "Changed cursor: " ++ show tc'
               , "Final forest: " ++ show t'
               ]

rebuildsToTheSameIfSuceeds ::
       (Show a, Show b, GenValid a, Rebuild a, Rebuild b)
    => (a -> Maybe b)
    -> Property
rebuildsToTheSameIfSuceeds func =
    forAll genValid $ \tc ->
        let t = rebuild tc
            mtc' = func tc
        in case mtc' of
               Nothing -> pure ()
               Just tc' ->
                   let t' = rebuild tc'
                   in unless (t' == t) $
                      expectationFailure $
                      unlines
                          [ "Initial data: " ++ show t
                          , "Built cursor: " ++ show tc
                          , "Changed cursor: " ++ show tc'
                          , "Final data: " ++ show t'
                          ]

rebuildsToValid ::
       (Show a, Show b, GenValid a, Validity b, Rebuild b)
    => (a -> b)
    -> Property
rebuildsToValid func = forAll genValid $ shouldBeValid . func
-- buildsToTheSame ::
--        ( Show (Building b)
--        , Show a
--        , Show b
--        , GenValid (Building b)
--        , Build a
--        , Build b
--        , Eq (Building b)
--        )
--     => (Building b -> a)
--     -> (a -> b)
--     -> Property
-- buildsToTheSame curse func =
--     forAll genValid $ \t ->
--         let tc = curse t
--             tc' = func tc
--             t' = build tc'
--         in unless (t' == t) $
--            expectationFailure $
--            unlines
--                [ "Initial data: " ++ show t
--                , "Built cursor: " ++ show tc
--                , "Changed cursor: " ++ show tc'
--                , "Final data: " ++ show t'
--                ]
--
-- buildsToTheSameIfSuceeds ::
--        ( Show (Building b)
--        , Show a
--        , Show b
--        , GenValid (Building b)
--        , Build a
--        , Build b
--        , Eq (Building b)
--        )
--     => (Building b -> a)
--     -> (a -> Maybe b)
--     -> Property
-- buildsToTheSameIfSuceeds curse func =
--     forAll genValid $ \t ->
--         let tc = curse t
--             mtc' = func tc
--         in case mtc' of
--                Nothing -> pure ()
--                Just tc' ->
--                    let t' = build tc'
--                    in unless (t' == t) $
--                       expectationFailure $
--                       unlines
--                           [ "Initial data: " ++ show t
--                           , "Built cursor: " ++ show tc
--                           , "Changed cursor: " ++ show tc'
--                           , "Final data: " ++ show t'
--                           ]

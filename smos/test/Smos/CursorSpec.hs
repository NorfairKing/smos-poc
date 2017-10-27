{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Smos.CursorSpec
    ( spec
    ) where

import TestImport

import Smos.Cursor
import Smos.Cursor.Gen ()
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
        -- forestCursorInsertAtStart
        -- forestCursorInsertAtEnd
    describe "TreeCursor" $ do
        describe "treeCursorSelectPrev" $
            it "rebuilds to the same" $
            rebuildsToTheSameIfSuceeds treeCursorSelectPrev
        describe "treeCursorSelectNext" $
            it "rebuilds to the same" $
            rebuildsToTheSameIfSuceeds treeCursorSelectNext
        -- treeCursorInsertAbove
        -- treeCursorInsertBelow
        -- treeCursorInsertChildAt
        -- treeCursorInsertChildAtStart
        -- treeCursorInsertChildAtEnd
        -- treeCursorInsertDeleteCurrent

--rebuildsToTheSame ::
--       (Show a, Show b, GenValid a, Rebuild a, Rebuild b)
--    => (a -> b)
--    -> Property
--rebuildsToTheSame func =
--    forAll genValid $ \tc ->
--        let t = rebuild tc
--            tc' = func tc
--            t' = rebuild tc'
--        in unless (t' == t) $
--           expectationFailure $
--           unlines
--               [ "Initial data: " ++ show t
--               , "Built cursor: " ++ show tc
--               , "Changed cursor: " ++ show tc'
--               , "Final forest: " ++ show t'
--               ]
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

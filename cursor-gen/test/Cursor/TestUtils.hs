{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.TestUtils
    ( buildsToValid
    , buildsToTheSame
    , buildsToTheSameIfSuceeds
    , rebuildsToValid
    , rebuildsToValidIfSucceeds
    , rebuildsToTheSame
    , rebuildsToTheSameIfSuceeds
    , reselectsToTheSameSelection
    , IntCursor(..)
    , IntView(..)
    ) where

import TestImport

import Data.Hashable

import Cursor.Class
import Cursor.Select

buildsToValid ::
       ( Show a
       , Show b
       , GenValid a
       , Build b
       , Show (Building b)
       , Validity (Building b)
       )
    => (a -> b)
    -> Property
buildsToValid func = forAll genValid $ shouldBeValid . build . func

buildsToTheSame ::
       ( Show a
       , Show b
       , GenValid a
       , Build a
       , Build b
       , Building a ~ Building b
       , Show (Building a)
       , Eq (Building a)
       )
    => (a -> b)
    -> Property
buildsToTheSame func =
    forAll genValid $ \tc ->
        let t = build tc
            tc' = func tc
            t' = build tc'
        in unless (t' == t) $
           expectationFailure $
           unlines
               [ "Initial data: " ++ show t
               , "Built cursor: " ++ show tc
               , "Changed cursor: " ++ show tc'
               , "Final data: " ++ show t'
               ]

buildsToTheSameIfSuceeds ::
       ( Show a
       , Show b
       , GenValid a
       , Build a
       , Build b
       , Building a ~ Building b
       , Show (Building a)
       , Eq (Building a)
       )
    => (a -> Maybe b)
    -> Property
buildsToTheSameIfSuceeds func =
    forAll genValid $ \tc ->
        let t = build tc
            mtc' = func tc
        in case mtc' of
               Nothing -> pure ()
               Just tc' ->
                   let t' = build tc'
                   in unless (t' == t) $
                      expectationFailure $
                      unlines
                          [ "Initial data: " ++ show t
                          , "Initial cursor: " ++ show tc
                          , "Changed cursor: " ++ show tc'
                          , "Final data: " ++ show t'
                          ]

rebuildsToValid ::
       ( Show a
       , Show b
       , GenValid a
       , Rebuild b
       , Validity (ReBuilding b)
       , Show (ReBuilding b)
       )
    => (a -> b)
    -> Property
rebuildsToValid func = forAll genValid $ shouldBeValid . rebuild . func

rebuildsToValidIfSucceeds ::
       ( Show a
       , Show b
       , GenValid a
       , Rebuild b
       , Validity (ReBuilding b)
       , Show (ReBuilding b)
       )
    => (a -> Maybe b)
    -> Property
rebuildsToValidIfSucceeds func =
    forAll genValid $ shouldBeValid . fmap rebuild . func

rebuildsToTheSame ::
       ( Show a
       , Show b
       , GenValid a
       , Rebuild a
       , Rebuild b
       , ReBuilding a ~ ReBuilding b
       , Show (ReBuilding a)
       , Eq (ReBuilding a)
       )
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
               , "Final data: " ++ show t'
               ]

rebuildsToTheSameIfSuceeds ::
       ( Show a
       , Show b
       , GenValid a
       , Rebuild a
       , Rebuild b
       , ReBuilding a ~ ReBuilding b
       , Show (ReBuilding a)
       , Eq (ReBuilding a)
       )
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

reselectsToTheSameSelection ::
       forall a.
       (Show a, GenValid a, Rebuild a, Reselect a, Rebuild (Reselection a))
    => Property
reselectsToTheSameSelection =
    forAll (genValid @a) $ \cur ->
        selection (reselect (selection cur) cur) `shouldBe` selection cur

-- A degenerate cursor
newtype IntView = IntView
    { intViewInt :: Int
    } deriving (Show, Eq, Generic)

instance Validity IntView

instance Hashable IntView

instance View IntView where
    type Source IntView = Int
    source = intViewInt
    view = IntView

newtype IntCursor = IntCursor
    { intValue :: Int
    } deriving (Show, Eq, Generic)

instance Validity IntCursor

instance Hashable IntCursor

instance GenUnchecked IntCursor

instance GenValid IntCursor

instance Build IntCursor where
    type Building IntCursor = IntView
    build = view . intValue

instance Rebuild IntCursor where
    type ReBuilding IntCursor = Select IntView
    rebuild = select . IntView . intValue
    selection _ = [0]

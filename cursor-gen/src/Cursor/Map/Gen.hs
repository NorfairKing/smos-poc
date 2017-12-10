{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Map.Gen where

import Import

import Cursor.Map

import Cursor.ListElem.Gen (listElemElemOf)

instance (Ord a, GenUnchecked a, GenUnchecked b) =>
         GenUnchecked (MapCursor a b)

instance (Ord a, GenValid a, GenValid b) => GenValid (MapCursor a b) where
    genValid = do
        m <- genValid
        case makeMapCursor m of
            Nothing -> scale (+ 1) genValid
            Just mc -> pure mc

instance (Ord a, GenUnchecked a, GenUnchecked b) =>
         GenUnchecked (KeyValueCursor a b)

instance (Ord a, GenValid a, GenValid b) => GenValid (KeyValueCursor a b) where
    genValid = do
        mc <- genValid
        listElemElemOf $ mapCursorList mc

instance (Ord a, GenUnchecked a, GenUnchecked b) =>
         GenUnchecked (KeyCursor a b)

instance (Ord a, GenValid a, GenValid b) => GenValid (KeyCursor a b) where
    genValid = keyValueCursorKey <$> genValid

instance (Ord a, GenUnchecked a, GenUnchecked b) =>
         GenUnchecked (ValueCursor a b)

instance (Ord a, GenValid a, GenValid b) => GenValid (ValueCursor a b) where
    genValid = keyValueCursorValue <$> genValid

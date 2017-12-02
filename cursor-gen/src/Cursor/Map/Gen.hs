{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Map.Gen where

import Import

import Cursor.Map

import Cursor.ListElem.Gen (listElemElemOf)

instance (GenUnchecked a, GenUnchecked b) => GenUnchecked (MapCursor a b)

instance (GenValid a, GenValid b) => GenValid (MapCursor a b)

instance (GenUnchecked a, GenUnchecked b) =>
         GenUnchecked (KeyValueCursor a b) where
    genUnchecked = do
        mc <- genUnchecked
        listElemElemOf mc
    shrinkUnchecked _ = []

instance (GenValid a, GenValid b) => GenValid (KeyValueCursor a b) where
    genValid = do
        mc <- genUnchecked
        listElemElemOf mc

instance (GenUnchecked a, GenUnchecked b) => GenUnchecked (KeyCursor a b)

instance (GenValid a, GenValid b) => GenValid (KeyCursor a b)

instance (GenUnchecked a, GenUnchecked b) =>
         GenUnchecked (ValueCursor a b)

instance (GenValid a, GenValid b) => GenValid (ValueCursor a b)

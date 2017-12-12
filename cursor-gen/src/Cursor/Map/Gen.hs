{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Map.Gen where

import Import

import Data.GenValidity.HashMap ()
import Data.Hashable

import Cursor.Map

import Cursor.ListElem.Gen (listElemElemOf)

instance (Eq a, Hashable a, GenUnchecked a, GenUnchecked b) =>
         GenUnchecked (MapCursor a b)

instance (Eq a, Hashable a, GenValid a, GenValid b) =>
         GenValid (MapCursor a b) where
    genValid = makeMapCursor <$> genValid

instance (Eq a, Hashable a, GenUnchecked a, GenUnchecked b) =>
         GenUnchecked (KeyValueCursor a b)

instance (Eq a, Hashable a, GenValid a, GenValid b) =>
         GenValid (KeyValueCursor a b) where
    genValid = do
        mc <- genValid
        listElemElemOf $ mapCursorList mc

instance (Eq a, Hashable a, GenUnchecked a, GenUnchecked b) =>
         GenUnchecked (KeyCursor a b)

instance (Eq a, Hashable a, GenValid a, GenValid b) =>
         GenValid (KeyCursor a b) where
    genValid = do
        kvc <- genValid
        pure $
            case kvc of
                KVK kc -> kc
                KVV vc -> valueCursorSelectKey vc

instance (Eq a, Hashable a, GenUnchecked a, GenUnchecked b) =>
         GenUnchecked (ValueCursor a b)

instance (Eq a, Hashable a, GenValid a, GenValid b) =>
         GenValid (ValueCursor a b) where
    genValid = do
        kvc <- genValid
        pure $
            case kvc of
                KVK kc -> keyCursorSelectValue kc
                KVV vc -> vc

instance (GenUnchecked a, GenUnchecked b) => GenUnchecked (MapView a b)

instance (GenValid a, GenValid b) => GenValid (MapView a b)

instance (GenUnchecked a, GenUnchecked b) =>
         GenUnchecked (KeyValueView a b)

instance (GenValid a, GenValid b) => GenValid (KeyValueView a b)
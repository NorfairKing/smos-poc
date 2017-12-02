{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.Map where

import Import

import Data.Map (Map)
import qualified Data.Map as M

import Cursor.ListElem

data MapCursor a b = MapCursor
    { mapCrusorList :: ListElemCursor (KeyValueCursor a b)
    } deriving (Show, Eq, Generic)

instance (Validity a, Validity b) => Validity (MapCursor a b)

makeMapCursor :: Map a b -> MapCursor a b
makeMapCursor m =
    let tups = M.assocs m
    in undefined

data KeyValueCursor a b = KeyValueCursor
    { keyValueCursorParent :: MapCursor a b
    , keyValueCursorKey :: KeyCursor a b
    , keyValueCursorValue :: ValueCursor a b
    } deriving (Show, Eq, Generic)

instance (Validity a, Validity b) => Validity (KeyValueCursor a b)

data KeyCursor a b = KeyCursor
    { keyCursorParent :: KeyValueCursor a b
    , keyCursorKey :: a
    } deriving (Show, Eq, Generic)

instance (Validity a, Validity b) => Validity (KeyCursor a b)

data ValueCursor a b = ValueCursor
    { valueCursorParent :: KeyValueCursor a b
    , valueCursorValue :: b
    } deriving (Show, Eq, Generic)

instance (Validity a, Validity b) => Validity (ValueCursor a b)

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.Map where

import Import

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M

import Cursor.Class
import Cursor.ListElem

newtype MapCursor a b = MapCursor
    { mapCursorList :: ListElemCursor (KeyValueCursor a b)
    } deriving (Show, Eq, Generic)

instance (Validity a, Validity b) => Validity (MapCursor a b) where
    isValid a = isValid (rebuild a)
    validate a = rebuild a <?!> "rebuild"

instance Rebuild (MapCursor a b) where
    type ReBuilding (MapCursor a b) = MapView a b
    rebuild = MapView . fmap build . rebuild . mapCursorList
    selection = selection . mapCursorList

newtype MapView a b = MapView
    { mapViewList :: ListElemView (KeyValueView a b)
    } deriving (Show, Eq, Generic)

instance (Validity a, Validity b) => Validity (MapView a b)

instance Ord a => View (MapView a b) where
    type Source (MapView a b) = Map a b
    source = M.fromList . fmap source . NE.toList . source . mapViewList
    view m = MapView $ view . fmap view . NE.fromList $ M.assocs m -- FIXME fromList is partial

makeMapCursor :: Map a b -> Maybe (MapCursor a b)
makeMapCursor m =
    let tups = M.assocs m
    in case tups of
           [] -> Nothing
           (t:ts) ->
               let ne = t :| ts
                   els =
                       flip fmap ne $ \(a, b) ->
                           let kc =
                                   KeyCursor
                                   {keyCursorParent = kvc, keyCursorKey = a}
                               vc =
                                   ValueCursor
                                   { valueCursorParent = kvc
                                   , valueCursorValue = b
                                   }
                               kvc =
                                   KeyValueCursor
                                   { keyValueCursorParent = mc
                                   , keyValueCursorKey = kc
                                   , keyValueCursorValue = vc
                                   }
                           in kvc
                   mc = MapCursor $ makeListElemCursor els
               in Just mc

data KeyValueCursor a b = KeyValueCursor
    { keyValueCursorParent :: MapCursor a b
    , keyValueCursorKey :: KeyCursor a b
    , keyValueCursorValue :: ValueCursor a b
    } deriving (Show, Eq, Generic)

instance (Validity a, Validity b) => Validity (KeyValueCursor a b) where
    isValid a = isValid (build a) && isValid (rebuild a)
    validate a = (build a <?!> "build") <> (rebuild a <?!> "rebuild")

instance Build (KeyValueCursor a b) where
    type Building (KeyValueCursor a b) = KeyValueView a b
    build KeyValueCursor {..} =
        KeyValueView
        { keyValueViewKey = build keyValueCursorKey
        , keyValueViewValue = build keyValueCursorValue
        }

instance Rebuild (KeyValueCursor a b) where
    type ReBuilding (KeyValueCursor a b) = MapView a b
    rebuild = rebuild . keyValueCursorParent
    selection = selection . keyValueCursorParent

data KeyValueView a b = KeyValueView
    { keyValueViewKey :: a
    , keyValueViewValue :: b
    } deriving (Show, Eq, Generic)

instance (Validity a, Validity b) => Validity (KeyValueView a b)

instance View (KeyValueView a b) where
    type Source (KeyValueView a b) = (a, b)
    source KeyValueView {..} = (keyValueViewKey, keyValueViewValue)
    view (a, b) = KeyValueView {keyValueViewKey = a, keyValueViewValue = b}

data KeyCursor a b = KeyCursor
    { keyCursorParent :: KeyValueCursor a b
    , keyCursorKey :: a
    } deriving (Show, Eq, Generic)

instance (Validity a, Validity b) => Validity (KeyCursor a b) where
    isValid a = isValid (build a) && isValid (rebuild a)
    validate a = (build a <?!> "build") <> (rebuild a <?!> "rebuild")

instance Build (KeyCursor a b) where
    type Building (KeyCursor a b) = a
    build = keyCursorKey

instance Rebuild (KeyCursor a b) where
    type ReBuilding (KeyCursor a b) = MapView a b
    rebuild = rebuild . keyCursorParent
    selection KeyCursor {..} = 0 : selection keyCursorParent

data ValueCursor a b = ValueCursor
    { valueCursorParent :: KeyValueCursor a b
    , valueCursorValue :: b
    } deriving (Show, Eq, Generic)

instance Build (ValueCursor a b) where
    type Building (ValueCursor a b) = b
    build = valueCursorValue

instance Rebuild (ValueCursor a b) where
    type ReBuilding (ValueCursor a b) = MapView a b
    rebuild = rebuild . valueCursorParent
    selection ValueCursor {..} = 1 : selection valueCursorParent

instance (Validity a, Validity b) => Validity (ValueCursor a b) where
    isValid a = isValid (build a) && isValid (rebuild a)
    validate a = (build a <?!> "build") <> (rebuild a <?!> "rebuild")

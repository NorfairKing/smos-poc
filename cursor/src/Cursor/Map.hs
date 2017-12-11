{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.Map where

import Import

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import Lens.Micro

import Cursor.Class
import Cursor.ListElem
import Cursor.Select

newtype MapCursor a b = MapCursor
    { mapCursorList :: ListElemCursor (KeyValueCursor a b)
    } deriving (Show, Eq, Generic)

instance (Validity a, Validity b) => Validity (MapCursor a b) where
    isValid a = isValid (rebuild a)
    validate a = rebuild a <?!> "rebuild"

instance Rebuild (MapCursor a b) where
    type ReBuilding (MapCursor a b) = MapView a b
    rebuild = MapView . fmap rebuild . rebuild . mapCursorList
    selection mcl = selection (mapCursorList mcl)

newtype MapView a b = MapView
    { mapViewList :: ListElemView (KeyValueView a b)
    } deriving (Show, Eq, Generic)

instance (Validity a, Validity b) => Validity (MapView a b)

instance (Eq a, Hashable a) => View (MapView a b) where
    type Source (MapView a b) = NonEmpty (a, b)
    source = fmap source . source . mapViewList
    view = MapView . view . fmap view

instance Selectable (MapView a b) where
    applySelection msel = MapView . applySelection msel . mapViewList

makeMapCursor :: NonEmpty (a, b) -> MapCursor a b
makeMapCursor ne =
    let els =
            flip fmap ne $ \(a, b) ->
                let lkc = KeyCursor {keyCursorKey = a, keyCursorValue = b}
                in KVK lkc
        mc = MapCursor $ makeListElemCursor els
    in mc

makeMapCursorFromMap :: HashMap a b -> Maybe (MapCursor a b)
makeMapCursorFromMap m =
    let tups = HM.toList m
    in case tups of
           [] -> Nothing
           (t:ts) -> Just $ makeMapCursor $ t :| ts

rebuildHashmapFromMapView :: (Eq a, Hashable a) => MapView a b -> HashMap a b
rebuildHashmapFromMapView = HM.fromList . NE.toList . source

mapCursorListL :: Lens' (MapCursor a b) (ListElemCursor (KeyValueCursor a b))
mapCursorListL = lens getter setter
  where
    getter = mapCursorList
    setter mc lc = mc {mapCursorList = lc}

mapCursorSelectedL :: Lens' (MapCursor a b) (KeyValueCursor a b)
mapCursorSelectedL = mapCursorListL . listElemCursorElemL

data KeyValueCursor a b
    = KVK (KeyCursor a b)
    | KVV (ValueCursor a b)
    deriving (Show, Eq, Generic)

instance (Validity a, Validity b) => Validity (KeyValueCursor a b) where
    isValid a = isValid (build a) && isValid (rebuild a)
    validate a = (build a <?!> "build") <> (rebuild a <?!> "rebuild")

instance Build (KeyValueCursor a b) where
    type Building (KeyValueCursor a b) = KeyValueView a b
    build (KVK KeyCursor {..}) = KVVK keyCursorKey keyCursorValue
    build (KVV ValueCursor {..}) = KVVV valueCursorKey valueCursorValue

instance Rebuild (KeyValueCursor a b) where
    type ReBuilding (KeyValueCursor a b) = KeyValueView a b
    rebuild (KVK kc) = rebuild kc
    rebuild (KVV vc) = rebuild vc
    selection (KVK kc) = selection kc
    selection (KVV vc) = selection vc

data KeyValueView a b
    = KVVK a
           b
    | KVVV a
           b
    deriving (Show, Eq, Generic)

instance (Validity a, Validity b) => Validity (KeyValueView a b)

instance View (KeyValueView a b) where
    type Source (KeyValueView a b) = (a, b)
    source (KVVK a b) = (a, b)
    source (KVVV a b) = (a, b)
    view (a, b) = KVVK a b

instance Selectable (KeyValueView a b) where
    applySelection =
        drillWithSel_ $ \mi kv ->
            case (mi, kv) of
                (Just 0, KVVV a b) -> KVVK a b
                (Just 1, KVVK a b) -> KVVV a b
                (_, _) -> kv

data KeyCursor a b = KeyCursor
    { keyCursorKey :: a
    , keyCursorValue :: b
    } deriving (Show, Eq, Generic)

instance (Validity a, Validity b) => Validity (KeyCursor a b) where
    isValid a = isValid (build a) && isValid (rebuild a)
    validate a = (build a <?!> "build") <> (rebuild a <?!> "rebuild")

instance Build (KeyCursor a b) where
    type Building (KeyCursor a b) = a
    build = keyCursorKey

instance Rebuild (KeyCursor a b) where
    type ReBuilding (KeyCursor a b) = KeyValueView a b
    rebuild KeyCursor {..} = KVVK keyCursorKey keyCursorValue
    selection KeyCursor {..} = [0]

keyCursorSelectValue :: KeyCursor a b -> ValueCursor a b
keyCursorSelectValue KeyCursor {..} =
    ValueCursor
    {valueCursorKey = keyCursorKey, valueCursorValue = keyCursorValue}

data ValueCursor a b = ValueCursor
    { valueCursorKey :: a
    , valueCursorValue :: b
    } deriving (Show, Eq, Generic)

instance Build (ValueCursor a b) where
    type Building (ValueCursor a b) = b
    build = valueCursorValue

instance Rebuild (ValueCursor a b) where
    type ReBuilding (ValueCursor a b) = KeyValueView a b
    rebuild ValueCursor {..} = KVVV valueCursorKey valueCursorValue
    selection ValueCursor {..} = [1]

instance (Validity a, Validity b) => Validity (ValueCursor a b) where
    isValid a = isValid (build a) && isValid (rebuild a)
    validate a = (build a <?!> "build") <> (rebuild a <?!> "rebuild")

valueCursorSelectKey :: ValueCursor a b -> KeyCursor a b
valueCursorSelectKey ValueCursor {..} =
    KeyCursor {keyCursorKey = valueCursorKey, keyCursorValue = valueCursorValue}

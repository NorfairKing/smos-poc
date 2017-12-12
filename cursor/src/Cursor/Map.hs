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

mapViewListElemViewL ::
       Lens (MapView a b) (MapView c d) (ListElemView (KeyValueView a b)) (ListElemView (KeyValueView c d))
mapViewListElemViewL = lens mapViewList $ \mv l -> mv {mapViewList = l}

mapViewListElemViewT ::
       Traversal (MapView a b) (MapView c d) (KeyValueView a b) (KeyValueView c d)
mapViewListElemViewT = mapViewListElemViewL . listElemViewElemsT

mapViewListElemViewKeysT :: Traversal (MapView a b) (MapView c b) a c
mapViewListElemViewKeysT = mapViewListElemViewT . keyValueViewKeyL

mapViewListElemViewValuesT :: Traversal (MapView a b) (MapView a d) b d
mapViewListElemViewValuesT = mapViewListElemViewT . keyValueViewValueL

mapCursorListElemCursorL ::
       Lens (MapCursor a b) (MapCursor c d) (ListElemCursor (KeyValueCursor a b)) (ListElemCursor (KeyValueCursor c d))
mapCursorListElemCursorL = lens mapCursorList $ \mv l -> mv {mapCursorList = l}

mapCursorListElemCursorT ::
       Traversal (MapCursor a b) (MapCursor c d) (KeyValueCursor a b) (KeyValueCursor c d)
mapCursorListElemCursorT = mapCursorListElemCursorL . listElemCursorElemsT

mapCursorListElemCursorKeysT :: Traversal (MapCursor a b) (MapCursor c b) a c
mapCursorListElemCursorKeysT = mapCursorListElemCursorT . keyValueCursorKeyL

mapCursorListElemCursorValuesT :: Traversal (MapCursor a b) (MapCursor a d) b d
mapCursorListElemCursorValuesT = mapCursorListElemCursorT . keyValueCursorValueL

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

rebuildMapCursor :: (Eq a, Hashable a) => MapCursor a b -> NonEmpty (a, b)
rebuildMapCursor = source . rebuild

mapCursorListL :: Lens' (MapCursor a b) (ListElemCursor (KeyValueCursor a b))
mapCursorListL = lens getter setter
  where
    getter = mapCursorList
    setter mc lc = mc {mapCursorList = lc}

mapCursorListT :: Traversal' (MapCursor a b) (KeyValueCursor a b)
mapCursorListT = mapCursorListL . listElemCursorElemsT

mapCursorKeysT :: Traversal' (MapCursor a b) a
mapCursorKeysT = mapCursorListT . keyValueCursorKeyL

mapCursorValuesT :: Traversal' (MapCursor a b) b
mapCursorValuesT = mapCursorListT . keyValueCursorValueL

mapCursorSelectedL :: Lens' (MapCursor a b) (KeyValueCursor a b)
mapCursorSelectedL = mapCursorListL . listElemCursorElemL

mapCursorSelectPrev :: MapCursor a b -> Maybe (MapCursor a b)
mapCursorSelectPrev = mapCursorListL listElemCursorSelectPrev

mapCursorSelectNext :: MapCursor a b -> Maybe (MapCursor a b)
mapCursorSelectNext = mapCursorListL listElemCursorSelectNext

mapCursorSelectFirst :: MapCursor a b -> MapCursor a b
mapCursorSelectFirst = mapCursorListL %~ listElemCursorSelectFirst

mapCursorSelectLast :: MapCursor a b -> MapCursor a b
mapCursorSelectLast = mapCursorListL %~ listElemCursorSelectLast

mapCursorInsert :: a -> b -> MapCursor a b -> MapCursor a b
mapCursorInsert a b =
    mapCursorListL %~
    listElemCursorInsert (KVK KeyCursor {keyCursorKey = a, keyCursorValue = b})

mapCursorAppend :: a -> b -> MapCursor a b -> MapCursor a b
mapCursorAppend a b =
    mapCursorListL %~
    listElemCursorAppend (KVK KeyCursor {keyCursorKey = a, keyCursorValue = b})

mapCursorInsertAndSelect :: a -> b -> MapCursor a b -> MapCursor a b
mapCursorInsertAndSelect a b =
    mapCursorListL %~
    listElemCursorInsertAndSelect
        (KVK KeyCursor {keyCursorKey = a, keyCursorValue = b})

mapCursorAppendAndSelect :: a -> b -> MapCursor a b -> MapCursor a b
mapCursorAppendAndSelect a b =
    mapCursorListL %~
    listElemCursorAppendAndSelect
        (KVK KeyCursor {keyCursorKey = a, keyCursorValue = b})

mapCursorRemoveElemAndSelectPrev :: MapCursor a b -> Maybe (MapCursor a b)
mapCursorRemoveElemAndSelectPrev =
    mapCursorListL listElemCursorRemoveElemAndSelectPrev

mapCursorDeleteElemAndSelectNext :: MapCursor a b -> Maybe (MapCursor a b)
mapCursorDeleteElemAndSelectNext =
    mapCursorListL listElemCursorDeleteElemAndSelectNext

mapCursorRemoveElem :: MapCursor a b -> Maybe (MapCursor a b)
mapCursorRemoveElem = mapCursorListL listElemCursorRemoveElem

mapCursorDeleteElem :: MapCursor a b -> Maybe (MapCursor a b)
mapCursorDeleteElem = mapCursorListL listElemCursorDeleteElem

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

keyValueCursorKeyL :: Lens (KeyValueCursor a b) (KeyValueCursor c b) a c
keyValueCursorKeyL = lens getter setter
  where
    getter kvc =
        case kvc of
            KVK kc -> keyCursorKey kc
            KVV vc -> valueCursorKey vc
    setter kvc k =
        case kvc of
            KVK kc -> KVK $ kc {keyCursorKey = k}
            KVV vc -> KVV $ vc {valueCursorKey = k}

keyValueCursorValueL :: Lens (KeyValueCursor a b) (KeyValueCursor a d) b d
keyValueCursorValueL = lens getter setter
  where
    getter kvc =
        case kvc of
            KVK kc -> keyCursorValue kc
            KVV vc -> valueCursorValue vc
    setter kvc k =
        case kvc of
            KVK kc -> KVK $ kc {keyCursorValue = k}
            KVV vc -> KVV $ vc {valueCursorValue = k}

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

keyValueViewKeyL :: Lens (KeyValueView a b) (KeyValueView c b) a c
keyValueViewKeyL = lens getter setter
  where
    getter kvc =
        case kvc of
            KVVK a _ -> a
            KVVV a _ -> a
    setter kvc a =
        case kvc of
            KVVK _ b -> KVVK a b
            KVVV _ b -> KVVV a b

keyValueViewValueL :: Lens (KeyValueView a b) (KeyValueView a d) b d
keyValueViewValueL = lens getter setter
  where
    getter kvc =
        case kvc of
            KVVK _ b -> b
            KVVV _ b -> b
    setter kvc b =
        case kvc of
            KVVK a _ -> KVVK a b
            KVVV a _ -> KVVV a b

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

keyCursorKeyL :: Lens (KeyCursor a b) (KeyCursor c b) a c
keyCursorKeyL = lens keyCursorKey $ \kc k -> kc {keyCursorKey = k}

keyCursorValueL :: Lens (KeyCursor a b) (KeyCursor a d) b d
keyCursorValueL = lens keyCursorValue $ \kc v -> kc {keyCursorValue = v}

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

valueCursorKeyL :: Lens' (ValueCursor a b) a
valueCursorKeyL = lens valueCursorKey $ \vc k -> vc {valueCursorKey = k}

valueCursorValueL :: Lens' (ValueCursor a b) b
valueCursorValueL = lens valueCursorValue $ \vc v -> vc {valueCursorValue = v}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.List
    ( ListCursor
    , listCursorPrev
    , listCursorNext
    , emptyListCursor
    , makeListCursor
    , rebuildListCursor
    , listCursorIndex
    , listCursorSelectPrev
    , listCursorSelectNext
    , listCursorSelectPrevChar
    , listCursorSelectNextChar
    , listCursorSelectStart
    , listCursorSelectEnd
    , listCursorInsert
    , listCursorAppend
    , listCursorRemove
    , listCursorDelete
    , listCursorSplit
    , listCursorCombine
    ) where

import Import

import Cursor.Class
import Cursor.Select

data ListCursor a = ListCursor
    { listCursorPrev :: [a] -- ^ In reverse order
    , listCursorNext :: [a]
    } deriving (Eq, Generic)

instance Validity a => Validity (ListCursor a)

instance Show a => Show (ListCursor a) where
    show ListCursor {..} =
        unwords [show listCursorPrev, "|", show listCursorNext]

instance Build (ListCursor a) where
    type Building (ListCursor a) = Maybe a
    build ListCursor {..} =
        case listCursorPrev of
            [] -> Nothing
            (c:_) -> Just c

instance Rebuild (ListCursor a) where
    type ReBuilding (ListCursor a) = [a]
    rebuild = rebuildListCursor
    selection = (: []) . listCursorIndex

instance Reselect (ListCursor a) where
    type Reselection (ListCursor a) = ListCursor a
    reselect sel cur =
        case sel of
            [] -> cur
            (ix_:_) ->
                let els = rebuild cur
                in ListCursor
                   { listCursorPrev = reverse $ take ix_ els
                   , listCursorNext = drop ix_ els
                   }

instance Selectable (ListCursor a) where
    applySelection =
        drillWithSel_ $ \mix lc ->
            case mix of
                Nothing -> makeListCursor $ rebuild lc
                Just ix_ ->
                    case splitAt ix_ $ rebuild lc of
                        (l, r) ->
                            ListCursor
                            {listCursorPrev = reverse l, listCursorNext = r}

emptyListCursor :: ListCursor a
emptyListCursor = ListCursor {listCursorPrev = [], listCursorNext = []}

makeListCursor :: [a] -> ListCursor a
makeListCursor ls = ListCursor {listCursorPrev = [], listCursorNext = ls}

rebuildListCursor :: ListCursor a -> [a]
rebuildListCursor ListCursor {..} = reverse listCursorPrev ++ listCursorNext

listCursorIndex :: ListCursor a -> Int
listCursorIndex = length . listCursorPrev

listCursorSelectPrev :: ListCursor a -> Maybe (ListCursor a)
listCursorSelectPrev tc =
    case listCursorPrev tc of
        [] -> Nothing
        (c:cs) ->
            Just
                ListCursor
                {listCursorPrev = cs, listCursorNext = c : listCursorNext tc}

listCursorSelectNext :: ListCursor a -> Maybe (ListCursor a)
listCursorSelectNext tc =
    case listCursorNext tc of
        [] -> Nothing
        (c:cs) ->
            Just
                ListCursor
                {listCursorPrev = c : listCursorPrev tc, listCursorNext = cs}

listCursorSelectPrevChar :: ListCursor a -> Maybe a
listCursorSelectPrevChar tc =
    case listCursorPrev tc of
        [] -> Nothing
        (c:_) -> Just c

listCursorSelectNextChar :: ListCursor a -> Maybe a
listCursorSelectNextChar tc =
    case listCursorNext tc of
        [] -> Nothing
        (c:_) -> Just c

listCursorSelectStart :: ListCursor a -> ListCursor a
listCursorSelectStart tc =
    case listCursorSelectPrev tc of
        Nothing -> tc
        Just tc' -> listCursorSelectStart tc'

listCursorSelectEnd :: ListCursor a -> ListCursor a
listCursorSelectEnd tc =
    case listCursorSelectNext tc of
        Nothing -> tc
        Just tc' -> listCursorSelectEnd tc'

listCursorInsert :: a -> ListCursor a -> ListCursor a
listCursorInsert c lc = lc {listCursorPrev = c : listCursorPrev lc}

listCursorAppend :: a -> ListCursor a -> ListCursor a
listCursorAppend c lc = lc {listCursorNext = c : listCursorNext lc}

listCursorRemove :: ListCursor a -> Maybe (ListCursor a)
listCursorRemove tc =
    case listCursorPrev tc of
        [] -> Nothing
        (_:prev) -> Just $ tc {listCursorPrev = prev}

listCursorDelete :: ListCursor a -> Maybe (ListCursor a)
listCursorDelete tc =
    case listCursorNext tc of
        [] -> Nothing
        (_:next) -> Just $ tc {listCursorNext = next}

listCursorSplit :: ListCursor a -> (ListCursor a, ListCursor a)
listCursorSplit ListCursor {..} =
    ( ListCursor {listCursorPrev = listCursorPrev, listCursorNext = []}
    , ListCursor {listCursorPrev = [], listCursorNext = listCursorNext})

listCursorCombine :: ListCursor a -> ListCursor a -> ListCursor a
listCursorCombine lc1 lc2 =
    ListCursor
    {listCursorPrev = reverse $ rebuild lc1, listCursorNext = rebuild lc2}

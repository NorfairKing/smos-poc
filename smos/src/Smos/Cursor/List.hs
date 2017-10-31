{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Smos.Cursor.List
    ( ListCursor
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
    , listCursorRemove
    , listCursorDelete
    ) where

import Import

import Smos.Cursor.Class

data ListCursor a = ListCursor
    { listCursorPrev :: [a]
    , listCursorNext :: [a]
    } deriving (Eq, Generic)

instance Validity a => Validity (ListCursor a)

instance Show a => Show (ListCursor a) where
    show ListCursor {..} =
        concat
            [ "|-"
            , show (reverse listCursorPrev)
            , "-|-"
            , show listCursorNext
            , "-|"
            ]

instance Rebuild (ListCursor a) where
    type ReBuilding (ListCursor a) = [a]
    rebuild = rebuildListCursor

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
listCursorInsert c tc =
    ListCursor
    {listCursorPrev = c : listCursorPrev tc, listCursorNext = listCursorNext tc}

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

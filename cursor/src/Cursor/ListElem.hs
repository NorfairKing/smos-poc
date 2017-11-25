{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.ListElem
    ( ListElemCursor(..)
    , ListElemView(..)
    , makeListElemCursor
    , makeNonEmptyListElemCursor
    , makeListElemCursorWithSelection
    , singletonListElemCursor
    , rebuildListElemCursor
    , listElemCursorElemL
    , listElemCursorSelectPrev
    , listElemCursorSelectNext
    , listElemCursorSelectFirst
    , listElemCursorSelectLast
    , listElemCursorInsert
    , listElemCursorAppend
    , listElemCursorInsertAndSelect
    , listElemCursorAppendAndSelect
    , listElemCursorRemoveElemAndSelectPrev
    , listElemCursorDeleteElemAndSelectNext
    , listElemCursorRemoveElem
    , listElemCursorDeleteElem
    , nonemptyPrepend
    , nonemptyAppend
    ) where

import Import

import Lens.Micro

import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NE

import Cursor.Class
import Cursor.Select

nonemptyPrepend :: [a] -> NonEmpty a -> NonEmpty a
nonemptyPrepend ls ne = foldr (<|) ne ls

nonemptyAppend :: NonEmpty a -> [a] -> NonEmpty a
nonemptyAppend (x :| xs) ls = x :| (xs ++ ls)

-- | A 'nonempty list' cursor
data ListElemCursor a = ListElemCursor
    { listElemCursorPrev :: [a] -- In reverse order
    , listElemCursorCurrent :: a
    , listElemCursorNext :: [a]
    } deriving (Eq, Generic)

instance Validity a => Validity (ListElemCursor a)

instance Show a => Show (ListElemCursor a) where
    show ListElemCursor {..} =
        concat
            [ unlines $ map (("    " ++) . show) listElemCursorPrev
            , "--> " ++ show listElemCursorCurrent ++ "\n"
            , unlines $ map (("    " ++) . show) listElemCursorNext
            ]

instance Build (ListElemCursor a) where
    type Building (ListElemCursor a) = a
    build = listElemCursorCurrent

instance Rebuild (ListElemCursor a) where
    type ReBuilding (ListElemCursor a) = ListElemView a
    rebuild ListElemCursor {..} =
        ListElemView
        { listElemViewPrev = reverse listElemCursorPrev
        , listElemViewCurrent = listElemCursorCurrent
        , listElemViewNext = listElemCursorNext
        }
    selection = (: []) . length . listElemCursorPrev

data ListElemView a = ListElemView
    { listElemViewPrev :: [a]
    , listElemViewCurrent :: a
    , listElemViewNext :: [a]
    } deriving (Eq, Generic)

instance Validity a => Validity (ListElemView a)

instance Show a => Show (ListElemView a) where
    show ListElemView {..} =
        concat
            [ unlines $ map (("    " ++) . show) listElemViewPrev
            , "--> " ++ show listElemViewCurrent ++ "\n"
            , unlines $ map (("    " ++) . show) listElemViewNext
            ]

instance View (ListElemView a) where
    type Source (ListElemView a) = NonEmpty a
    source ListElemView {..} =
        nonemptyPrepend
            listElemViewPrev
            (listElemViewCurrent :| listElemViewNext)
    view = rebuild . makeListElemCursor

instance Selectable a => Selectable (ListElemView a) where
    applySelection =
        drillWithSel $ \mixr_ lec ->
            case mixr_ of
                Nothing -> view $ source lec
                Just (ix_, sel) ->
                    rebuild $
                    makeListElemCursorWithSelection ix_ (source lec) &
                    listElemCursorElemL %~
                    applySelection (Just sel)

makeListElemCursor :: NonEmpty a -> ListElemCursor a
makeListElemCursor = makeListElemCursorWithSelection 0

makeListElemCursorWithSelection :: Int -> NonEmpty a -> ListElemCursor a
makeListElemCursorWithSelection i ne =
    let (l, m, r) = applyListSelection ne i
    in ListElemCursor
       { listElemCursorPrev = reverse l
       , listElemCursorCurrent = m
       , listElemCursorNext = r
       }
  where
    applyListSelection :: NonEmpty a -> Int -> ([a], a, [a])
    applyListSelection (c :| rest) i_
        | i_ <= 0 = ([], c, rest)
        | otherwise =
            case NE.nonEmpty rest of
                Nothing -> ([], c, [])
                Just ne_ ->
                    let (l, m, r) = applyListSelection ne_ (i_ - 1)
                    in (c : l, m, r)

makeNonEmptyListElemCursor :: [a] -> Maybe (ListElemCursor a)
makeNonEmptyListElemCursor = fmap makeListElemCursor . NE.nonEmpty

singletonListElemCursor :: a -> ListElemCursor a
singletonListElemCursor a = makeListElemCursor $ a :| []

rebuildListElemCursor :: ListElemCursor a -> NonEmpty a
rebuildListElemCursor = source . rebuild

listElemCursorElemL :: Lens' (ListElemCursor a) a
listElemCursorElemL =
    lens listElemCursorCurrent $ \lec le -> lec {listElemCursorCurrent = le}

listElemCursorSelectPrev :: ListElemCursor a -> Maybe (ListElemCursor a)
listElemCursorSelectPrev lec =
    case listElemCursorPrev lec of
        [] -> Nothing
        (e:rest) ->
            Just $
            lec
            { listElemCursorPrev = rest
            , listElemCursorCurrent = e
            , listElemCursorNext =
                  listElemCursorCurrent lec : listElemCursorNext lec
            }

listElemCursorSelectNext :: ListElemCursor a -> Maybe (ListElemCursor a)
listElemCursorSelectNext lec =
    case listElemCursorNext lec of
        [] -> Nothing
        (e:rest) ->
            Just $
            lec
            { listElemCursorPrev =
                  listElemCursorCurrent lec : listElemCursorPrev lec
            , listElemCursorCurrent = e
            , listElemCursorNext = rest
            }

listElemCursorSelectFirst :: ListElemCursor a -> ListElemCursor a
listElemCursorSelectFirst = go
  where
    go lec =
        case listElemCursorSelectPrev lec of
            Nothing -> lec
            Just lec' -> go lec'

listElemCursorSelectLast :: ListElemCursor a -> ListElemCursor a
listElemCursorSelectLast = go
  where
    go lec =
        case listElemCursorSelectNext lec of
            Nothing -> lec
            Just lec' -> go lec'

listElemCursorInsert :: a -> ListElemCursor a -> ListElemCursor a
listElemCursorInsert c lec =
    lec {listElemCursorPrev = c : listElemCursorPrev lec}

listElemCursorAppend :: a -> ListElemCursor a -> ListElemCursor a
listElemCursorAppend c lec =
    lec {listElemCursorNext = c : listElemCursorNext lec}

listElemCursorInsertAndSelect :: a -> ListElemCursor a -> ListElemCursor a
listElemCursorInsertAndSelect c lec =
    lec
    { listElemCursorCurrent = c
    , listElemCursorNext = listElemCursorCurrent lec : listElemCursorNext lec
    }

listElemCursorAppendAndSelect :: a -> ListElemCursor a -> ListElemCursor a
listElemCursorAppendAndSelect c lec =
    lec
    { listElemCursorCurrent = c
    , listElemCursorPrev = listElemCursorCurrent lec : listElemCursorPrev lec
    }

listElemCursorRemoveElemAndSelectPrev ::
       ListElemCursor a -> Maybe (ListElemCursor a)
listElemCursorRemoveElemAndSelectPrev lec =
    case listElemCursorPrev lec of
        [] -> Nothing
        (e:rest) ->
            Just $ lec {listElemCursorPrev = rest, listElemCursorCurrent = e}

listElemCursorDeleteElemAndSelectNext ::
       ListElemCursor a -> Maybe (ListElemCursor a)
listElemCursorDeleteElemAndSelectNext lec =
    case listElemCursorNext lec of
        [] -> Nothing
        (e:rest) ->
            Just $ lec {listElemCursorCurrent = e, listElemCursorNext = rest}

listElemCursorRemoveElem :: ListElemCursor a -> Maybe (ListElemCursor a)
listElemCursorRemoveElem lec =
    listElemCursorRemoveElemAndSelectPrev lec <|>
    listElemCursorDeleteElemAndSelectNext lec

listElemCursorDeleteElem :: ListElemCursor a -> Maybe (ListElemCursor a)
listElemCursorDeleteElem lec =
    listElemCursorDeleteElemAndSelectNext lec <|>
    listElemCursorRemoveElemAndSelectPrev lec

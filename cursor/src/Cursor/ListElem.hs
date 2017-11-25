{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.ListElem
    ( ListElemCursor(..)
    , makeListElemCursor
    , makeNonEmptyListElemCursor
    , singletonListElemCursor
    , rebuildListElemCursor
    , listElemCursorElemL
    , listElemCursorSelectPrev
    , listElemCursorSelectNext
    , listElemCursorRemoveElemAndSelectPrev
    , listElemCursorDeleteElemAndSelectNext
    , listElemCursorRemoveElem
    , listElemCursorDeleteElem
    ) where

import Import

import Lens.Micro

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import Cursor.Class
import Cursor.Select

-- | A 'nonempty list' cursor
data ListElemCursor a = ListElemCursor
    { listElemCursorPrev :: [a]
    , listElemCursorCurrent :: a
    , listElemCursorNext :: [a]
    } deriving (Eq, Generic)

instance Validity a => Validity (ListElemCursor a)

instance Show a => Show (ListElemCursor a) where
    show ListElemCursor {..} =
        concat
            [ unlines $ map (("    " ++) . show) listElemCursorPrev
            , "--> " ++ show listElemCursorCurrent
            , unlines $ map (("    " ++) . show) listElemCursorNext
            ]

instance Build (ListElemCursor a) where
    type Building (ListElemCursor a) = a
    build = listElemCursorCurrent

instance Rebuild (ListElemCursor a) where
    type ReBuilding (ListElemCursor a) = NonEmpty a
    rebuild = rebuildListElemCursor
    selection = (: []) . length . listElemCursorPrev

instance Selectable a => Selectable (ListElemCursor a) where
    applySelection =
        drillWithSel $ \mixr_ lec ->
            case mixr_ of
                Nothing -> lec
                Just (ix_, sel) ->
                    makeListElemCursorWithSelection ix_ (rebuild lec) &
                    listElemCursorElemL %~
                    applySelection (Just sel)

makeListElemCursor :: NonEmpty a -> ListElemCursor a
makeListElemCursor = makeListElemCursorWithSelection 0

makeListElemCursorWithSelection :: Int -> NonEmpty a -> ListElemCursor a
makeListElemCursorWithSelection i ne =
    let (l, m, r) = applyListSelection ne i
    in ListElemCursor
       { listElemCursorPrev = l
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
rebuildListElemCursor ListElemCursor {..} =
    listElemCursorCurrent :| listElemCursorNext

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

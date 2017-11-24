{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.ListElem
    ( ListElemCursor
    , listElemCursorPrev
    , listElemCursorCurrent
    , listElemCursorNext
    , makeListElemCursor
    , makeNonEmptyListElemCursor
    , rebuildListElemCursor
    ) where

import Import

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import Cursor.Class

data ListElemCursor a = ListElemCursor
    { listElemCursorPrev :: [a]
    , listElemCursorCurrent :: a
    , listElemCursorNext :: [a]
    } deriving (Show, Eq, Generic)

instance Validity a => Validity (ListElemCursor a)

instance Build (ListElemCursor a) where
    type Building (ListElemCursor a) = a
    build = listElemCursorCurrent

instance Rebuild (ListElemCursor a) where
    type ReBuilding (ListElemCursor a) = NonEmpty a
    rebuild = rebuildListElemCursor
    selection = (: []) . length . listElemCursorPrev

makeListElemCursor :: NonEmpty a -> ListElemCursor a
makeListElemCursor (c :| n) =
    ListElemCursor
    {listElemCursorPrev = [], listElemCursorCurrent = c, listElemCursorNext = n}

makeNonEmptyListElemCursor :: [a] -> Maybe (ListElemCursor a)
makeNonEmptyListElemCursor = fmap makeListElemCursor . NE.nonEmpty

rebuildListElemCursor :: ListElemCursor a -> NonEmpty a
rebuildListElemCursor ListElemCursor {..} =
    listElemCursorCurrent :| listElemCursorNext

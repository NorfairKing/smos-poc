{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.ListElem.Gen
    ( listElemElemOf
    ) where

import Import

import Cursor.ListElem

instance GenUnchecked a => GenUnchecked (ListElemCursor a)

instance GenValid a => GenValid (ListElemCursor a)

listElemElemOf :: ListElemCursor a -> Gen a
listElemElemOf ListElemCursor {..} =
    elements $
    listElemCursorPrev ++ [listElemCursorCurrent] ++ listElemCursorNext

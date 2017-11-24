{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.ListElem.Gen where

import Import

import Cursor.ListElem

instance GenUnchecked a => GenUnchecked (ListElemCursor a)

instance GenValid a => GenValid (ListElemCursor a)

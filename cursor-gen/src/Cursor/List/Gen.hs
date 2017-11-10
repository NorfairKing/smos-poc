{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.List.Gen where

import Import

import Cursor.List

instance GenUnchecked a => GenUnchecked (ListCursor a)

instance GenValid a => GenValid (ListCursor a)

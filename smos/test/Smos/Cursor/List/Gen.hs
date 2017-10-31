{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.List.Gen where

import TestImport

import Smos.Cursor.List

instance GenUnchecked a => GenUnchecked (ListCursor a)

instance GenValid a => GenValid (ListCursor a)

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Text.Gen where

import TestImport

import Smos.Cursor.List.Gen ()
import Smos.Cursor.Text

instance GenUnchecked TextCursor

instance GenValid TextCursor

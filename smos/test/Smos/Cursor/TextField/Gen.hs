{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.TextField.Gen where

import TestImport

import Smos.Cursor.List.Gen ()
import Smos.Cursor.TextField

instance GenUnchecked TextFieldCursor

instance GenValid TextFieldCursor

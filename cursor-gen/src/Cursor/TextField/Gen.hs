{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.TextField.Gen where

import Import

import Cursor.TextField

import Cursor.ListElem.Gen ()
import Cursor.Text.Gen ()

instance GenUnchecked TextFieldCursor

instance GenValid TextFieldCursor

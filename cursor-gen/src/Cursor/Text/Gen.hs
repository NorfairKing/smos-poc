{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Text.Gen where

import Import

import Cursor.List.Gen ()
import Cursor.Text

instance GenUnchecked TextCursor

instance GenValid TextCursor

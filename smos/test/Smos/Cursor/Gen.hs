{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Gen where

import TestImport

import Smos.Cursor
import Smos.Cursor.Entry.Gen ()
import Smos.Cursor.Tree.Gen ()
import Smos.Data.Gen ()

instance GenUnchecked AnyCursor

instance GenValid AnyCursor

instance GenUnchecked ACursor

instance GenValid ACursor

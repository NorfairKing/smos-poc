{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Gen where

import TestImport

import Cursor.Tree.Gen ()

import Smos.Cursor
import Smos.Cursor.Entry.Gen ()
import Smos.Data.Gen ()

instance GenUnchecked AnyCursor

instance GenValid AnyCursor where
    genValid =
        oneof
            [ AnyForest <$> genValid
            , AnyTree <$> genValid
            , AnyEntry <$> genValid
            , AnyHeader <$> genValid
            , AnyContents <$> genValid
            , AnyState <$> genValid
            , AnyTags <$> genValid
            ]

instance GenUnchecked ACursor

instance GenValid ACursor where
    genValid =
        oneof
            [ AnEntry <$> genValid
            , AHeader <$> genValid
            , AContents <$> genValid
            , ATags <$> genValid
            ]

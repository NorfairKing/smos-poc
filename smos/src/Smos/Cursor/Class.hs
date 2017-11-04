{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Smos.Cursor.Class
    ( Cursor
    , Rebuild(..)
    , Build(..)
    ) where

import Import

type Cursor a = (Build a, Rebuild a)

class Rebuild a where
    type ReBuilding a :: *
    -- | Rebuild to the entire thing
    rebuild :: a -> ReBuilding a
    -- | The path upward to the ReBuilding
    selection :: a -> [Int]

class Build a where
    type Building a :: *
    -- | Build to the thing we're looking at
    build :: a -> Building a

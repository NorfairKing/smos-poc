{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Smos.Cursor.Class
    ( Cursor
    , Rebuild(..)
    , Build(..)
    ) where

type Cursor a = (Build a, Rebuild a)

class Rebuild a where
    type ReBuilding a :: *
    rebuild :: a -> ReBuilding a

class Build a where
    type Building a :: *
    build :: a -> Building a

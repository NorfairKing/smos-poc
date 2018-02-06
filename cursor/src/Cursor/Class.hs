{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Cursor.Class
    ( View(..)
    , Rebuild(..)
    , Build(..)
    , BuiltFrom(..)
    , Reselect(..)
    , reselectLike
    , NOUOD(..)
    ) where

import Import

-- | The datastructure that represents what the cursor should look like.
-- This structure should contain data about representation like, for example,
-- whether or not a subsection should be shown. The 'Source' is the data that
-- is being edited and will not have this information
class View a where
    type Source a :: *
    source :: a -> Source a
    view :: Source a -> a

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

class BuiltFrom a b where
    type Parent a :: *
    makeWith :: Parent a -> b -> a

class Reselect a where
    type Reselection a :: *
    reselect :: [Int] -> a -> Reselection a

reselectLike :: (Reselect a, Rebuild b) => a -> b -> Reselection a
reselectLike a b = reselect (selection b) a

data NOUOD a
    = New a
    | Unchanged
    | Deleted
    deriving (Show, Eq, Generic)

instance Validity a => Validity( NOUOD a)

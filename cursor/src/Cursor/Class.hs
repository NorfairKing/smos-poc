{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}

module Cursor.Class
    ( Cursor
    , Rebuild(..)
    , Build(..)
    , BuiltFrom(..)
    , Reselect(..)
    , drillSel
    , reselectLike
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

class BuiltFrom a b where
    type Parent a :: *
    makeWith :: Parent a -> b -> a

class Reselect a where
    type Reselection a :: *
    reselect :: [Int] -> a -> Reselection a

drillSel :: Maybe [Int] -> Int -> Maybe [Int]
drillSel msel ix =
    case msel of
        Nothing -> Nothing
        Just [] -> Nothing
        Just (x:xs) ->
            if x == ix
                then Just xs
                else Nothing

reselectLike :: (Reselect a, Rebuild b) => a -> b -> Reselection a
reselectLike a b = reselect (selection b) a

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Smos.Cursor.Class
    ( Cursor
    , Rebuild(..)
    , Build(..)
    , drillSel
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

drillSel :: Maybe [Int] -> Int -> Maybe [Int]
drillSel msel ix =
    case msel of
        Nothing -> Nothing
        Just [] -> Nothing
        Just (x:xs) ->
            if x == ix
                then Just xs
                else Nothing

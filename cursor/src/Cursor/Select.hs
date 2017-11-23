{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Cursor.Select where

import Import

import Lens.Micro

data Select a = Select
    { selected :: Bool
    , selectValue :: a
    } deriving (Show, Eq, Generic)

instance Validity a => Validity (Select a)

instance Functor Select where
    fmap f (Select b a) = Select b $ f a

select :: a -> Select a
select a = Select {selected = False, selectValue = a}

selectedL :: Lens' (Select a) Bool
selectedL = lens selected (\s b -> s {selected = b})

selectValueL :: Lens' (Select a) a
selectValueL = lens selectValue (\s v -> s {selectValue = v})

class Selectable a where
    applySelection :: Maybe [Int] -> a -> a

drillSel :: Int -> Maybe [Int] -> Maybe [Int]
drillSel ix msel =
    case msel of
        Nothing -> Nothing
        Just [] -> Nothing
        Just (x:xs) ->
            if x == ix
                then Just xs
                else Nothing

drill :: Int -> (Maybe [Int] -> a -> a) -> Maybe [Int] -> Select a -> Select a
drill i func msel =
    case msel of
        Nothing -> id
        Just [] -> id
        Just (x:xs) ->
            if x == i
                then selectedL .~ True
                else id

drillApply :: Selectable a => Int -> Maybe [Int] -> Select a -> Select a
drillApply ix_ = drill ix_ applySelection

drillStop :: Int -> Maybe [Int] -> Select a -> Select a
drillStop ix_ = drill ix_ $ flip const

instance Selectable a => Selectable [Select a] where
    applySelection msel list =
        flip map (zip [0 ..] list) $ \(ix_, v) -> drillApply ix_ msel v

drillStopList :: Maybe [Int] -> [Select a] -> [Select a]
drillStopList msel list =
    flip map (zip [0 ..] list) $ \(ix_, v) -> drillStop ix_ msel v

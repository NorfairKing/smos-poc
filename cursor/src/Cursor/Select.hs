{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Cursor.Select where

import Import

import Data.Hashable

import Lens.Micro

data Select a = Select
    { selected :: Bool
    , selectValue :: a
    } deriving (Show, Eq, Generic)

instance Validity a => Validity (Select a)

instance Hashable a => Hashable (Select a)

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

unconsSel :: Maybe [Int] -> Maybe (Int, [Int])
unconsSel = (>>= uncons)

drillSel :: Int -> Maybe [Int] -> Maybe [Int]
drillSel i msel =
    case unconsSel msel of
        Nothing -> Nothing
        Just (x, xs) ->
            if x == i
                then Just xs
                else Nothing

drillWithSel :: (Maybe (Int, [Int]) -> a -> a) -> Maybe [Int] -> a -> a
drillWithSel func msel = func (unconsSel msel)

drillWithSel_ :: (Maybe Int -> a -> a) -> Maybe [Int] -> a -> a
drillWithSel_ func msel = func (fst <$> unconsSel msel)

drill :: Int -> (Maybe [Int] -> a -> a) -> Maybe [Int] -> Select a -> Select a
drill i func msel s =
    let msel' = drillSel i msel
        setSelect =
            case msel of
                Just [x] ->
                    if x == i
                        then selectedL .~ True
                        else id
                _ -> id
    in func msel' <$> setSelect s

drillPrefix ::
       Int -> (Maybe [Int] -> a -> a) -> Maybe [Int] -> Select a -> Select a
drillPrefix i func msel s =
    let msel' = drillSel i msel
        setSelect =
            case msel of
                Just (x:_) ->
                    if x == i
                        then selectedL .~ True
                        else id
                _ -> id
    in func msel' <$> setSelect s

drillApply :: Selectable a => Int -> Maybe [Int] -> Select a -> Select a
drillApply ix_ = drill ix_ applySelection

drillPrefixApply :: Selectable a => Int -> Maybe [Int] -> Select a -> Select a
drillPrefixApply ix_ = drillPrefix ix_ applySelection

drillStop :: Int -> Maybe [Int] -> Select a -> Select a
drillStop ix_ = drill ix_ $ flip const

drillPrefixStop :: Int -> Maybe [Int] -> Select a -> Select a
drillPrefixStop ix_ = drillPrefix ix_ $ flip const

instance Selectable a => Selectable [Select a] where
    applySelection msel list =
        flip map (zip [0 ..] list) $ \(ix_, v) -> drillApply ix_ msel v

drillStopList :: Maybe [Int] -> [Select a] -> [Select a]
drillStopList msel list =
    flip map (zip [0 ..] list) $ \(ix_, v) -> drillStop ix_ msel v

drillPrefixApplyList :: Selectable a => Maybe [Int] -> [Select a] -> [Select a]
drillPrefixApplyList msel list =
    flip map (zip [0 ..] list) $ \(ix_, v) -> drillPrefixApply ix_ msel v

{-# LANGUAGE DeriveGeneric #-}

module Cursor.Select where

import Import

data Select a = Select
    { selected :: Bool
    , selectValue :: a
    } deriving (Show, Eq, Generic)

instance Validity a => Validity (Select a)

instance Functor Select where
    fmap f (Select b a) = Select b $ f a

select :: a -> Select a
select a = Select {selected = False, selectValue = a}

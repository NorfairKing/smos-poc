{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Smos.Types where

import Import

import Data.String

import Smos.Cursor

newtype SmosState = SmosState
    { smosStateCursor :: ACursor
    } deriving (Generic)

newtype ResourceName = ResourceName
    { unResourceName :: String
    } deriving (Show, Eq, Ord, Generic, IsString)

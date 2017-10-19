{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Smos.Types where

import Import

import Data.String

import Smos.Data.Types

newtype SmosState = SmosState
    { smosFile :: SmosFile
    } deriving (Show, Eq, Generic)

newtype ResourceName = ResourceName
    { unResourceName :: String
    } deriving (Show, Eq, Ord, Generic, IsString)

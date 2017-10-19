{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Smos.Types where

import Import

import Data.String

newtype ResourceName = ResourceName
    { unResourceName :: String
    } deriving (Show, Eq, Ord, Generic, IsString)

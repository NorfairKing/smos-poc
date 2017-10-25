{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Smos.Types where

import Import

import Data.Map (Map)
import Data.String

import Brick.Types as B

import Smos.Cursor

newtype SmosConfig e = SmosConfig
    { keyMap :: Map (BrickEvent ResourceName e) (SmosM ())
    } deriving (Generic)

newtype SmosState = SmosState
    { smosStateCursor :: Maybe ACursor
    } deriving (Generic)

newtype ResourceName = ResourceName
    { unResourceName :: Text
    } deriving (Show, Eq, Ord, Generic, IsString)

type SmosM = EventM ResourceName SmosState

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module Cursor.MapSpec
    ( spec
    ) where

import TestImport

import Cursor.Class
import Cursor.Map
import Cursor.Map.Gen ()

spec :: Spec
spec = do
    genValidSpec @(MapCursor Int Int)
    genValidSpec @(KeyValueCursor Int Int)
    genValidSpec @(KeyCursor Int Int)
    genValidSpec @(ValueCursor Int Int)

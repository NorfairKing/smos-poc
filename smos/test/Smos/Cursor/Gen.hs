{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Gen where

import TestImport

import Smos.Cursor
import Smos.Data.Gen ()

instance GenUnchecked ForestCursor where
    genUnchecked = makeForestCursor <$> genUnchecked
    shrinkUnchecked = shrinkNothing

instance GenValid ForestCursor where
    genValid = makeForestCursor <$> genValid

instance GenUnchecked TreeCursor where
    genUnchecked = do
        sf <- genUnchecked
        case forestCursorSelectFirst $ makeForestCursor sf of
            Nothing -> genUnchecked
            Just tc -> pure tc
    shrinkUnchecked = shrinkNothing

instance GenValid TreeCursor where
    genValid = do
        sf <- genValid
        case forestCursorSelectFirst $ makeForestCursor sf of
            Nothing -> genValid
            Just tc -> pure tc

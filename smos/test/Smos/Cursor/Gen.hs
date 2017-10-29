{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Gen where

import TestImport

import Smos.Cursor
import Smos.Data.Gen ()

instance GenUnchecked AnyCursor

instance GenValid AnyCursor

instance GenUnchecked ACursor

instance GenValid ACursor

instance GenUnchecked ForestCursor where
    genUnchecked = makeForestCursor <$> genUnchecked
    shrinkUnchecked = shrinkNothing

instance GenValid ForestCursor where
    genValid = makeForestCursor <$> genValid

instance GenUnchecked TreeCursor where
    genUnchecked = do
        sf <- genUnchecked
        -- TODO don't juts select the first.
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

instance GenUnchecked EntryCursor where
    genUnchecked = treeCursorEntry <$> genUnchecked
    shrinkUnchecked = shrinkNothing

instance GenValid EntryCursor where
    genValid = treeCursorEntry <$> genValid

instance GenUnchecked HeaderCursor where
    genUnchecked = entryCursorHeader <$> genUnchecked
    shrinkUnchecked = shrinkNothing

instance GenValid HeaderCursor where
    genValid = entryCursorHeader <$> genValid

instance GenUnchecked StateCursor where
    genUnchecked = entryCursorState <$> genUnchecked
    shrinkUnchecked = shrinkNothing

instance GenValid StateCursor where
    genValid = entryCursorState <$> genValid

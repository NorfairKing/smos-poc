{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Entry.Gen where

import TestImport

import Smos.Cursor.Entry
import Smos.Cursor.Text.Gen ()
import Smos.Cursor.Tree
import Smos.Cursor.Tree.Gen ()
import Smos.Data.Gen ()

instance GenUnchecked EntryCursor where
    genUnchecked = treeCursorValue <$> genUnchecked
    shrinkUnchecked = shrinkNothing

instance GenValid EntryCursor where
    genValid = treeCursorValue <$> genValid

instance GenUnchecked HeaderCursor where
    genUnchecked = entryCursorHeader <$> genUnchecked
    shrinkUnchecked = shrinkNothing

instance GenValid HeaderCursor where
    genValid = entryCursorHeader <$> genValid

instance GenUnchecked ContentsCursor where
    genUnchecked =
        genUnchecked >>= \ec ->
            case entryCursorContents ec of
                Nothing -> genUnchecked
                Just c -> pure c
    shrinkUnchecked = shrinkNothing

instance GenValid ContentsCursor where
    genValid =
        genValid >>= \ec ->
            case entryCursorContents ec of
                Nothing -> genValid
                Just c -> pure c

instance GenUnchecked StateCursor where
    genUnchecked = entryCursorState <$> genUnchecked
    shrinkUnchecked = shrinkNothing

instance GenValid StateCursor where
    genValid = entryCursorState <$> genValid

instance GenUnchecked TagsCursor where
    genUnchecked = entryCursorTags <$> genUnchecked
    shrinkUnchecked = shrinkNothing

instance GenValid TagsCursor where
    genValid = entryCursorTags <$> genValid

instance GenUnchecked TagCursor where
    genUnchecked = do
        tsc <- genUnchecked
        case tagsCursorTags tsc of
            [] -> scale (+ 1) genUnchecked
            tcs -> elements tcs
    shrinkUnchecked = shrinkNothing

instance GenValid TagCursor where
    genValid = do
        tsc <- genValid
        case tagsCursorTags tsc of
            [] -> scale (+ 1) genValid
            tcs -> elements tcs

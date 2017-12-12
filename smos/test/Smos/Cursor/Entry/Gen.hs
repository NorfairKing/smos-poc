{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Entry.Gen where

import TestImport

import Cursor.Tree

import Smos.Cursor.Types

import Cursor.ListElem.Gen ()
import Cursor.Map.Gen ()
import Cursor.Text.Gen ()
import Cursor.TextField.Gen ()
import Cursor.Tree.Gen ()

import Smos.Data.Gen ()

instance GenUnchecked EntryCursor

instance GenValid EntryCursor where
    genValid = treeCursorValue <$> genValid

instance GenUnchecked HeaderCursor

instance GenValid HeaderCursor where
    genValid = entryCursorHeader <$> genValid

instance GenUnchecked ContentsCursor

instance GenValid ContentsCursor where
    genValid =
        genValid >>= \ec ->
            case entryCursorContents ec of
                Nothing -> scale (+ 1) genValid
                Just c -> pure c

instance GenUnchecked StateCursor

instance GenValid StateCursor where
    genValid = entryCursorState <$> genValid

instance GenUnchecked TagsCursor

instance GenValid TagsCursor where
    genValid = do
        ec <- genValid
        case entryCursorTags ec of
            Nothing -> scale (+ 1) genValid
            Just tc -> pure tc

instance GenUnchecked TagCursor

instance GenValid TagCursor where
    genValid = TagCursor <$> genValid

instance GenUnchecked TimestampsCursor

instance GenValid TimestampsCursor where
    genValid = do
        ec <- genValid
        case entryCursorTimestamps ec of
            Nothing -> scale (+ 1) genValid
            Just tc -> pure tc

instance GenUnchecked TimestampNameCursor

instance GenValid TimestampNameCursor where
    genValid = TimestampNameCursor <$> genValid

instance GenUnchecked TimestampCursor

instance GenValid TimestampCursor where
    genValid = TimestampCursor <$> genValid <*> genValid

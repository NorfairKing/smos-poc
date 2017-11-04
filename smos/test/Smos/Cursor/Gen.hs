{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Gen where

import TestImport

import Smos.Cursor
import Smos.Cursor.Class
import Smos.Cursor.Tree
import Smos.Data.Gen ()

instance GenUnchecked AnyCursor

instance GenValid AnyCursor

instance GenUnchecked ACursor

instance GenValid ACursor

instance ( GenUnchecked a
         , Build a
         , GenUnchecked (Building a)
         , Parent a ~ TreeCursor a
         , a `BuiltFrom` (Building a)
         ) =>
         GenUnchecked (ForestCursor a) where
    genUnchecked = do
        fc <- makeForestCursor <$> genUnchecked
        let go fc_ = do
                b <- genUnchecked
                if b
                    then pure fc_
                    else case forestCursorElems fc_ of
                             [] -> pure fc_
                             els -> do
                                 tc <- elements els
                                 go $ treeCursorForest tc
        go fc
    shrinkUnchecked = shrinkNothing

instance ( GenValid a
         , Build a
         , GenValid (Building a)
         , Validity (Building a)
         , Parent a ~ TreeCursor a
         , a `BuiltFrom` (Building a)
         ) =>
         GenValid (ForestCursor a) where
    genValid = do
        fc <- makeForestCursor <$> genValid
        let go fc_ = do
                b <- genValid
                if b
                    then pure fc_
                    else case forestCursorElems fc_ of
                             [] -> pure fc_
                             els -> do
                                 tc <- elements els
                                 go $ treeCursorForest tc
        go fc

instance ( GenUnchecked a
         , Build a
         , GenUnchecked (Building a)
         , Parent a ~ TreeCursor a
         , a `BuiltFrom` (Building a)
         ) =>
         GenUnchecked (TreeCursor a) where
    genUnchecked = do
        sf <- genUnchecked
        let go tc = do
                b <- genUnchecked
                if b
                    then pure tc
                    else case forestCursorElems $ treeCursorForest tc of
                             [] -> pure tc
                             els -> elements els >>= go
        case forestCursorElems sf of
            [] -> scale (+ 1) genUnchecked
            els -> elements els >>= go
    shrinkUnchecked = shrinkNothing

instance ( GenValid a
         , Build a
         , GenValid (Building a)
         , Validity (Building a)
         , Parent a ~ TreeCursor a
         , a `BuiltFrom` (Building a)
         ) =>
         GenValid (TreeCursor a) where
    genValid = do
        sf <- genValid
        let go tc = do
                b <- genValid
                if b
                    then pure tc
                    else case forestCursorElems $ treeCursorForest tc of
                             [] -> pure tc
                             els -> elements els >>= go
        case forestCursorElems sf of
            [] -> scale (+ 1) genValid
            els -> elements els >>= go

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

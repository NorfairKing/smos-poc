{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cursor.Tree.Gen where

import TestImport

import Cursor.Class
import Cursor.Tree

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

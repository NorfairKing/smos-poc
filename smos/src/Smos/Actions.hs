{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions
    ( insertHeaderAbove
    , moveUp
    , moveDown
    , halt
    , modify
    ) where

import Import

import qualified Data.HashMap.Lazy as HM

import Control.Monad.State

import Brick.Main as B

import Smos.Data

import Smos.Cursor
import Smos.Types

insertHeaderAbove :: SmosM ()
insertHeaderAbove =
    modify $ \ss ->
        let newE =
                SmosTree
                { treeEntry =
                      Entry
                      { entryHeader = "new"
                      , entryContents = Nothing
                      , entryTimestamps = HM.empty
                      , entryState = Nothing
                      , entryTags = []
                      , entryLogbook = LogEnd
                      }
                , treeForest = SmosForest []
                }
        in case smosStateCursor ss of
               AForest fc ->
                   let fc' = forestCursorInsertAtStart fc newE
                   in ss {smosStateCursor = AForest fc'}
               ATree tc ->
                   let tc' = treeCursorInsertAbove tc newE
                   in ss {smosStateCursor = ATree tc'}

moveUp :: SmosM ()
moveUp =
    modify $ \ss ->
        case smosStateCursor ss of
            AForest fc ->
                let mfc' = forestCursorSelectLast fc
                in ss
                   { smosStateCursor =
                         case mfc' of
                             Nothing -> AForest fc
                             Just tc -> ATree tc
                   }
            ATree tc ->
                let mtc' = treeCursorSelectPrev tc
                in ss {smosStateCursor = maybe (smosStateCursor ss) ATree mtc'}

moveDown :: SmosM ()
moveDown =
    modify $ \ss ->
        case smosStateCursor ss of
            AForest fc ->
                let mfc' = forestCursorSelectFirst fc
                in ss
                   { smosStateCursor =
                         case mfc' of
                             Nothing -> AForest fc
                             Just tc -> ATree tc
                   }
            ATree tc ->
                let mtc' = treeCursorSelectNext tc
                in ss {smosStateCursor = maybe (smosStateCursor ss) ATree mtc'}

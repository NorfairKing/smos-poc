{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions
    ( halt
    -- * Ready-made actions
    , insertHeaderAbove
    , deleteCurrentHeader
    , moveUp
    , moveDown
    -- * Helper functions to define your own actions
    , modify
    , withForestCursor
    , withTreeCursor
    ) where

import Import

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
                {treeEntry = newEntry "NEW HEADER", treeForest = SmosForest []}
        in case smosStateCursor ss of
               AForest fc ->
                   let fc' = forestCursorInsertAtStart fc newE
                   in ss {smosStateCursor = AForest fc'}
               ATree tc ->
                   let tc' = treeCursorInsertAbove tc newE
                   in ss {smosStateCursor = ATree tc'}

deleteCurrentHeader :: SmosM ()
deleteCurrentHeader =
    withTreeCursor $ \tc ->
        modify (\ss -> ss {smosStateCursor = treeCursorDeleteCurrent tc})

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

withTreeCursor :: (TreeCursor -> SmosM ()) -> SmosM ()
withTreeCursor func = do
    ss <- get
    case smosStateCursor ss of
        ATree tc -> func tc
        _ -> pure ()

withForestCursor :: (ForestCursor -> SmosM ()) -> SmosM ()
withForestCursor func = do
    ss <- get
    case smosStateCursor ss of
        AForest fc -> func fc
        _ -> pure ()

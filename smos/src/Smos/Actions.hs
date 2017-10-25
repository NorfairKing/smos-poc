{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions
    ( halt
    -- * Ready-made actions
    , insertTreeAbove
    , deleteCurrentHeader
    , moveUp
    , moveDown
    -- * Helper functions to define your own actions
    , modify
    , withEntryCursor
    , withHeaderCursor
    ) where

import Import

import Control.Monad.State

import Brick.Main as B

import Smos.Data

import Smos.Cursor
import Smos.Types

insertTreeAbove :: SmosM ()
insertTreeAbove =
    modify $ \ss ->
        let newT =
                SmosTree
                {treeEntry = newEntry "NEW HEADER", treeForest = SmosForest []}
        in case smosStateCursor ss of
               Nothing ->
                   let fc = makeForestCursor $ SmosForest []
                       fc' = forestCursorInsertAtStart fc newT
                       mtc' = forestCursorSelectFirst fc'
                   in ss
                      {smosStateCursor = (AnEntry . treeCursorEntry) <$> mtc'}
               Just (AnEntry ec) ->
                   let tc = entryCursorParent ec
                       tc' = treeCursorInsertAbove tc newT
                       ec' = treeCursorEntry tc'
                   in ss {smosStateCursor = Just $ AnEntry ec'}
               _ -> ss

deleteCurrentHeader :: SmosM ()
deleteCurrentHeader =
    withEntryCursor $ \ec ->
        let tc = entryCursorParent ec
            eft = treeCursorDeleteCurrent tc
            mec' =
                case eft of
                    Left fc ->
                        case forestCursorParent fc of
                            Nothing -> Nothing
                            Just tc_ -> Just $ treeCursorEntry tc_
                    Right tc_ -> Just $ treeCursorEntry tc_
        in modify (\ss -> ss {smosStateCursor = AnEntry <$> mec'})

moveUp :: SmosM ()
moveUp =
    modify $ \ss ->
        case smosStateCursor ss of
            Just (AnEntry ec) ->
                let tc = entryCursorParent ec
                    tc' = fromMaybe tc $ treeCursorSelectPrev tc
                    ec' = treeCursorEntry tc'
                in ss {smosStateCursor = Just $ AnEntry ec'}
            _ -> ss
            -- AForest fc ->
            --     let mfc' = forestCursorSelectLast fc
            --     in ss
            --        { smosStateCursor =
            --              case mfc' of
            --                  Nothing -> AForest fc
            --                  Just tc -> ATree tc
            --        }
            -- ATree tc ->
            --     let mtc' = treeCursorSelectPrev tc
            --     in ss {smosStateCursor = maybe (smosStateCursor ss) ATree mtc'}
            -- AnEntry _ -> ss

moveDown :: SmosM ()
moveDown =
    modify $ \ss ->
        case smosStateCursor ss of
            Just (AnEntry ec) ->
                let tc = entryCursorParent ec
                    tc' = fromMaybe tc $ treeCursorSelectNext tc
                    ec' = treeCursorEntry tc'
                in ss {smosStateCursor = Just (AnEntry ec')}
            _ -> ss
    -- modify $ \ss ->
    --     case smosStateCursor ss of
    --         AForest fc ->
    --             let mfc' = forestCursorSelectFirst fc
    --             in ss
    --                { smosStateCursor =
    --                      case mfc' of
    --                          Nothing -> AForest fc
    --                          Just tc -> ATree tc
    --                }
    --         ATree tc ->
    --             let mtc' = treeCursorSelectNext tc
    --             in ss {smosStateCursor = maybe (smosStateCursor ss) ATree mtc'}
    --         AnEntry _ -> ss

withEntryCursor :: (EntryCursor -> SmosM ()) -> SmosM ()
withEntryCursor func = do
    ss <- get
    case smosStateCursor ss of
        Just (AnEntry fc) -> func fc
        _ -> pure ()

withHeaderCursor :: (HeaderCursor -> SmosM ()) -> SmosM ()
withHeaderCursor func = do
    ss <- get
    case smosStateCursor ss of
        Just (AHeader fc) -> func fc
        _ -> pure ()

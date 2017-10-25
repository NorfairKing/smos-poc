{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions
    ( stop
    -- * Ready-made actions
    , insertTreeAbove
    , deleteCurrentHeader
    , moveUp
    , moveDown
    -- * Header actions
    , enterHeader
    , headerInsert
    , headerRemove
    , headerDelete
    , headerLeft
    , headerRight
    , exitHeader
    -- * Helper functions to define your own actions
    , modifyHeaderM
    , modifyHeader
    , modifyCursor
    , modifyMCursor
    , withEntryCursor
    , withHeaderCursor
    , modify
    ) where

import Import

import Control.Monad.State

import Smos.Data

import Smos.Cursor
import Smos.Types

stop :: SmosM a
stop = MkSmosM $ NextT $ pure Stop

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

enterHeader :: SmosM ()
enterHeader =
    modifyCursor $ \cur ->
        case cur of
            AnEntry ec -> AHeader $ entryCursorHeader ec
            _ -> cur

headerInsert :: Char -> SmosM ()
headerInsert c = modifyHeader $ \hc -> headerCursorInsert c hc

headerRemove :: SmosM ()
headerRemove = modifyHeaderM headerCursorRemove

headerDelete :: SmosM ()
headerDelete = modifyHeaderM headerCursorDelete

headerLeft :: SmosM ()
headerLeft = modifyHeaderM headerCursorLeft

headerRight :: SmosM ()
headerRight = modifyHeaderM headerCursorRight

exitHeader :: SmosM ()
exitHeader =
    modifyCursor $ \cur ->
        case cur of
            AHeader hc -> AnEntry $ headerCursorParent hc
            _ -> cur

modifyHeaderM :: (HeaderCursor -> Maybe HeaderCursor) -> SmosM ()
modifyHeaderM func = modifyHeader $ \hc -> fromMaybe hc $ func hc

modifyHeader :: (HeaderCursor -> HeaderCursor) -> SmosM ()
modifyHeader func =
    modifyCursor $ \cur ->
        case cur of
            AHeader h -> AHeader $ func h
            _ -> cur

modifyCursor :: (ACursor -> ACursor) -> SmosM ()
modifyCursor func = modifyMCursor $ \mc -> func <$> mc

modifyMCursor :: (Maybe ACursor -> Maybe ACursor) -> SmosM ()
modifyMCursor func =
    modify $ \ss -> ss {smosStateCursor = func $ smosStateCursor ss}

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

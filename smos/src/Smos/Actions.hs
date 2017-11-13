{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Actions
    ( save
    , stop
    -- * Entry and tree actions
    , insertTreeAbove
    , insertTreeBelow
    , insertTreeChild
    , deleteCurrentHeader
    , moveUp
    , moveDown
    , moveLeft
    , moveRight
    , clockIn
    , clockOut
    , todoStateClear
    , todoStateSet
    -- * Header actions
    , enterHeader
    , headerInsert
    , headerAppend
    , headerRemove
    , headerDelete
    , headerLeft
    , headerRight
    , headerStart
    , headerEnd
    , exitHeader
    -- * Header actions
    , enterContents
    , contentsInsert
    , contentsAppend
    , contentsNewline
    , contentsRemove
    , contentsDelete
    , contentsLeft
    , contentsRight
    , contentsUp
    , contentsDown
    , contentsStart
    , contentsEnd
    , editorOnContents
    , exitContents
    -- * Tags actions
    , enterTag
    , tagInsert
    , tagAppend
    , tagRemove
    , tagDelete
    , tagLeft
    , tagRight
    , tagStart
    , tagEnd
    , tagSelectPrev
    , tagSelectNext
    , editorOnTags
    , exitTag
    -- * Helper functions to define your own actions
    , modifyEntryM
    , modifyEntry
    , modifyHeaderM
    , modifyHeader
    , modifyTodoState
    , modifyCursor
    , modifyMCursor
    , withEntryCursor
    , withHeaderCursor
    , withFullMod
    , withAgendaFilesMod
    , module Control.Monad.Reader
    , module Control.Monad.State
    ) where

import Import

import qualified Data.Text as T
import Data.Time
import Data.Tree

import Control.Monad.Reader
import Control.Monad.State

import Lens.Micro

import Smos.Data

import Cursor.Tree

import Smos.Actions.Editor
import Smos.Cursor
import Smos.Cursor.Entry
import Smos.Types

{-# ANN module ("HLint: ignore Use fromMaybe" :: String) #-}

emptyTree :: Tree Entry
emptyTree = Node {rootLabel = newEntry "", subForest = []}

initEntryCursor :: Maybe ACursor
initEntryCursor =
    let fc = makeForestCursor []
        fc' = forestCursorInsertAtStart emptyTree fc
        mtc' = forestCursorSelectFirst fc'
    in (AnEntry . treeCursorValue) <$> mtc'

save :: SmosM ()
save = do
    file <- gets smosStateFilePath
    mcur <- gets smosStateCursor
    let sf =
            case mcur of
                Nothing -> SmosFile []
                Just cur -> rebuild cur
    writeSmosFile file sf

insertTreeAbove :: SmosM ()
insertTreeAbove =
    modifyMCursor $ \mcur ->
        case mcur of
            Nothing -> initEntryCursor
            Just (AnEntry ec) ->
                let tc = entryCursorParent ec
                    tc' = treeCursorInsertAbove tc emptyTree
                    ec' = treeCursorValue tc'
                in Just $ AnEntry ec'
            _ -> mcur

insertTreeBelow :: SmosM ()
insertTreeBelow =
    modifyMCursor $ \mcur ->
        case mcur of
            Nothing -> initEntryCursor
            Just (AnEntry ec) ->
                let tc = entryCursorParent ec
                    tc' = treeCursorInsertBelow tc emptyTree
                    ec' = treeCursorValue tc'
                in Just $ AnEntry ec'
            _ -> mcur

insertTreeChild :: SmosM ()
insertTreeChild =
    modifyMCursor $ \mcur ->
        case mcur of
            Nothing -> initEntryCursor
            Just (AnEntry ec) ->
                let tc = entryCursorParent ec
                    tc' = treeCursorInsertChildAtStart emptyTree tc
                    ec' = treeCursorValue tc'
                    fc' = treeCursorForest tc'
                    mtc' = forestCursorSelectFirst fc'
                    mec' = treeCursorValue <$> mtc'
                in Just $ maybe (AnEntry ec') AnEntry mec'
            _ -> mcur

deleteCurrentHeader :: SmosM ()
deleteCurrentHeader =
    modifyMCursor $ \mcur ->
        case mcur of
            Just (AnEntry ec) ->
                let tc = entryCursorParent ec
                    eft = treeCursorDeleteCurrent tc
                    mec' =
                        case eft of
                            Left fc ->
                                case forestCursorParent fc of
                                    Nothing -> Nothing
                                    Just tc_ -> Just $ treeCursorValue tc_
                            Right tc_ -> Just $ treeCursorValue tc_
                in AnEntry <$> mec'
            _ -> mcur

moveUp :: SmosM ()
moveUp =
    modifyEntry $ \ec ->
        let tc = entryCursorParent ec
            -- First try to go to the last element recursively of the previous node
            recursivelyDeepestOf t =
                let fc = treeCursorForest t
                in case forestCursorSelectLast fc
                    -- Then try to go to the directly previous node
                         of
                       Nothing -> t
                       Just t_ -> recursivelyDeepestOf t_
            tc' =
                case treeCursorSelectPrev tc of
                    Just t_ -> recursivelyDeepestOf t_
                    Nothing -- Then try to go up
                     ->
                        let fc = treeCursorParent tc
                        in case forestCursorParent fc
                        -- If all else fails, stay where we are
                                 of
                               Nothing -> tc
                               Just tc_ -> tc_
            ec' = treeCursorValue tc'
        in ec'

moveDown :: SmosM ()
moveDown =
    modifyEntry $ \ec ->
        let tc = entryCursorParent ec
            -- First try to go down the current tree's forrest
            goNextViaDown t =
                case forestCursorElems $ treeCursorForest t of
                    [] -> Nothing
                    (tc_:_) -> Just tc_
            goNextViaUp t
                -- Then try to go to the direct next element
             =
                case treeCursorSelectNext t of
                    Just tc_ -> tc_
                    Nothing
                        -- Then try to go recursively upward until there is a next element
                     ->
                        let fc = treeCursorParent t
                        in case forestCursorParent fc of
                               Just tc_ -> goNextViaUp tc_
                               -- If all else fails, stay where we are
                               Nothing -> tc -- CHECKME
            tc' =
                case goNextViaDown tc of
                    Nothing -> goNextViaUp tc
                    Just tc_ -> tc_
            ec' = treeCursorValue tc'
        in ec'

moveLeft :: SmosM ()
moveLeft =
    modifyEntry $ \ec ->
        let tc = entryCursorParent ec
            tc' = fromMaybe tc $ forestCursorParent $ treeCursorParent tc
            ec' = treeCursorValue tc'
        in ec'

moveRight :: SmosM ()
moveRight =
    modifyEntry $ \ec ->
        let tc = entryCursorParent ec
            fc = treeCursorForest tc
            tc' = fromMaybe tc $ forestCursorSelectFirst fc
            ec' = treeCursorValue tc'
        in ec'

clockIn :: SmosM ()
clockIn = do
    now <- liftIO getCurrentTime
    withFullMod $ clockOutMod now
    modifyEntryM $ entryCursorClockIn now

clockOut :: SmosM ()
clockOut = do
    now <- liftIO getCurrentTime
    withFullMod $ clockOutMod now
    withAgendaFilesMod $ const $ clockOutMod now

clockOutMod :: UTCTime -> (SmosFile -> SmosFile)
clockOutMod now = SmosFile . gof . smosFileForest
  where
    gof = map got
    got Node {..} = Node {rootLabel = goe rootLabel, subForest = gof subForest}
    goe e =
        e
        { entryLogbook =
              fromMaybe (entryLogbook e) $ clockOutAt now $ entryLogbook e
        }

enterHeader :: SmosM ()
enterHeader =
    modifyCursor $ \cur ->
        case cur of
            AnEntry ec -> AHeader $ entryCursorHeader ec
            _ -> cur

headerInsert :: Char -> SmosM ()
headerInsert = modifyHeader . headerCursorInsert

headerAppend :: Char -> SmosM ()
headerAppend = modifyHeader . headerCursorAppend

headerRemove :: SmosM ()
headerRemove = modifyHeaderM headerCursorRemove

headerDelete :: SmosM ()
headerDelete = modifyHeaderM headerCursorDelete

headerLeft :: SmosM ()
headerLeft = modifyHeaderM headerCursorLeft

headerRight :: SmosM ()
headerRight = modifyHeaderM headerCursorRight

headerStart :: SmosM ()
headerStart = modifyHeader headerCursorStart

headerEnd :: SmosM ()
headerEnd = modifyHeader headerCursorEnd

exitHeader :: SmosM ()
exitHeader =
    modifyCursor $ \cur ->
        case cur of
            AHeader hc -> AnEntry $ headerCursorParent hc
            _ -> cur

enterContents :: SmosM ()
enterContents =
    modifyCursor $ \cur ->
        case cur of
            AnEntry ec ->
                case entryCursorContents ec of
                    Nothing ->
                        let e' = ec & entryCursorContentsL .~ Just cc
                            cc = emptyContentsCursor e'
                        in AContents cc
                    Just cc -> AContents cc
            _ -> cur

contentsInsert :: Char -> SmosM ()
contentsInsert c = modifyContents $ contentsCursorInsert c

contentsAppend :: Char -> SmosM ()
contentsAppend c = modifyContents $ contentsCursorAppend c

contentsNewline :: SmosM ()
contentsNewline = modifyContents contentsCursorNewline

contentsRemove :: SmosM ()
contentsRemove = modifyContentsM contentsCursorRemove

contentsDelete :: SmosM ()
contentsDelete = modifyContentsM contentsCursorDelete

contentsLeft :: SmosM ()
contentsLeft = modifyContentsM contentsCursorLeft

contentsRight :: SmosM ()
contentsRight = modifyContentsM contentsCursorRight

contentsUp :: SmosM ()
contentsUp = modifyContentsM contentsCursorUp

contentsDown :: SmosM ()
contentsDown = modifyContentsM contentsCursorDown

contentsStart :: SmosM ()
contentsStart = modifyContents contentsCursorStart

contentsEnd :: SmosM ()
contentsEnd = modifyContents contentsCursorEnd

editorOnContents :: String -> SmosM ()
editorOnContents cmd =
    modifyEntryS $ \ec -> do
        er <-
            liftIO $
            startEditorOnContentsAsIs cmd $
            maybe (Contents "") build $ entryCursorContents ec
        case er of
            EditorUnchanged -> pure ec
            EditorChangedTo nc ->
                pure $
                ec & entryCursorContentsL %~ fmap (contentsCursorSetContents nc)
            EditorError {} -> pure ec
            EditorParsingError _ _ -> pure ec

exitContents :: SmosM ()
exitContents =
    modifyCursor $ \cur ->
        case cur of
            AContents hc -> AnEntry $ contentsCursorParent hc
            _ -> cur

todoStateClear :: SmosM ()
todoStateClear = do
    now <- liftIO getCurrentTime
    modifyTodoState $ stateCursorClear now

todoStateSet :: TodoState -> SmosM ()
todoStateSet ts = do
    now <- liftIO getCurrentTime
    modifyTodoState $ stateCursorSetState now ts

enterTag :: SmosM ()
enterTag =
    modifyCursor $ \cur ->
        case cur of
            AnEntry ec ->
                let tsc = entryCursorTags ec
                in case tagsCursorSelectFirst tsc of
                       Nothing ->
                           let tsc' = tagsCursorInsertAtStart "" tsc
                           in case tagsCursorSelectFirst tsc' of
                                  Nothing -> cur -- Should never happen, but fine if it does.
                                  Just tc -> ATag tc
                       Just tc -> ATag tc
            _ -> cur

tagInsert :: Char -> SmosM ()
tagInsert = modifyTag . tagCursorInsert

tagAppend :: Char -> SmosM ()
tagAppend = modifyTag . tagCursorAppend

tagRemove :: SmosM ()
tagRemove = modifyTagM tagCursorRemove

tagDelete :: SmosM ()
tagDelete = modifyTagM tagCursorDelete

tagLeft :: SmosM ()
tagLeft = modifyTagM tagCursorLeft

tagRight :: SmosM ()
tagRight = modifyTagM tagCursorRight

tagStart :: SmosM ()
tagStart = modifyTag tagCursorStart

tagEnd :: SmosM ()
tagEnd = modifyTag tagCursorEnd

tagSelectPrev :: SmosM ()
tagSelectPrev = modifyTagM tagCursorSelectPrev

tagSelectNext :: SmosM ()
tagSelectNext =
    modifyTagM $ \tc ->
        case tagCursorSelectNext tc of
            Just tc_ -> Just tc_
            Nothing ->
                let t = tagText $ build tc
                in if T.null t
                       then Nothing
                       else let tsc = tagCursorParent tc
                                tsc' =
                                    tagsCursorInsertAt
                                        (tagCursorIndex tc + 1)
                                        ""
                                        tsc
                            in tagsCursorTags tsc' `atMay`
                               (tagCursorIndex tc + 1)

editorOnTags :: String -> SmosM ()
editorOnTags cmd =
    modifyEntryS $ \ec -> do
        er <- liftIO $ startEditorOnTagsAsIs cmd $ build $ entryCursorTags ec
        case er of
            EditorUnchanged -> pure ec
            EditorChangedTo tgs ->
                pure $ ec & entryCursorTagsL %~ tagsCursorSetTags tgs
            EditorError {} -> pure ec
            EditorParsingError _ _ -> pure ec

exitTag :: SmosM ()
exitTag =
    modifyCursor $ \cur ->
        case cur of
            ATag tc -> AnEntry $ tagsCursorParent $ tagCursorParent tc
            _ -> cur

modifyEntryM :: (EntryCursor -> Maybe EntryCursor) -> SmosM ()
modifyEntryM func = modifyEntry $ \hc -> fromMaybe hc $ func hc

modifyEntry :: (EntryCursor -> EntryCursor) -> SmosM ()
modifyEntry func = modifyEntryS $ pure . func

modifyEntryS :: (EntryCursor -> SmosM EntryCursor) -> SmosM ()
modifyEntryS func =
    modifyCursorS $ \cur ->
        case cur of
            AnEntry h -> AnEntry <$> func h
            _ -> pure cur

modifyHeaderM :: (HeaderCursor -> Maybe HeaderCursor) -> SmosM ()
modifyHeaderM func = modifyHeader $ \hc -> fromMaybe hc $ func hc

modifyHeader :: (HeaderCursor -> HeaderCursor) -> SmosM ()
modifyHeader func =
    modifyCursor $ \cur ->
        case cur of
            AHeader h -> AHeader $ func h
            _ -> cur

modifyContentsM :: (ContentsCursor -> Maybe ContentsCursor) -> SmosM ()
modifyContentsM func = modifyContents $ \cc -> fromMaybe cc $ func cc

modifyContents :: (ContentsCursor -> ContentsCursor) -> SmosM ()
modifyContents func = modifyContentsS $ pure . func

modifyContentsS :: (ContentsCursor -> SmosM ContentsCursor) -> SmosM ()
modifyContentsS func =
    modifyCursorS $ \cur ->
        case cur of
            AContents cc -> AContents <$> func cc
            _ -> pure cur

modifyTodoState :: (StateCursor -> StateCursor) -> SmosM ()
modifyTodoState func =
    modifyCursor $ \cur ->
        case cur of
            AnEntry ec ->
                AnEntry $ stateCursorParent $ func $ entryCursorState ec
            _ -> cur

modifyTagM :: (TagCursor -> Maybe TagCursor) -> SmosM ()
modifyTagM func = modifyTag $ \tc -> fromMaybe tc $ func tc

modifyTag :: (TagCursor -> TagCursor) -> SmosM ()
modifyTag func =
    modifyCursor $ \cur ->
        case cur of
            ATag t -> ATag $ func t
            _ -> cur

modifyCursor :: (ACursor -> ACursor) -> SmosM ()
modifyCursor func = modifyMCursor $ \mc -> func <$> mc

modifyCursorS :: (ACursor -> SmosM ACursor) -> SmosM ()
modifyCursorS func =
    modifyMCursorS $ \mc ->
        case mc of
            Nothing -> pure Nothing
            Just c -> Just <$> func c

modifyMCursor :: (Maybe ACursor -> Maybe ACursor) -> SmosM ()
modifyMCursor func = modifyMCursorS $ pure . func

modifyMCursorS :: (Maybe ACursor -> SmosM (Maybe ACursor)) -> SmosM ()
modifyMCursorS func = do
    ss <- get
    let msc = smosStateCursor ss
    msc' <- func msc
    let ss' = ss {smosStateCursor = msc'}
    put ss'

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

withFullMod :: (SmosFile -> SmosFile) -> SmosM ()
withFullMod func =
    modify $ \ss ->
        case smosStateCursor ss of
            Nothing -> ss
            Just cur ->
                let sf = rebuild cur
                    sel = selection $ selectAnyCursor cur
                    sf' = func sf
                    cur' = selectACursor $ reselectCursor sel sf'
                in ss {smosStateCursor = cur'}

withAgendaFilesMod :: (Path Abs File -> SmosFile -> SmosFile) -> SmosM ()
withAgendaFilesMod func = do
    getAgendaFiles <- asks configAgendaFiles
    files <- liftIO getAgendaFiles
    forM_ files $ \file -> do
        errOrSF <- readSmosFile file
        let result =
                case errOrSF of
                    Nothing -> Nothing
                    Just (Left _) -> Nothing
                    Just (Right sf) -> Just $ func file sf
        case result of
            Nothing -> pure ()
            Just sf' -> writeSmosFile file sf'

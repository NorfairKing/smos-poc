{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
    , swapUp
    , swapDown
    , swapLeft
    , swapRight
    , toggleShowDebug
    -- * Clock action
    , clockIn
    , clockOut
    -- * Logbook
    , editorOnLogbook
    -- * Properties
    , editorOnProperties
    -- * Timestamps
    , editorOnTimestamps
    -- * Todo state actions
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
    , tagToggle
    , tagSet
    , tagUnset
    , editorOnTags
    , exitTag
    -- ** Single Tag Actions
    , tagInsert
    , tagAppend
    , tagRemove
    , tagDelete
    , tagLeft
    , tagRight
    , tagStart
    , tagEnd
    -- ** Tags actions
    , tagsSelectPrev
    , tagsSelectNext
    , tagsSelectFirst
    , tagsSelectLast
    -- * Timestamps actions
    , enterTimestamps
    , timestampSwitch
    , exitTimestamps
    -- ** Single Timestamp name actions
    , timestampNameInsert
    , timestampNameAppend
    , timestampNameLeft
    , timestampNameRight
    , timestampNameStart
    , timestampNameEnd
    -- * Helper functions to define your own actions
    , modifyEntryM
    , modifyEntry
    , modifyHeaderM
    , modifyHeader
    , modifyTagsM
    , modifyTags
    , modifyTagM
    , modifyTag
    , modifyTodoState
    , modifyCursor
    , modifyMCursor
    , modifyCursorS
    , modifyMCursorS
    , modifyFileCursor
    , modifyMFileCursor
    , modifyFileCursorS
    , modifyMFileCursorS
    , withEntryCursor
    , withHeaderCursor
    , withFullMod
    , withAgendaFilesMod
    , module Control.Monad.Reader
    , module Control.Monad.State
    ) where

import Import

import Data.Time
import Data.Tree

import Control.Monad.Reader
import Control.Monad.State

import Lens.Micro

import Smos.Data

import Cursor.Class
import Cursor.Map
import Cursor.Tree

import Smos.Actions.Editor
import Smos.Cursor
import Smos.Types

{-# ANN module ("HLint: ignore Use fromMaybe" :: String) #-}

emptyTree :: Tree Entry
emptyTree = Node {rootLabel = newEntry "", subForest = []}

initEntryCursor :: Maybe ACursor
initEntryCursor =
    let fc = makeForestCursor' []
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
                Just cur -> source $ rebuild cur
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
moveLeft = modifyTreeM $ forestCursorParent . treeCursorParent

moveRight :: SmosM ()
moveRight = modifyTreeM $ forestCursorSelectFirst . treeCursorForest

swapUp :: SmosM ()
swapUp = modifyTreeM treeCursorMoveUp

swapDown :: SmosM ()
swapDown = modifyTreeM treeCursorMoveDown

swapLeft :: SmosM ()
swapLeft = modifyTreeM treeCursorMoveLeft

swapRight :: SmosM ()
swapRight = modifyTreeM treeCursorMoveRight

toggleShowDebug :: SmosM ()
toggleShowDebug =
    modify $ \ss -> ss {smosStateShowDebug = not $ smosStateShowDebug ss}

modifyTreeM ::
       (TreeCursor EntryCursor -> Maybe (TreeCursor EntryCursor)) -> SmosM ()
modifyTreeM func = modifyTree $ \tc -> fromMaybe tc $ func tc

modifyTree :: (TreeCursor EntryCursor -> TreeCursor EntryCursor) -> SmosM ()
modifyTree func = modifyEntry $ treeCursorValue . func . entryCursorParent

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

editorOnLogbook :: String -> SmosM ()
editorOnLogbook = editorOn entryCursorLogbookL startEditorOnLogbookAsIs

editorOnProperties :: String -> SmosM ()
editorOnProperties = editorOn entryCursorPropertiesL startEditorOnPropertiesAsIs

editorOnTimestamps :: String -> SmosM ()
editorOnTimestamps =
    editorOn entryCursorTimestampsMapL startEditorOnTimestampsAsIs

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
editorOnContents =
    editorOn
        (entryCursorContentsML . non (Contents ""))
        startEditorOnContentsAsIs

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
                case entryCursorTags ec of
                    Nothing ->
                        let ec' = ec & entryCursorTagsL .~ Just tsc
                            tsc = newTagsCursor ec'
                        in ATags tsc
                    Just tc -> ATags tc
            _ -> cur

tagToggle :: Tag -> SmosM ()
tagToggle t =
    modifyEntry $ \ec ->
        ec & entryCursorTagsL %~
        (\mtc ->
             case mtc of
                 Nothing -> Just $ makeNewTagsCursor ec t
                 Just tgsc -> tagsCursorToggle t tgsc)

tagSet :: Tag -> SmosM ()
tagSet t =
    modifyEntry $ \ec ->
        ec & entryCursorTagsL %~
        (\mtc ->
             Just $
             case mtc of
                 Nothing -> makeNewTagsCursor ec t
                 Just tgsc -> tagsCursorSet t tgsc)

tagUnset :: Tag -> SmosM ()
tagUnset t =
    modifyEntry $
    entryCursorTagsL %~
    (\mtc ->
         case mtc of
             Nothing -> Nothing
             Just tgsc -> tagsCursorUnset t tgsc)

tagInsert :: Char -> SmosM ()
tagInsert = modifyTag . tagCursorInsert

tagAppend :: Char -> SmosM ()
tagAppend = modifyTag . tagCursorAppend

tagRemove :: SmosM ()
tagRemove = modifyTagNOUOD tagsCursorRemove

tagDelete :: SmosM ()
tagDelete = modifyTagNOUOD tagsCursorDelete

tagLeft :: SmosM ()
tagLeft = modifyTagM tagCursorLeft

tagRight :: SmosM ()
tagRight = modifyTagM tagCursorRight

tagStart :: SmosM ()
tagStart = modifyTag tagCursorStart

tagEnd :: SmosM ()
tagEnd = modifyTag tagCursorEnd

tagsSelectPrev :: SmosM ()
tagsSelectPrev =
    modifyTags $ \tc ->
        case tagsCursorSelectPrev tc of
            Just tc_ -> tc_
            Nothing -> tagsCursorInsertAndSelect "" tc

tagsSelectNext :: SmosM ()
tagsSelectNext =
    modifyTags $ \tc ->
        case tagsCursorSelectNext tc of
            Just tc_ -> tc_
            Nothing -> tagsCursorAppendAndSelect "" tc

tagsSelectFirst :: SmosM ()
tagsSelectFirst = modifyTags tagsCursorSelectFirst

tagsSelectLast :: SmosM ()
tagsSelectLast = modifyTags tagsCursorSelectLast

editorOnTags :: String -> SmosM ()
editorOnTags = editorOn entryCursorTagsListL startEditorOnTagsAsIs

exitTag :: SmosM ()
exitTag =
    modifyCursor $ \cur ->
        case cur of
            ATags tc -> AnEntry $ tagsCursorParent tc
            _ -> cur

enterTimestamps :: SmosM ()
enterTimestamps =
    modifyCursorS $ \cur ->
        case cur of
            AnEntry ec ->
                case entryCursorTimestamps ec of
                    Nothing -> do
                        now <- liftIO getCurrentTime
                        let ec' = ec & entryCursorTimestampsL .~ Just tsc
                            tsc = newTimestampsCursor ec' now
                        pure $ ATimestamps tsc
                    Just tc -> pure $ ATimestamps tc
            _ -> pure cur

timestampSwitch :: SmosM ()
timestampSwitch =
    modifyTimestamp $ \kvc ->
        case kvc of
            KVK kc -> KVV $ keyCursorSelectValue kc
            KVV vc -> KVK $ valueCursorSelectKey vc

exitTimestamps :: SmosM ()
exitTimestamps =
    modifyCursor $ \cur ->
        case cur of
            ATimestamps tc -> AnEntry $ timestampsCursorParent tc
            _ -> cur

timestampNameInsert :: Char -> SmosM ()
timestampNameInsert = modifyTimestampName . timestampNameCursorInsert

timestampNameAppend :: Char -> SmosM ()
timestampNameAppend = modifyTimestampName . timestampNameCursorAppend

timestampNameLeft :: SmosM ()
timestampNameLeft = modifyTimestampNameM timestampNameCursorLeft

timestampNameRight :: SmosM ()
timestampNameRight = modifyTimestampNameM timestampNameCursorRight

timestampNameStart :: SmosM ()
timestampNameStart = modifyTimestampName timestampNameCursorStart

timestampNameEnd :: SmosM ()
timestampNameEnd = modifyTimestampName timestampNameCursorEnd

modifyTimestampNameM ::
       (TimestampNameCursor -> Maybe TimestampNameCursor) -> SmosM ()
modifyTimestampNameM func = modifyTimestampName $ \tc -> fromMaybe tc $ func tc

modifyTimestampName :: (TimestampNameCursor -> TimestampNameCursor) -> SmosM ()
modifyTimestampName func =
    modifyTimestamp $ \kvc -> kvc & keyValueCursorKeyL %~ func

modifyTimestamp ::
       (KeyValueCursor TimestampNameCursor TimestampCursor -> KeyValueCursor TimestampNameCursor TimestampCursor)
    -> SmosM ()
modifyTimestamp func = modifyTimestampS $ pure . func

modifyTimestampS ::
       (KeyValueCursor TimestampNameCursor TimestampCursor -> SmosM (KeyValueCursor TimestampNameCursor TimestampCursor))
    -> SmosM ()
modifyTimestampS func =
    modifyTimestampsS $ \tsc -> do
        r <- func $ tsc ^. timestampsCursorSelectedL
        pure $ tsc & timestampsCursorSelectedL .~ r

modifyTimestampsS :: (TimestampsCursor -> SmosM TimestampsCursor) -> SmosM ()
modifyTimestampsS func =
    modifyCursorS $ \cur ->
        case cur of
            ATimestamps tc -> ATimestamps <$> func tc
            _ -> pure cur

editorOn ::
       Lens' EntryCursor a
    -> (String -> a -> IO (EditorResult a))
    -> String
    -> SmosM ()
editorOn l editFunc cmd =
    modifyEntryS $ \ec -> do
        er <- liftIO $ editFunc cmd $ ec ^. l
        case er of
            EditorUnchanged -> pure ec
            EditorChangedTo nc -> pure $ ec & l .~ nc
            EditorError {} -> pure ec
            EditorParsingError _ _ -> pure ec

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

modifyTagsM :: (TagsCursor -> Maybe TagsCursor) -> SmosM ()
modifyTagsM func = modifyTags $ \tc -> fromMaybe tc $ func tc

modifyTags :: (TagsCursor -> TagsCursor) -> SmosM ()
modifyTags func =
    modifyCursor $ \cur ->
        case cur of
            ATags ts -> ATags $ func ts
            _ -> cur

modifyTagNOUOD :: (TagsCursor -> NOUOD TagsCursor) -> SmosM ()
modifyTagNOUOD func =
    modifyCursor $ \cur ->
        case cur of
            ATags tgs ->
                case func tgs of
                    New t -> ATags t
                    Unchanged -> cur
                    Deleted ->
                        AnEntry $
                        tagsCursorParent tgs & entryCursorTagsL .~ Nothing
            _ -> cur

modifyTagM :: (TagCursor -> Maybe TagCursor) -> SmosM ()
modifyTagM func = modifyTag $ \tc -> fromMaybe tc $ func tc

modifyTag :: (TagCursor -> TagCursor) -> SmosM ()
modifyTag func =
    modifyCursor $ \cur ->
        case cur of
            ATags t -> ATags $ t & tagsCursorSelectedL %~ func
            _ -> cur

modifyCursor :: (ACursor -> ACursor) -> SmosM ()
modifyCursor func = modifyMCursor $ fmap func

modifyCursorS :: (ACursor -> SmosM ACursor) -> SmosM ()
modifyCursorS func =
    modifyMCursorS $ \mac ->
        case mac of
            Nothing -> pure Nothing
            Just ac -> do
                r <- func ac
                pure $ Just r

modifyMCursor :: (Maybe ACursor -> Maybe ACursor) -> SmosM ()
modifyMCursor func = modifyMCursorS $ pure . func

modifyMCursorS :: (Maybe ACursor -> SmosM (Maybe ACursor)) -> SmosM ()
modifyMCursorS func =
    modifyMFileCursorS $ \msfc -> do
        mr <- func $ fileCursorA <$> msfc
        pure $ SmosFileCursor <$> mr

modifyFileCursor :: (SmosFileCursor -> SmosFileCursor) -> SmosM ()
modifyFileCursor func = modifyMFileCursor $ \mc -> func <$> mc

modifyFileCursorS :: (SmosFileCursor -> SmosM SmosFileCursor) -> SmosM ()
modifyFileCursorS func =
    modifyMFileCursorS $ \mc ->
        case mc of
            Nothing -> pure Nothing
            Just c -> Just <$> func c

modifyMFileCursor :: (Maybe SmosFileCursor -> Maybe SmosFileCursor) -> SmosM ()
modifyMFileCursor func = modifyMFileCursorS $ pure . func

modifyMFileCursorS ::
       (Maybe SmosFileCursor -> SmosM (Maybe SmosFileCursor)) -> SmosM ()
modifyMFileCursorS func = do
    ss <- get
    let msc = smosStateCursor ss
    msc' <- func msc
    let ss' = ss {smosStateCursor = msc'}
    put ss'

withEntryCursor :: (EntryCursor -> SmosM ()) -> SmosM ()
withEntryCursor func =
    withACursor $ \acur ->
        case acur of
            AnEntry ec -> func ec
            _ -> pure ()

withHeaderCursor :: (HeaderCursor -> SmosM ()) -> SmosM ()
withHeaderCursor func =
    withACursor $ \acur ->
        case acur of
            AHeader fc -> func fc
            _ -> pure ()

withACursor :: (ACursor -> SmosM ()) -> SmosM ()
withACursor func = do
    ss <- get
    case smosStateCursor ss of
        Just acur -> func $ fileCursorA acur
        _ -> pure ()

withFullMod :: (SmosFile -> SmosFile) -> SmosM ()
withFullMod func =
    modify $ \ss ->
        case smosStateCursor ss of
            Nothing -> ss
            Just cur ->
                let sf = rebuild cur
                    sel = selection $ selectAnyCursor $ fileCursorA cur
                    sf' = func $ source sf
                    cur' = selectACursor $ reselectCursor sel sf'
                in ss
                   { smosStateCursor =
                         case cur' of
                             Nothing -> Nothing
                             Just ac -> Just $ cur {fileCursorA = ac}
                   }

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

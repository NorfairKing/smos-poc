{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos where

import Import

import qualified Data.HashMap.Lazy as HM
import Data.List

import System.Environment
import System.Exit

import Brick.AttrMap as B
import Brick.Main as B
import Brick.Types as B
import Brick.Util as B (fg)
import Brick.Widgets.Core as B
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes

import Smos.Data

import Smos.Cursor
import Smos.Types

smos :: IO ()
smos = do
    args <- getArgs
    case args of
        [] -> die "Expected argument file."
        (f:_) -> do
            fp <- resolveFile' f
            errOrSF <- readSmosFile fp
            startF <-
                case errOrSF of
                    Nothing -> pure Nothing
                    Just (Left err) -> die err
                    Just (Right sf) -> pure $ Just sf
            let s = initState $ fromMaybe emptySmosFile startF
            s' <- defaultMain smosApp s
            let sf' = rebuildSmosFile s'
            when (startF /= Just sf') $ writeSmosFile fp sf'

initState :: SmosFile -> SmosState
initState sf = SmosState {smosStateCursor = makeACursor sf}

rebuildSmosFile :: SmosState -> SmosFile
rebuildSmosFile SmosState {..} =
    SmosFile {smosFileForest = rebuild smosStateCursor}

smosApp :: App SmosState e ResourceName
smosApp =
    App
    { appDraw = smosDraw
    , appChooseCursor = smosChooseCursor
    , appHandleEvent = smosHandleEvent
    , appStartEvent = smosStartEvent
    , appAttrMap = smosAttrMap
    }

smosDraw :: SmosState -> [Widget ResourceName]
smosDraw SmosState {..} = [renderForest smosStateCursor]
  where
    renderForest cur = smosForest (Just $ makeASelection cur) (rebuild cur)
    smosForest msel SmosForest {..} =
        padLeft (Pad 2) . B.vBox $
        flip map (zip [0 ..] smosTrees) $ \(ix, st) ->
            smosTree (drillSel msel ix) st
    smosTree msel SmosTree {..} =
        smosEntry msel treeEntry <=> smosForest msel treeForest
    smosEntry msel Entry {..} =
        withSel msel $
        B.vBox
            [ B.hBox $
              intersperse
                  (B.txt " ")
                  [ B.txt ">"
                  , mayW entryState $ B.txt . todoStateText
                  , B.txt $ headerText entryHeader
                  , B.hBox $
                    intersperse (B.txt ":") $ map (B.txt . tagText) entryTags
                  ]
            , mayW entryContents $ B.txt . contentsText
            , B.vBox $
              flip map (HM.toList entryTimestamps) $ \(k, ts) ->
                  B.hBox
                      [ B.txt $ timestampNameText k
                      , B.txt ": "
                      , smosTimestamp ts
                      ]
            , smosLogbook entryLogbook
            ]
    smosLogbook LogEnd = B.emptyWidget
    smosLogbook (LogEntry b e l) =
        B.hBox [smosTimestamp b, smosTimestamp e] <=> smosLogbook l
    smosLogbook (LogOpenEntry b l) =
        B.hBox [smosTimestamp b, B.txt "present"] <=> smosLogbook l
    smosTimestamp = B.str . show
    mayW mw func = maybe emptyWidget func mw
    drillSel msel ix =
        case msel of
            Nothing -> Nothing
            Just [] -> Nothing
            Just (x:xs) ->
                if x == ix
                    then Just xs
                    else Nothing
    withSel :: Maybe [Int] -> Widget n -> Widget n
    withSel msel =
        case msel of
            Nothing -> id
            Just [] -> withAttr selectedAttr
            Just _ -> id

selectedAttr :: AttrName
selectedAttr = "selected"

smosChooseCursor :: s -> [CursorLocation n] -> Maybe (CursorLocation n)
smosChooseCursor = neverShowCursor

smosHandleEvent :: SmosState -> BrickEvent n e -> EventM n (Next SmosState)
smosHandleEvent ss@SmosState {..} (VtyEvent e) =
    case e of
        V.EvKey (V.KChar 'h') [] -> do
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
            case smosStateCursor of
                AForest fc -> do
                    let fc' = forestCursorInsertAtStart fc newE
                        ss' = ss {smosStateCursor = AForest fc'}
                    B.continue ss'
                ATree tc -> do
                    let tc' = treeCursorInsertAbove tc newE
                        ss' = ss {smosStateCursor = ATree tc'}
                    B.continue ss'
        V.EvKey V.KUp [] ->
            case smosStateCursor of
                AForest fc -> do
                    let mfc' = forestCursorSelectLast fc
                        ss' =
                            ss
                            { smosStateCursor =
                                  case mfc' of
                                      Nothing -> AForest fc
                                      Just tc -> ATree tc
                            }
                    B.continue ss'
                ATree tc -> do
                    let mtc' = treeCursorSelectPrev tc
                        ss' =
                            ss
                            {smosStateCursor = maybe smosStateCursor ATree mtc'}
                    B.continue ss'
        V.EvKey V.KDown [] ->
            case smosStateCursor of
                AForest fc -> do
                    let mfc' = forestCursorSelectFirst fc
                        ss' =
                            ss
                            { smosStateCursor =
                                  case mfc' of
                                      Nothing -> AForest fc
                                      Just tc -> ATree tc
                            }
                    B.continue ss'
                ATree tc -> do
                    let mtc' = treeCursorSelectNext tc
                        ss' =
                            ss
                            {smosStateCursor = maybe smosStateCursor ATree mtc'}
                    B.continue ss'
        V.EvKey (V.KChar 'q') [] -> B.halt ss
        V.EvKey V.KEsc [] -> B.halt ss
        _ -> B.continue ss
smosHandleEvent ss _ = B.continue ss

smosStartEvent :: s -> EventM n s
smosStartEvent = pure

smosAttrMap :: s -> AttrMap
smosAttrMap _ = attrMap defAttr [(selectedAttr, fg V.brightWhite)]

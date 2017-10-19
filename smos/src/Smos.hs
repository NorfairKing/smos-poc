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
import Brick.Widgets.Core as B
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes

import Smos.Data

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
            let s = SmosState {smosFile = fromMaybe emptySmosFile startF}
            s' <- defaultMain smosApp s
            when (startF /= Just (smosFile s')) $ writeSmosFile fp $ smosFile s'

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
smosDraw SmosState {..} = [smosForest $ smosFileForest smosFile]
  where
    smosForest = padLeft (Pad 2) . B.vBox . map smosTree . smosTrees
    smosTree SmosTree {..} = smosEntry treeEntry <=> smosForest treeForest
    smosEntry Entry {..} =
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

smosChooseCursor :: s -> [CursorLocation n] -> Maybe (CursorLocation n)
smosChooseCursor = neverShowCursor

smosHandleEvent :: SmosState -> BrickEvent n e -> EventM n (Next SmosState)
smosHandleEvent ss@SmosState {..} (VtyEvent e) =
    case e of
        V.EvKey (V.KChar 'h') [] -> do
            let sff = smosFileForest smosFile
                e' =
                    Entry
                    { entryHeader = "new"
                    , entryContents = Nothing
                    , entryTimestamps = HM.empty
                    , entryState = Nothing
                    , entryTags = []
                    , entryLogbook = LogEnd
                    }
                st' =
                    SmosTree
                    {treeEntry = e', treeForest = SmosForest {smosTrees = []}}
                sff' = SmosForest {smosTrees = st' : smosTrees sff}
                sf' = SmosFile sff'
                ss' = ss {smosFile = sf'}
            B.continue ss'
        V.EvKey (V.KChar 'q') [] -> B.halt ss
        V.EvKey V.KEsc [] -> B.halt ss
        _ -> B.continue ss
smosHandleEvent ss _ = B.continue ss

smosStartEvent :: s -> EventM n s
smosStartEvent = pure

smosAttrMap :: s -> AttrMap
smosAttrMap _ = attrMap defAttr []

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos
    ( SmosConfig(..)
    , SmosState(..)
    , smos
    ) where

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

smos :: Ord e => SmosConfig e -> IO ()
smos sc@SmosConfig {..} = do
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
            s' <- defaultMain (mkSmosApp sc) s
            let sf' = rebuildSmosFile s'
            when (startF /= Just sf') $ writeSmosFile fp sf'

initState :: SmosFile -> SmosState
initState sf = SmosState {smosStateCursor = makeACursor sf}

rebuildSmosFile :: SmosState -> SmosFile
rebuildSmosFile SmosState {..} =
    SmosFile
    {smosFileForest = fromMaybe (SmosForest []) $ rebuild <$> smosStateCursor}

mkSmosApp :: Ord e => SmosConfig e -> App SmosState e ResourceName
mkSmosApp SmosConfig {..} =
    App
    { appDraw = smosDraw
    , appChooseCursor = smosChooseCursor
    , appHandleEvent = smosHandleEvent keyMap
    , appStartEvent = smosStartEvent
    , appAttrMap = smosAttrMap
    }

smosDraw :: SmosState -> [Widget ResourceName]
smosDraw SmosState {..} =
    [fromMaybe (str "NO CONTENT") $ renderForest <$> smosStateCursor]
  where
    renderForest cur = smosForest (Just $ makeASelection cur) (rebuild cur)
    smosForest msel SmosForest {..} =
        padLeft (Pad 2) . B.vBox $
        flip map (zip [0 ..] smosTrees) $ \(ix, st) ->
            smosTree (drillSel msel ix) st
    smosTree msel SmosTree {..} =
        smosEntry (drillSel msel 0) treeEntry <=>
        smosForest (drillSel msel 1) treeForest
    smosEntry msel Entry {..} =
        withSel msel $
        B.vBox
            [ B.hBox $
              intersperse
                  (B.txt " ")
                  [ B.txt ">"
                  , mayW entryState $ B.txt . todoStateText
                  , smosHeader (drillSel msel 0) entryHeader
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
    smosHeader msel Header {..} = withTextSel msel headerText
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
    withTextSel :: Maybe [Int] -> Text -> Widget ResourceName
    withTextSel msel t =
        case msel of
            Nothing -> B.txt t
            Just [ix_] ->
                withAttr selectedAttr $
                B.showCursor headerCursorName (B.Location (ix_, 0)) $ B.txt t
            Just _ -> B.txt t

selectedAttr :: AttrName
selectedAttr = "selected"

headerCursorName :: ResourceName
headerCursorName = "header-cursor"

smosChooseCursor ::
       s -> [CursorLocation ResourceName] -> Maybe (CursorLocation ResourceName)
smosChooseCursor _ = showCursorNamed headerCursorName

smosHandleEvent ::
       Ord e
    => Keymap e
    -> SmosState
    -> BrickEvent ResourceName e
    -> EventM ResourceName (Next SmosState)
smosHandleEvent km s e = do
    (mkHalt, s') <- runSmosM s $ unKeymap km s e
    case mkHalt of
        Stop -> B.halt s'
        Continue () -> B.continue s'

smosStartEvent :: s -> EventM n s
smosStartEvent = pure

smosAttrMap :: s -> AttrMap
smosAttrMap _ = attrMap defAttr [(selectedAttr, fg V.brightWhite)]

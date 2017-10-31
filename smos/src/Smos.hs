{-# LANGUAGE RecordWildCards #-}

module Smos
    ( SmosConfig(..)
    , SmosState(..)
    , smos
    ) where

import Import

import System.Exit

import Brick.Main as B
import Brick.Types as B

import Smos.Data

import Smos.Cursor
import Smos.Draw
import Smos.OptParse
import Smos.Style
import Smos.Types

smos :: SmosConfig -> IO ()
smos sc@SmosConfig {..} = do
    Instructions p Settings <- getInstructions sc
    errOrSF <- readSmosFile p
    startF <-
        case errOrSF of
            Nothing -> pure Nothing
            Just (Left err) -> die err
            Just (Right sf) -> pure $ Just sf
    let s = initState p $ fromMaybe emptySmosFile startF
    s' <- defaultMain (mkSmosApp sc) s
    let sf' = rebuildSmosFile s'
    when (startF /= Just sf') $ writeSmosFile p sf'

initState :: Path Abs File -> SmosFile -> SmosState
initState p sf =
    SmosState
    {smosStateFilePath = p, smosStateCursor = selectACursor $ makeAnyCursor sf}

rebuildSmosFile :: SmosState -> SmosFile
rebuildSmosFile SmosState {..} =
    SmosFile
    {smosFileForest = fromMaybe (SmosForest []) $ rebuild <$> smosStateCursor}

mkSmosApp :: SmosConfig -> App SmosState () ResourceName
mkSmosApp sc@SmosConfig {..} =
    App
    { appDraw = smosDraw
    , appChooseCursor = smosChooseCursor
    , appHandleEvent = smosHandleEvent sc
    , appStartEvent = smosStartEvent
    , appAttrMap = smosAttrMap configAttrMap
    }

smosChooseCursor ::
       s -> [CursorLocation ResourceName] -> Maybe (CursorLocation ResourceName)
smosChooseCursor _ = showCursorNamed textCursorName

smosHandleEvent ::
       SmosConfig
    -> SmosState
    -> BrickEvent ResourceName ()
    -> EventM ResourceName (Next SmosState)
smosHandleEvent cf s e = do
    (mkHalt, s') <- runSmosM cf s $ unKeymap (configKeyMap cf) s e
    case mkHalt of
        Stop -> B.halt s'
        Continue () -> B.continue s'

smosStartEvent :: s -> EventM n s
smosStartEvent = pure

smosAttrMap :: a -> a
smosAttrMap = id

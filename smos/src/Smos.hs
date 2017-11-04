{-# LANGUAGE RecordWildCards #-}

module Smos
    ( SmosConfig(..)
    , SmosState(..)
    , smos
    ) where

import Import

import System.Exit

import Brick.Main as B

import Smos.Data

import Smos.App
import Smos.Cursor
import Smos.OptParse
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
    fromMaybe (SmosFile $ SmosForest []) $ rebuild <$> smosStateCursor

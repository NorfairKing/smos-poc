{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos
    ( SmosConfig(..)
    , SmosState(..)
    , smos
    ) where

import Import

import System.Environment
import System.Exit

import Brick.Main as B
import Brick.Types as B

import Smos.Data

import Smos.Cursor
import Smos.Draw
import Smos.Style
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
    , appHandleEvent = smosHandleEvent configKeyMap
    , appStartEvent = smosStartEvent
    , appAttrMap = smosAttrMap configAttrMap
    }

smosChooseCursor ::
       s -> [CursorLocation ResourceName] -> Maybe (CursorLocation ResourceName)
smosChooseCursor _ = showCursorNamed textCursorName

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

smosAttrMap :: a -> a
smosAttrMap = id

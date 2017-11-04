{-# LANGUAGE RecordWildCards #-}

module Smos.App
    ( mkSmosApp
    ) where

import Import


import Brick.Main as B
import Brick.Types as B


import Smos.Draw
import Smos.Style
import Smos.Types

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

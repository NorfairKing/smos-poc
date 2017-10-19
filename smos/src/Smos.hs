module Smos where

import Import

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Graphics.Vty.Attributes

import Smos.Types

smos :: IO ()
smos = defaultMain smosApp ()

smosApp :: App s e ResourceName
smosApp =
    App
    { appDraw = smosDraw
    , appChooseCursor = smosChooseCursor
    , appHandleEvent = smosHandleEvent
    , appStartEvent = smosStartEvent
    , appAttrMap = smosAttrMap
    }

smosDraw :: s -> [Widget n]
smosDraw _ = [str "Smos!"]

smosChooseCursor :: s -> [CursorLocation n] -> Maybe (CursorLocation n)
smosChooseCursor = neverShowCursor

smosHandleEvent :: s -> BrickEvent n e -> EventM n (Next s)
smosHandleEvent = resizeOrQuit

smosStartEvent :: s -> EventM n s
smosStartEvent = pure

smosAttrMap :: s -> AttrMap
smosAttrMap _ = attrMap defAttr []

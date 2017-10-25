module Smos.Default where

import Import

import qualified Data.Map as M

import qualified Brick.Types as B
import qualified Graphics.Vty as V

import Smos
import Smos.Actions

defaultSmos :: IO ()
defaultSmos = smos (defaultConfig :: SmosConfig ())

defaultConfig :: Ord e => SmosConfig e
defaultConfig =
    SmosConfig
    { keyMap =
          M.fromList
              [ (B.VtyEvent (V.EvKey (V.KChar 'h') []), insertTreeAbove)
              , (B.VtyEvent (V.EvKey (V.KChar 'd') []), deleteCurrentHeader)
              , (B.VtyEvent (V.EvKey (V.KChar 'j') []), moveDown)
              , (B.VtyEvent (V.EvKey (V.KChar 'k') []), moveUp)
              , (B.VtyEvent (V.EvKey V.KDown []), moveDown)
              , (B.VtyEvent (V.EvKey V.KUp []), moveUp)
              , (B.VtyEvent (V.EvKey (V.KChar 'q') []), halt)
              , (B.VtyEvent (V.EvKey V.KEsc []), halt)
              ]
    }

module Smos.Default where

import Import

import Smos
import Smos.Actions
import Smos.Keys

defaultSmos :: IO ()
defaultSmos = smos (defaultConfig :: SmosConfig ())

defaultConfig :: Ord e => SmosConfig e
defaultConfig =
    SmosConfig
    { keyMap =
          mconcat
              [ matchChar 'h' insertTreeAbove
              , matchChar 'd' deleteCurrentHeader
              , matchChar 'j' moveDown
              , matchChar 'k' moveUp
              , matchKey KDown moveDown
              , matchKey KUp moveUp
              , matchChar 'q' stop
              , matchKey KEsc stop
              ]
    }

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
              [ inEntry $
                mconcat
                    [ matchChar 'h' insertTreeAbove
                    , matchChar 'H' insertTreeChild
                    , matchChar 'd' deleteCurrentHeader
                    , matchChar 'j' moveDown
                    , matchChar 'k' moveUp
                    , matchChar 'i' enterHeader
                    , matchKey KDown moveDown
                    , matchKey KUp moveUp
                    ]
              , inHeader $
                mconcat
                    [ onChar headerInsert
                    , matchKey KBS headerRemove
                    , matchKey KDel headerDelete
                    , matchKey KLeft headerLeft
                    , matchKey KRight headerRight
                    , matchKey KEnter exitHeader
                    ]
              , matchChar 'q' stop
              , matchKey KEsc stop
              ]
    }

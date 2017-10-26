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
              , matchChar 'H' insertTreeChild
              , inEntry $
                mconcat
                    [ matchChar 'd' deleteCurrentHeader
                    , matchChar 'i' enterHeader
                    , matchChar 't' enterTodoState
                    , matchChar 'j' moveDown
                    , matchChar 'k' moveUp
                    , matchChar 'h' moveLeft
                    , matchChar 'l' moveRight
                    , matchKey KDown moveDown
                    , matchKey KUp moveUp
                    , matchKey KLeft moveLeft
                    , matchKey KRight moveRight
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
              , inTodoState $
                mconcat
                    [ matchChar ' ' todoStateClear
                    , matchChar 't' $ todoStateSet "TODO" >> exitTodoState
                    , matchChar 'd' $ todoStateSet "DONE" >> exitTodoState
                    ]
              , matchChar 'q' stop
              , matchKey KEsc stop
              ]
    }

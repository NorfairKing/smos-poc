module Smos.Default where

import Import

import Smos
import Smos.Actions
import Smos.Keys
import Smos.Style

defaultSmos :: IO ()
defaultSmos = smos (defaultConfig :: SmosConfig ())

defaultConfig :: Ord e => SmosConfig e
defaultConfig =
    SmosConfig
    { configKeyMap =
          mconcat
              [ inEmpty $
                mconcat
                    [ matchChar 'h' $ insertTreeBelow >> enterHeader
                    , matchChar 'q' stop
                    , matchKey KEsc stop
                    ]
              , inEntry $
                mconcat
                    [ matchChar 'h' $ insertTreeBelow >> enterHeader
                    , matchChar 'H' $ insertTreeChild >> enterHeader
                    , matchChar 'd' deleteCurrentHeader
                    , matchChar 'i' $ enterHeader >> headerStart
                    , matchChar 'a' $ enterHeader >> headerEnd
                    , matchChar 't' enterTodoState
                    , matchChar 'c' clockIn
                    , matchChar 'o' clockOut
                    , matchChar 'j' moveDown
                    , matchChar 'k' moveUp
                    , matchChar 'h' moveLeft
                    , matchChar 'l' moveRight
                    , matchKey KDown moveDown
                    , matchKey KUp moveUp
                    , matchKey KLeft moveLeft
                    , matchKey KRight moveRight
                    , matchChar 'q' stop
                    , matchKey KEsc stop
                    ]
              , inHeader $
                mconcat
                    [ onChar headerInsert
                    , matchKey KBS headerRemove
                    , matchKey KDel headerDelete
                    , matchKey KLeft headerLeft
                    , matchKey KRight headerRight
                    , matchKey KEnter exitHeader
                    , matchKey KEsc exitHeader
                    ]
              , inTodoState $
                mconcat
                    [ matchChar ' ' todoStateClear
                    , matchChar 't' $ todoStateSet "TODO" >> exitTodoState
                    , matchChar 'd' $ todoStateSet "DONE" >> exitTodoState
                    ]
              ]
    , configAttrMap =
          applyAttrMappings
              [ (todoStateSpecificAttr "TODO", fg red)
              , (todoStateSpecificAttr "DONE", fg green)
              , (todoStateAttr, bg white)
              ] .
          defaultAttrMap
    }

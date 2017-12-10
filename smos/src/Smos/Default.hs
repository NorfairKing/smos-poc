{-# LANGUAGE OverloadedStrings #-}

module Smos.Default where

import Import

import Smos
import Smos.Actions
import Smos.Keys
import Smos.Style

defaultSmos :: IO ()
defaultSmos = smos defaultConfig

defaultConfig :: SmosConfig
defaultConfig =
    SmosConfig
    { configKeyMap =
          mconcat
              [ inEmpty $
                mconcat
                    [ matchChar 'h' $ insertTreeBelow >> enterHeader
                    , matchChar 'H' $ insertTreeChild >> enterHeader
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
                    , matchChar 'e' $ enterContents >> contentsStart
                    , afterChar 'g' $
                      mconcat
                          [ matchChar 'i' $ enterTag >> tagsSelectFirst
                          , matchChar 'I' $
                            enterTag >> tagsSelectFirst >> tagsSelectPrev
                          , matchChar 'a' $ enterTag >> tagsSelectLast
                          , matchChar 'A' $
                            enterTag >> tagsSelectLast >> tagsSelectNext
                          , matchChar 'w' $ tagToggle "work"
                          , matchChar 'h' $ tagToggle "home"
                          ]
                    , afterChar 'c' $
                      mconcat [matchChar 'i' clockIn, matchChar 'o' clockOut]
                    , afterChar 't' $
                      mconcat
                          [ matchChar ' ' todoStateClear
                          , matchChar 't' $ todoStateSet "TODO"
                          , matchChar 'n' $ todoStateSet "NEXT"
                          , matchChar 's' $ todoStateSet "STARTED"
                          , matchChar 'r' $ todoStateSet "READY"
                          , matchChar 'w' $ todoStateSet "WAITING"
                          , matchChar 'd' $ todoStateSet "DONE"
                          , matchChar 'c' $ todoStateSet "CANCELLED"
                          ]
                    , matchChar 'j' moveDown
                    , matchChar 'k' moveUp
                    , matchChar 'h' moveLeft
                    , matchChar 'l' moveRight
                    , matchChar 'J' swapDown
                    , matchChar 'K' swapUp
                    , matchChar 'H' swapLeft
                    , matchChar 'L' swapRight
                    , matchKey KUp moveUp
                    , matchKey KDown moveDown
                    , matchKey KLeft moveLeft
                    , matchKey KRight moveRight
                    , matchKeyPress (KeyPress KUp [MShift]) swapUp
                    , matchKeyPress (KeyPress KDown [MShift]) swapDown
                    , matchKeyPress (KeyPress KLeft [MShift]) swapLeft
                    , matchKeyPress (KeyPress KRight [MShift]) swapRight
                    , afterChar 'v' $
                      let editor = "urxvt -e vim"
                      in mconcat
                             [ matchChar 'c' $ editorOnContents editor
                             , matchChar 'g' $ editorOnTags editor
                             , matchChar 'l' $ editorOnLogbook editor
                             , matchChar 't' $ editorOnTimestamps editor
                             , matchChar 'p' $ editorOnProperties editor
                             ]
                    , matchChar 's' save
                    , matchChar 'q' stop
                    , matchKey KEsc stop
                    , matchString "DD" toggleShowDebug
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
              , inContents $
                mconcat
                    [ onChar contentsInsert
                    , matchKey KBS contentsRemove
                    , matchKey KDel contentsDelete
                    , matchKey KLeft contentsLeft
                    , matchKey KRight contentsRight
                    , matchKey KUp contentsUp
                    , matchKey KDown contentsDown
                    , matchKey KEnter contentsNewline
                    , matchKey KEsc exitContents
                    ]
              , inTags $
                mconcat
                    [ onCharM $ \c ->
                          if c == '\t'
                              then Nothing
                              else Just $ tagInsert c
                    , matchKey KBS tagRemove
                    , matchKey KDel tagDelete
                    , matchKey KLeft tagLeft
                    , matchKey KRight tagRight
                    , matchChar '\t' tagsSelectNext
                    , matchKey KBackTab tagsSelectPrev
                    , matchKeyPress
                          (KeyPress (KChar '\t') [MShift])
                          tagsSelectPrev
                    , matchKey KEnter exitTag
                    , matchKey KEsc exitTag
                    ]
              ]
    , configAttrMap =
          let col = rgbColor :: Int -> Int -> Int -> Color
              orange = col 255 165 0
              brown = col 205 133 63
          in applyAttrMappings
                 [ (todoStateSpecificAttr "TODO", fg red)
                 , (todoStateSpecificAttr "NEXT", fg orange)
                 , (todoStateSpecificAttr "STARTED", fg orange)
                 , (todoStateSpecificAttr "WAITING", fg blue)
                 , (todoStateSpecificAttr "READY", fg brown)
                 , (todoStateSpecificAttr "DONE", fg green)
                 , (todoStateSpecificAttr "CANCELLED", fg green)
                 , (selectedAttr <> tagAttr, fg brightWhite)
                 ] .
             defaultAttrMap
    , configAgendaFiles =
          do home <- getHomeDir
             d <- resolveDir home "smos"
             fromMaybe [] <$> forgivingAbsence (snd <$> listDirRecur d)
    }

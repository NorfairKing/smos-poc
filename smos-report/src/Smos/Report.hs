{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report
    ( smosReport
    -- * EntryReport
    , EntryReport
    , entryReport
    , prettyEntryReport
    -- * Agenda report
    , AgendaReport
    , agendaReport
    , prettyAgendaReport
    -- * Helper functions
    , stateIs
    , hasTag
    , (&&&)
    , (|||)
    ) where

import Import

import Text.PrettyPrint.ANSI.Leijen (putDoc)

import Smos.Data
import Smos.Report.Agenda
import Smos.Report.Config
import Smos.Report.Entry
import Smos.Report.OptParse

smosReport :: SmosReportConfig -> IO ()
smosReport src@SmosReportConfig {..} = do
    Instructions reportFunc Settings <- getInstructions src
    afs <- reportConfigAgendaFiles
    sfs <-
        fmap catMaybes $
        forM afs $ \af -> do
            errOrSf <- readSmosFile af
            case errOrSf of
                Nothing -> do
                    putStrLn $
                        unwords ["WARNING:", "File not found:", toFilePath af]
                    pure Nothing
                Just (Left err) -> do
                    putStrLn $
                        unwords
                            [ "WARNING:"
                            , "Error while reading file"
                            , toFilePath af ++ ":"
                            , err
                            ]
                    pure Nothing
                Just (Right sf) -> pure $ Just (af, sf)
    let doc = reportFunc sfs
    putDoc doc

stateIs :: TodoState -> Entry -> Bool
stateIs s e = entryState e == Just s

hasTag :: Tag -> Entry -> Bool
hasTag t e = t `elem` entryTags e

(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) p1 p2 a = p1 a && p2 a

(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) p1 p2 a = p1 a || p2 a

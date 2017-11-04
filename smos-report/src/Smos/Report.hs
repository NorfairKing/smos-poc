{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report
    ( smosReport
    , EntryReport(..)
    , entryReport
    , stateIs
    , hasTag
    , (&&&)
    , (|||)
    ) where

import Import

import Data.Yaml

import Text.PrettyPrint.ANSI.Leijen (putDoc)

import Smos.Data
import Smos.Report.Config
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

-- | A report of entries, made by filtering using an element-wise predicate
newtype EntryReport = EntryReport
    { entryReportEntries :: [(Path Abs File, [Entry])]
    } deriving (Show, Eq, Generic)

instance Validity EntryReport

instance FromJSON EntryReport where
    parseJSON = withObject "EntryReport" $ \o -> EntryReport <$> o .: "entries"

instance ToJSON EntryReport where
    toJSON EntryReport {..} = object ["entries" .= entryReportEntries]

entryReport :: (Entry -> Bool) -> [(Path Abs File, SmosFile)] -> EntryReport
entryReport p fs =
    EntryReport {entryReportEntries = [(f, go sf) | (f, sf) <- fs]}
  where
    go :: SmosFile -> [Entry]
    go SmosFile {..} = gof smosFileForest
    gof = concatMap got
    got Node {..} = [rootLabel | p rootLabel]

stateIs :: TodoState -> Entry -> Bool
stateIs s e = entryState e == Just s

hasTag :: Tag -> Entry -> Bool
hasTag t e = t `elem` entryTags e

(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) p1 p2 a = p1 a && p2 a

(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) p1 p2 a = p1 a || p2 a

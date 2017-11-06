{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Entry.Pretty where

import Import hiding ((<$>))

import qualified Data.Text as T
import Text.PrettyPrint.ANSI.Leijen

import Smos.Data
import Smos.Report

prettyEntryReport :: EntryReport -> Doc
prettyEntryReport EntryReport {..} =
    vcat (map (uncurry go) entryReportEntries) <$> hardline
  where
    go :: Path Abs File -> [Entry] -> Doc
    go f e = string (toFilePath f) <$$> indent 2 (vcat $ map goe e)
    goe :: Entry -> Doc
    goe e@Entry {..} =
        maybe empty (text . T.unpack . todoStateText) (entryState e) <+>
        text (T.unpack $ headerText entryHeader)

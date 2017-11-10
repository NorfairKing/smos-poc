{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Agenda.Pretty
    ( prettyAgendaReport
    ) where

import Import hiding ((<$>))

import qualified Data.Text as T
import Data.Time
import Text.PrettyPrint.ANSI.Leijen

import Smos.Data
import Smos.Report.Agenda.Types

prettyAgendaReport :: AgendaReport -> Doc
prettyAgendaReport AgendaReport {..} =
    vcat (map (uncurry god) agendaReportAgenda) <$> hardline
  where
    god :: Day -> [(Path Abs File, [Entry])] -> Doc
    god _ tups = vcat $ map (uncurry go) tups
    go :: Path Abs File -> [Entry] -> Doc
    go f e = string (toFilePath f) <$$> indent 2 (vcat $ map goe e)
    goe :: Entry -> Doc
    goe e@Entry {..} =
        maybe empty (text . T.unpack . todoStateText) (entryState e) <+>
        text (T.unpack $ headerText entryHeader)

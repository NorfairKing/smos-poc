{-# LANGUAGE RecordWildCards #-}

module Smos.Data
    ( module Smos.Data.Types
    , readSmosFile
    , writeSmosFile
    , emptySmosFile
    , prettySmosForest
    , clockInAt
    , clockOutAt
    ) where

import Import

import qualified Data.ByteString as SB
import qualified Data.Text as T
import Data.Time
import Data.Yaml as Yaml

import Smos.Data.Types

readSmosFile :: MonadIO m => Path Abs File -> m (Maybe (Either String SmosFile))
readSmosFile fp = do
    mContents <- liftIO $ forgivingAbsence $ SB.readFile $ toFilePath fp
    case mContents of
        Nothing -> pure Nothing
        Just contents -> pure $ Just $ Yaml.decodeEither contents

writeSmosFile :: MonadIO m => Path Abs File -> SmosFile -> m ()
writeSmosFile fp sf =
    liftIO $ do
        ensureDir $ parent fp
        SB.writeFile (toFilePath fp) (Yaml.encode sf)

emptySmosFile :: SmosFile
emptySmosFile = SmosFile $ SmosForest []

prettySmosForest :: SmosForest -> String
prettySmosForest (SmosForest ts) = unlines $ map prettySmosTree ts

prettySmosTree :: SmosTree -> String
prettySmosTree SmosTree {..} =
    unlines [prettySmosEntry treeEntry, prettySmosForest treeForest]

prettySmosEntry :: Entry -> String
prettySmosEntry Entry {..} = T.unpack $ headerText entryHeader

clockInAt :: UTCTime -> Logbook -> Maybe Logbook
clockInAt now lb =
    case lb of
        LogEnd -> Just $ LogOpenEntry now LogEnd
        LogEntry {} -> Just $ LogOpenEntry now lb
        LogOpenEntry {} -> Nothing

clockOutAt :: UTCTime -> Logbook -> Maybe Logbook
clockOutAt now lb =
    case lb of
        LogEnd -> Nothing
        LogEntry {} -> Nothing
        LogOpenEntry start lb' -> Just $ LogEntry start now lb'

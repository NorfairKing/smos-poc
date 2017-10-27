{-# LANGUAGE RecordWildCards #-}

module Smos.Data
    ( module Smos.Data.Types
    , readSmosFile
    , writeSmosFile
    , emptySmosFile
    , prettySmosForest
    ) where

import Import

import qualified Data.ByteString as SB
import qualified Data.Text as T
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

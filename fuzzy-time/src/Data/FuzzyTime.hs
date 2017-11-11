{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Data.FuzzyTime
    ( parseFuzzyDateTime
    , FuzzyDateTime(..)
    ) where

import Import

import Data.Tree

import Text.Megaparsec

-- Can handle:
--
-- - yesterday
-- - now
-- - today
-- - tomorrow
--
-- and all non-ambiguous prefixes
parseFuzzyDateTime :: String -> Maybe FuzzyDateTime
parseFuzzyDateTime = parseMaybe fuzzyDateTimeParser

type Parser = Parsec Dec String

fuzzyDateTimeParser :: Parser FuzzyDateTime
fuzzyDateTimeParser = do
    let pf =
            makeParseForest
                [ ("yesterday", Yesterday)
                , ("now", Now)
                , ("today", Today)
                , ("tomorrow", Tomorrow)
                ]
    s <- some letterChar
    case lookupInParseForest s pf of
        Nothing -> fail "nope"
        Just f -> pure f

data FuzzyDateTime
    = Yesterday
    | Now
    | Today
    | Tomorrow
    deriving (Show, Eq, Generic)

lookupInParseForest :: Eq c => [c] -> Forest (c, Maybe a) -> Maybe a
lookupInParseForest = gof
  where
    gof :: Eq c => [c] -> Forest (c, Maybe a) -> Maybe a
    gof cs = msum . map (got cs)
    got :: Eq c => [c] -> Tree (c, Maybe a) -> Maybe a
    got [] _ = Nothing
    got (c:cs) Node {..} =
        let (tc, tma) = rootLabel
        in if tc == c
               then case cs of
                        [] -> tma
                        _ -> gof cs subForest
               else Nothing

-- traceForest :: Show a => Forest a -> Forest a
-- traceForest f = trace (drawForest $ map (fmap show) f) f
makeParseForest :: Eq c => [([c], a)] -> Forest (c, Maybe a)
makeParseForest = foldl insertf []
  where
    insertf :: Eq c => Forest (c, Maybe a) -> ([c], a) -> Forest (c, Maybe a)
    insertf for ([], _) = for
    insertf for (c:cs, a) =
        case find ((== c) . fst . rootLabel) for of
            Nothing ->
                let got [] = Nothing
                    got (c_:cs_) =
                        Just $ Node (c_, Just a) $ maybeToList $ got cs_
                in case got (c : cs) of
                       Nothing -> for -- Should not happen, but is fine
                       Just t -> t : for
            Just n ->
                flip map for $ \t ->
                    let (tc, _) = rootLabel t
                    in if tc == c
                           then n
                                { rootLabel = (tc, Nothing)
                                , subForest = insertf (subForest n) (cs, a)
                                }
                           else t

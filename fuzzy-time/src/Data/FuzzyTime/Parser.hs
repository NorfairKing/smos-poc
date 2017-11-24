{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Data.FuzzyTime.Parser
    ( fuzzyDayP
    , fuzzyDayOfTheWeekP
    , Parser
    ) where

import Import

import Data.Time
import Data.Tree

import Text.Megaparsec

import Data.FuzzyTime.FuzzyTypes

type Parser = Parsec Dec String

-- | Can handle:
--
-- - yesterday
-- - now
-- - today
-- - tomorrow
-- - "%Y-%m-%d"
--
-- and all non-ambiguous prefixes
fuzzyDayP :: Parser FuzzyDay
fuzzyDayP =
    recTreeParser
        [ ("yesterday", Yesterday)
        , ("now", Now)
        , ("today", Today)
        , ("tomorrow", Tomorrow)
        ] <|>
    fmap
        ExactDay
        (some (digitChar <|> char '-') >>=
         parseTimeM True defaultTimeLocale "%Y-%m-%d")

-- | Can handle:
--
-- - monday
-- - tuesday
-- - wednesday
-- - thursday
-- - friday
-- - saturday
-- - sunday
--
-- and all non-ambiguous prefixes
fuzzyDayOfTheWeekP :: Parser FuzzyDayOfTheWeek
fuzzyDayOfTheWeekP =
    recTreeParser
        [ ("monday", Monday)
        , ("tuesday", Tuesday)
        , ("wednesday", Wednesday)
        , ("thursday", Thursday)
        , ("friday", Friday)
        , ("saturday", Saturday)
        , ("sunday", Sunday)
        ]

recTreeParser :: [(String, a)] -> Parser a
recTreeParser tups = do
    let pf = makeParseForest tups
    s <- some letterChar
    case lookupInParseForest s pf of
        Nothing ->
            fail $
            "Could not parse any of these recursively unambiguously: " ++
            show (map fst tups)
        Just f -> pure f

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

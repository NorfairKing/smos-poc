{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cursor.Tree
    ( ForestCursor
    , makeForestCursor
    , foldForestSel
    , forestCursorParent
    , forestCursorElems
    , forestCursorSelectIx
    , forestCursorSelectFirst
    , forestCursorSelectLast
    , forestCursorInsertAt
    , forestCursorInsertAtStart
    , forestCursorInsertAtEnd
    , TreeCursor
    , treeCursorParent
    , treeCursorPrevElemens
    , treeCursorNextElemens
    , treeCursorIndex
    , treeCursorValue
    , treeCursorForest
    , foldTreeSel
    , treeCursorSelectPrev
    , treeCursorSelectNext
    , treeCursorValueL
    , treeCursorForestL
    , treeCursorInsertAbove
    , treeCursorInsertBelow
    , treeCursorInsertChildAt
    , treeCursorInsertChildAtStart
    , treeCursorInsertChildAtEnd
    , treeCursorDeleteCurrent
    ) where

import Import

import Data.Tree

import Lens.Micro

import Cursor.Class

data ForestCursor a = ForestCursor
    { forestCursorParent :: Maybe (TreeCursor a)
    , forestCursorElems :: [TreeCursor a]
    }

instance (Validity a, Build a, Validity (Building a)) =>
         Validity (ForestCursor a) where
    isValid a = isValid (build a) && isValid (rebuild a)
    validate a = (build a <?!> "build") <> (rebuild a <?!> "rebuild")

instance (Show a, Build a, Show (Building a)) => Show (ForestCursor a) where
    show ForestCursor {..} =
        unlines
            $(case forestCursorParent of
                  Nothing -> "Nothing"
                  Just _ -> "Just [..]") :
        map ((" -" ++) . show) forestCursorElems

instance (Eq a, Build a, Eq (Building a)) => Eq (ForestCursor a) where
    (==) = ((==) `on` build) &&& ((==) `on` rebuild)

instance Build a => Rebuild (ForestCursor a) where
    type ReBuilding (ForestCursor a) = Forest (Building a)
    rebuild fc =
        case forestCursorParent fc of
            Nothing -> build fc
            Just pc -> rebuild pc
    selection ForestCursor {..} =
        case forestCursorParent of
            Nothing -> []
            Just p -> 1 : selection p

instance Build a => Build (ForestCursor a) where
    type Building (ForestCursor a) = Forest (Building a)
    build = map build . forestCursorElems

makeForestCursor ::
       (a `BuiltFrom` (Building a), Parent a ~ TreeCursor a, Build a)
    => Forest (Building a)
    -> ForestCursor a
makeForestCursor = forestCursor Nothing

forestCursor ::
       (a `BuiltFrom` (Building a), Parent a ~ TreeCursor a, Build a)
    => Maybe (TreeCursor a)
    -> Forest (Building a)
    -> ForestCursor a
forestCursor mpar sf = fc
  where
    fc =
        ForestCursor
        {forestCursorParent = mpar, forestCursorElems = treeElems fc sf}

foldForestSel ::
       (Maybe [Int] -> Tree a -> r)
    -> ([(Int, r)] -> r)
    -> Maybe [Int]
    -> Forest a
    -> r
foldForestSel rFunc combFunc msel sf =
    combFunc $
    flip map (zip [0 ..] sf) $ \(ix_, st) -> (ix_, rFunc (drillSel msel ix_) st)

forestElemsL ::
       (Functor f, a `BuiltFrom` (Building a), Parent a ~ TreeCursor a, Build a)
    => ([TreeCursor a] -> f [TreeCursor a])
    -> ForestCursor a
    -> f (ForestCursor a)
forestElemsL = lens getter setter
  where
    getter = forestCursorElems
    setter ForestCursor {..} elems = fc'
      where
        fc' =
            ForestCursor
            { forestCursorParent =
                  rebuildForestParentCursor (const fc') forestCursorParent
            , forestCursorElems = elems
            }

rebuildForestParentCursor ::
       (a `BuiltFrom` (Building a), Parent a ~ TreeCursor a, Build a)
    => (ForestCursor a -> ForestCursor a)
    -> Maybe (TreeCursor a)
    -> Maybe (TreeCursor a)
rebuildForestParentCursor func mtc =
    (\tc -> tc & treeCursorForestL %~ func) <$> mtc

forestCursorSelectIx :: ForestCursor a -> Int -> Maybe (TreeCursor a)
forestCursorSelectIx fc = atMay $ forestCursorElems fc

forestCursorSelectFirst :: ForestCursor a -> Maybe (TreeCursor a)
forestCursorSelectFirst fc =
    case forestCursorElems fc of
        [] -> Nothing
        (tc:_) -> Just tc

forestCursorSelectLast :: ForestCursor a -> Maybe (TreeCursor a)
forestCursorSelectLast fc =
    case reverse $ forestCursorElems fc of
        [] -> Nothing
        (tc:_) -> Just tc

forestCursorInsertAt ::
       (a `BuiltFrom` (Building a), Parent a ~ TreeCursor a, Build a)
    => Int
    -> Tree (Building a)
    -> ForestCursor a
    -> ForestCursor a
forestCursorInsertAt ix_ newTree fc = fc'
  where
    fc' =
        fc & forestElemsL %~
        (\els ->
             treeElems fc' $
             map build (prevs els) ++ [newTree] ++ map build (nexts els))
    ffilter rel = filter ((`rel` ix_) . treeCursorIndex)
    prevs = ffilter (<)
    nexts = ffilter (>=)

forestCursorInsertAtStart ::
       (a `BuiltFrom` (Building a), Parent a ~ TreeCursor a, Build a)
    => Tree (Building a)
    -> ForestCursor a
    -> ForestCursor a
forestCursorInsertAtStart = forestCursorInsertAt 0

forestCursorInsertAtEnd ::
       (a `BuiltFrom` (Building a), Parent a ~ TreeCursor a, Build a)
    => Tree (Building a)
    -> ForestCursor a
    -> ForestCursor a
forestCursorInsertAtEnd t fc =
    forestCursorInsertAt (length $ forestCursorElems fc) t fc

data TreeCursor a = TreeCursor
    { treeCursorParent :: ForestCursor a
    , treeCursorPrevElemens :: [TreeCursor a] -- ^ In reverse order, so that the first element is the nearest.
    , treeCursorNextElemens :: [TreeCursor a]
    , treeCursorIndex :: Int
    , treeCursorValue :: a
    , treeCursorForest :: ForestCursor a
    }

instance (Validity a, Build a, Validity (Building a)) =>
         Validity (TreeCursor a) where
    isValid a = isValid (build a) && isValid (rebuild a)
    validate a = (build a <?!> "build") <> (rebuild a <?!> "rebuild")

instance (Eq a, Build a, Eq (Building a)) => Eq (TreeCursor a) where
    (==) = ((==) `on` build) &&& ((==) `on` rebuild)

instance Build a => Rebuild (TreeCursor a) where
    type ReBuilding (TreeCursor a) = Forest (Building a)
    rebuild = rebuild . treeCursorParent
    selection TreeCursor {..} =
        length treeCursorPrevElemens : selection treeCursorParent

instance Build a => Build (TreeCursor a) where
    type Building (TreeCursor a) = Tree (Building a)
    build TreeCursor {..} =
        Node
        {rootLabel = build treeCursorValue, subForest = build treeCursorForest}

instance (Show a, Build a, Show (Building a)) => Show (TreeCursor a) where
    show TreeCursor {..} =
        unlines
            ("[..]" :
             map
                 (" |-" ++)
                 (concat
                      [ map (const "tree") treeCursorPrevElemens
                      , [ "---"
                        , unwords
                              [ show treeCursorIndex
                              , show $ build treeCursorValue
                              , show $ build treeCursorForest
                              ]
                        , "---"
                        ]
                      , map (const "tree") treeCursorNextElemens
                      ]))

treeCursorValueL ::
       (Functor f, a `BuiltFrom` (Building a), Parent a ~ TreeCursor a, Build a)
    => (a -> f a)
    -> TreeCursor a
    -> f (TreeCursor a)
treeCursorValueL = lens getter setter
  where
    getter = treeCursorValue
    setter tc ec = treeCursorModify (const ec) id tc

treeCursorForestL ::
       (Functor f, a `BuiltFrom` (Building a), Parent a ~ TreeCursor a, Build a)
    => (ForestCursor a -> f (ForestCursor a))
    -> TreeCursor a
    -> f (TreeCursor a)
treeCursorForestL = lens getter setter
  where
    getter = treeCursorForest
    setter tc fc = treeCursorModify id (const fc) tc

treeCursorModify ::
       (a `BuiltFrom` (Building a), Parent a ~ TreeCursor a, Build a)
    => (a -> a)
    -> (ForestCursor a -> ForestCursor a)
    -> TreeCursor a
    -> TreeCursor a
treeCursorModify efunc ffunc tc = tc''
  where
    tc' =
        tc
        { treeCursorValue = efunc $ treeCursorValue tc
        , treeCursorForest = ffunc $ treeCursorForest tc
        }
    tcs =
        reverse (treeCursorPrevElemens tc) ++ [tc'] ++ treeCursorNextElemens tc
    trees = map build tcs
    fc = treeCursorParent tc & forestElemsL .~ els
    els = treeElems fc trees
    tc'' = els !! treeCursorIndex tc

treeElems ::
       (a `BuiltFrom` (Building a), Build a, Parent a ~ TreeCursor a)
    => ForestCursor a
    -> [Tree (Building a)]
    -> [TreeCursor a]
treeElems fc sts = tcs
  where
    tcs = zipWith tc [0 ..] sts
    tc i st = cur
      where
        cur =
            TreeCursor
            { treeCursorParent = fc
            , treeCursorPrevElemens =
                  reverse $ filter ((< i) . treeCursorIndex) tcs
            , treeCursorNextElemens = filter ((> i) . treeCursorIndex) tcs
            , treeCursorIndex = i
            , treeCursorValue = makeWith cur $ rootLabel st
            , treeCursorForest = fc'
            }
        fc' = forestCursor (Just cur) (subForest st)

foldTreeSel ::
       (Maybe [Int] -> a -> r)
    -> (Maybe [Int] -> Forest a -> r)
    -> (r -> r -> r)
    -> Maybe [Int]
    -> Tree a
    -> r
foldTreeSel eFunc fFunc combFunc msel Node {..} =
    eFunc (drillSel msel 0) rootLabel `combFunc`
    fFunc (drillSel msel 1) subForest

treeCursorSelectPrev :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorSelectPrev tc =
    case treeCursorPrevElemens tc of
        [] -> Nothing
        (tc':_) -> Just tc'

treeCursorSelectNext :: TreeCursor a -> Maybe (TreeCursor a)
treeCursorSelectNext tc =
    case treeCursorNextElemens tc of
        [] -> Nothing
        (tc':_) -> Just tc'

treeCursorInsertAbove ::
       (a `BuiltFrom` (Building a), Build a, Parent a ~ TreeCursor a)
    => TreeCursor a
    -> Tree (Building a)
    -> TreeCursor a
treeCursorInsertAbove tc t = fromJust $ forestCursorSelectIx newpar newIx
  where
    newIx = treeCursorIndex tc
    newpar = forestCursorInsertAt newIx t (treeCursorParent tc)

treeCursorInsertBelow ::
       (a `BuiltFrom` (Building a), Build a, Parent a ~ TreeCursor a)
    => TreeCursor a
    -> Tree (Building a)
    -> TreeCursor a
treeCursorInsertBelow tc t =
    fromJust $ forestCursorSelectIx newpar $ treeCursorIndex tc + 1
  where
    newIx = treeCursorIndex tc + 1
    newpar = forestCursorInsertAt newIx t (treeCursorParent tc)

treeCursorInsertChildAt ::
       (a `BuiltFrom` (Building a), Build a, Parent a ~ TreeCursor a)
    => Int
    -> Tree (Building a)
    -> TreeCursor a
    -> TreeCursor a
treeCursorInsertChildAt ix_ t tc =
    tc & treeCursorForestL %~ forestCursorInsertAt ix_ t

treeCursorInsertChildAtStart ::
       (a `BuiltFrom` (Building a), Build a, Parent a ~ TreeCursor a)
    => Tree (Building a)
    -> TreeCursor a
    -> TreeCursor a
treeCursorInsertChildAtStart = treeCursorInsertChildAt 0

treeCursorInsertChildAtEnd ::
       (a `BuiltFrom` (Building a), Build a, Parent a ~ TreeCursor a)
    => Tree (Building a)
    -> TreeCursor a
    -> TreeCursor a
treeCursorInsertChildAtEnd t tc =
    treeCursorInsertChildAt
        (length $ forestCursorElems $ treeCursorForest tc)
        t
        tc

treeCursorDeleteCurrent ::
       (a `BuiltFrom` (Building a), Build a, Parent a ~ TreeCursor a)
    => TreeCursor a
    -> Either (ForestCursor a) (TreeCursor a)
treeCursorDeleteCurrent tc = tc''
  where
    tcs = reverse (treeCursorPrevElemens tc) ++ treeCursorNextElemens tc
    trees = map build tcs
    for = treeCursorParent tc & forestElemsL .~ els
    els = treeElems for trees
    tc'' =
        let ix_ = treeCursorIndex tc
        in maybe (Left for) Right $
           (els `atMay` ix_) `mplus` (els `atMay` (ix_ - 1))

(&&&) :: (a -> b -> Bool) -> (a -> b -> Bool) -> a -> b -> Bool
(&&&) op1 op2 a b = op1 a b && op2 a b
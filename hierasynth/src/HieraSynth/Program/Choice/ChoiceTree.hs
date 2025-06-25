{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module HieraSynth.Program.Choice.ChoiceTree
  ( ChoiceMeta (..),
    ChoiceTree (..),
  )
where

import Data.List ((\\))
import Grisette
  ( Doc,
    GenSym (fresh),
    GenSymSimple (simpleFresh),
    Mergeable,
    PPrint (pformat),
    SimpleMergeable,
    Union,
    allClasses0,
    allClasses01,
    chooseSimpleFresh,
    chooseUnionFresh,
    derive,
    pprintClasses,
  )
import HieraSynth.Program.Concrete.OpPPrint
  ( OpPPrint (describeArguments, pformatOp, prefixResults),
  )
import HieraSynth.Util.Pretty (encloseList, encloseListIfNotSingle)

data ChoiceMeta = NoSplit | Split {_seqNum :: Int, _onlyWhenSuccess :: Bool}

derive [''ChoiceMeta] allClasses0

data ChoiceTree op = Leaf [op] | Branch ChoiceMeta [ChoiceTree op]
  deriving (Functor)

derive [''ChoiceTree] (allClasses01 \\ pprintClasses)

pformatChoiceTree :: (op -> Doc ann) -> ChoiceTree op -> Doc ann
pformatChoiceTree pp (Leaf ops) =
  encloseListIfNotSingle "<" ">" "," $ map pp ops
pformatChoiceTree pp (Branch NoSplit choices) =
  encloseListIfNotSingle "<" ">" "," $ map (pformatChoiceTree pp) choices
pformatChoiceTree pp (Branch (Split depth onlyWhenSuccess) subLists) =
  encloseList "{" "}" "," (map (pformatChoiceTree pp) subLists)
    <> "@"
    <> pformat depth
    <> (if onlyWhenSuccess then "s" else "")

instance (PPrint op) => PPrint (ChoiceTree op) where
  pformat = pformatChoiceTree pformat

instance (OpPPrint op) => OpPPrint (ChoiceTree op) where
  prefixResults _ = return []
  describeArguments _ = return []
  pformatOp = pformatChoiceTree pformatOp

flattenChoiceTree :: ChoiceTree op -> [op]
flattenChoiceTree (Leaf ops) = ops
flattenChoiceTree (Branch _ subLists) = concatMap flattenChoiceTree subLists

instance
  {-# OVERLAPS #-}
  (GenSym opSpec op) =>
  GenSym (ChoiceTree opSpec) op
  where
  fresh tree = do
    ops <- mapM fresh $ flattenChoiceTree tree
    chooseUnionFresh ops

instance
  {-# OVERLAPS #-}
  (GenSym opSpec op) =>
  GenSym (ChoiceTree opSpec) (Union op)
  where
  fresh tree = do
    ops <- mapM fresh $ flattenChoiceTree tree
    chooseUnionFresh ops

instance
  {-# OVERLAPS #-}
  (GenSymSimple opSpec op, SimpleMergeable op) =>
  GenSymSimple (ChoiceTree opSpec) op
  where
  simpleFresh tree = do
    ops <- mapM simpleFresh $ flattenChoiceTree tree
    chooseSimpleFresh ops

instance
  {-# OVERLAPS #-}
  (GenSym opSpec op, Mergeable op) =>
  GenSymSimple (ChoiceTree opSpec) (Union op)
  where
  simpleFresh tree = do
    ops <- mapM simpleFresh $ flattenChoiceTree tree
    chooseUnionFresh ops

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module HieraSynth.Program.Choice.ComponentBag
  ( ComponentBag (..),
  )
where

import Data.List ((\\))
import Grisette
  ( GenSymSimple (simpleFresh),
    PPrint (pformat),
    SimpleMergeable,
    allClasses012,
    derive,
    hardline,
    liftFresh,
    nest,
    pprintClasses,
  )
import HieraSynth.Context (SymbolicContext)
import HieraSynth.Operator.OpTyping (OpTyping)
import HieraSynth.Program.Choice.ChoiceTree (ChoiceTree)
import HieraSynth.Program.Choice.Split
  ( LowestSeqNum (lowestSeqNum),
    PartitionSpec (partitionSpec),
    combineLowestSeqNum,
  )
import HieraSynth.Program.ComponentSketch
  ( MkFreshProg (mkFreshProg),
    freshStmts,
  )
import qualified HieraSynth.Program.ComponentSketch.Program as Component
import HieraSynth.Program.Concrete (OpPPrint (pformatOp))
import HieraSynth.Program.ProgPPrint (ProgPPrint (pformatProg))
import HieraSynth.Util.Pretty
  ( encloseList,
    parenCommaList,
    parenCommaListIfNotSingle,
  )

data ComponentBag opSpec ty = ComponentBag
  { argTypes :: [ty],
    components :: [(ChoiceTree opSpec, Int)],
    resTypes :: [ty]
  }

derive [''ComponentBag] (allClasses012 \\ pprintClasses)

instance LowestSeqNum (ComponentBag opSpec ty) where
  lowestSeqNum succeeded (ComponentBag _ components _) =
    combineLowestSeqNum $ map (lowestSeqNum succeeded . fst) components

partitionChoices ::
  Int -> (ChoiceTree opSpec, Int) -> [[(ChoiceTree opSpec, Int)]]
partitionChoices seqNum (choices, num) =
  let lst = partitionSpec seqNum choices
      go [] _ = undefined
      go [c] i = [[(c, i)]]
      go (c : cs) i =
        concat [((c, v) :) <$> go cs (i - v) | v <- [0 .. i]]
   in filter ((/= 0) . snd) <$> go lst num

partitionComponents ::
  Int -> [(ChoiceTree opSpec, Int)] -> [[(ChoiceTree opSpec, Int)]]
partitionComponents _ [] = [[]]
partitionComponents seqNum (c : cs) = do
  c' <- partitionChoices seqNum c
  cs' <- partitionComponents seqNum cs
  return $ c' ++ cs'

instance PartitionSpec (ComponentBag opSpec ty) where
  partitionSpec seqNum (ComponentBag argTypes components resTypes) = do
    let lst = partitionComponents seqNum components
     in [ComponentBag argTypes l resTypes | l <- lst]

instance (OpPPrint opSpec, PPrint ty) => ProgPPrint (ComponentBag opSpec ty) where
  pformatProg key (ComponentBag argTypes components resTypes) =
    let firstLine =
          nest (-2) $
            "sketch "
              <> pformat key
              <> parenCommaList (map pformat argTypes)
              <> " -> "
              <> parenCommaListIfNotSingle (map pformat resTypes)
              <> ":"
        prettyComponent (ops, i) = pformat i <> " * " <> pformatOp ops
     in Right $
          nest 2 $
            firstLine
              <> hardline
              <> "reorder "
              <> encloseList "{" "}" "," (map prettyComponent components)

instance
  ( GenSymSimple () symVarId,
    GenSymSimple ty0 ty1,
    GenSymSimple op0 op1,
    OpTyping op1 SymbolicContext,
    SimpleMergeable op1
  ) =>
  GenSymSimple (ComponentBag op0 ty0) (Component.Prog op1 symVarId ty1)
  where
  simpleFresh (ComponentBag argTypes components resTypes) = do
    let stmts = map (\(ops, i) -> freshStmts i (simpleFresh ops)) components
    argTypes <- mapM simpleFresh argTypes
    resTypes <- mapM simpleFresh resTypes
    liftFresh $ mkFreshProg argTypes stmts resTypes

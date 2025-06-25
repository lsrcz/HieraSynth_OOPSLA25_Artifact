{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HieraSynth.Program.CostModel.PerStmtCostModel
  ( PerStmtCostObj (..),
    OpCost (..),
  )
where

import Grisette (Mergeable, MonadUnion, Union, liftUnion)
import HieraSynth.Context (MonadContext)
import HieraSynth.Program.ProgCost (SymbolCostTable)

newtype PerStmtCostObj opCostObj = PerStmtCostObj opCostObj

class (MonadContext ctx) => OpCost opCostObj op cost ctx where
  opCost :: opCostObj -> SymbolCostTable cost ctx -> op -> ctx cost

instance
  (MonadUnion ctx, OpCost opCostObj op cost ctx, Mergeable op) =>
  OpCost opCostObj (Union op) cost ctx
  where
  opCost obj table op = liftUnion op >>= opCost obj table

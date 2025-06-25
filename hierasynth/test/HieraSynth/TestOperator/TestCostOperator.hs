{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module HieraSynth.TestOperator.TestCostOperator
  ( TestCostOperator (..),
    TestCost (..),
  )
where

import Grisette (Mergeable, allClasses0, derive, mrgReturn)
import HieraSynth.Context (MonadContext)
import HieraSynth.Program.CostModel.PerStmtCostModel (OpCost (opCost))

data TestCost = TestCost

newtype TestCostOperator = TestCostOperator Integer

derive [''TestCost, ''TestCostOperator] allClasses0

instance
  (Num cost, MonadContext ctx, Mergeable cost) =>
  OpCost TestCost TestCostOperator cost ctx
  where
  opCost _ _ (TestCostOperator x) = mrgReturn $ fromIntegral x

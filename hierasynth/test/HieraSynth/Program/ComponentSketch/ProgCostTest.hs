{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Program.ComponentSketch.ProgCostTest (progCostTest) where

import Grisette (ITEOp (symIte), SymInteger)
import HieraSynth.Context (SymbolicContext)
import qualified HieraSynth.Program.ComponentSketch as Component
import HieraSynth.Program.CostModel.PerStmtCostModel
  ( PerStmtCostObj (PerStmtCostObj),
  )
import HieraSynth.Program.ProgCost (ProgCost (progCost))
import HieraSynth.TestOperator.TestCostOperator
  ( TestCost (TestCost),
    TestCostOperator (TestCostOperator),
  )
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.SymbolicAssertion ((.@?=))

progCostTest :: Test
progCostTest = testCase "ProgCost" $ do
  let prog =
        Component.Prog
          []
          [ Component.Stmt (TestCostOperator 10) [] 0 [] 0 "x" [],
            Component.Stmt (TestCostOperator 20) [] 0 [] 0 "y" []
          ]
          []
  let cost =
        progCost (PerStmtCostObj TestCost) mempty prog ::
          SymbolicContext SymInteger
  cost .@?= return (symIte "x" 0 10 + symIte "y" 0 20)

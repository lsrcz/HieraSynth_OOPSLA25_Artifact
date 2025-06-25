{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Program.Concrete.ProgCostTest (progCostTest) where

import HieraSynth.Context (ConcreteContext)
import qualified HieraSynth.Program.Concrete as Concrete
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
import Test.HUnit ((@?=))

progCostTest :: Test
progCostTest = testCase "ProgCost" $ do
  let prog =
        Concrete.Prog
          []
          [ Concrete.Stmt (TestCostOperator 10) [] [],
            Concrete.Stmt (TestCostOperator 20) [] []
          ]
          []
  let cost =
        progCost (PerStmtCostObj TestCost) mempty prog ::
          ConcreteContext Integer
  cost @?= Right 30

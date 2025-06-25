{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Program.Concrete.GenSymSimpleTest
  ( genSymSimpleTest,
  )
where

import Grisette (Solvable (isym), Union, genSymSimple, mrgIf)
import HieraSynth.Program.Choice.ChoiceTree (ChoiceTree (Leaf))
import HieraSynth.Program.Concrete (Prog, buildProg, node1)
import HieraSynth.TestOperator.TestSemanticsOperator
  ( TestSemanticsOp (Add, DivMod, Double, Inc),
    TestSemanticsType (IntType),
  )
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.SymbolicAssertion ((.@?=))

prog ::
  ChoiceTree TestSemanticsOp ->
  ChoiceTree TestSemanticsOp ->
  Prog (ChoiceTree TestSemanticsOp) Integer TestSemanticsType
prog op1 op2 = buildProg [("x", IntType), ("y", IntType)] $ \[x, y] -> do
  r1 <- node1 op1 [x, y]
  r2 <- node1 op2 [x, y]
  return [(r1, IntType), (r2, IntType)]

genSymSimpleTest :: Test
genSymSimpleTest =
  testCase "GenSymSimple" $ do
    let p = prog (Leaf [Add, DivMod]) (Leaf [Inc, Double])
    let p' =
          genSymSimple p "prog" ::
            Prog (Union TestSemanticsOp) Integer TestSemanticsType
    let expected = buildProg [("x", IntType), ("y", IntType)] $ \[x, y] -> do
          r1 <- node1 (mrgIf (isym "prog" 0) (return Add) (return DivMod)) [x, y]
          r2 <- node1 (mrgIf (isym "prog" 1) (return Inc) (return Double)) [x, y]
          return [(r1, IntType), (r2, IntType)]
    p' .@?= expected

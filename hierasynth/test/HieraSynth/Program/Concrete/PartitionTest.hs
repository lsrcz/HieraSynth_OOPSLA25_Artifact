{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Program.Concrete.PartitionTest (partitionTest) where

import HieraSynth.Program.Choice.ChoiceTree
  ( ChoiceMeta (NoSplit, Split),
    ChoiceTree (Branch, Leaf),
  )
import HieraSynth.Program.Choice.Split
  ( LowestSeqNum (lowestSeqNum),
    PartitionSpec (partitionSpec),
  )
import HieraSynth.Program.Concrete (Prog, buildProg, node1)
import HieraSynth.TestOperator.TestSemanticsOperator
  ( TestSemanticsOp (Add, DivMod, Double, Inc),
    TestSemanticsType (IntType),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

prog ::
  ChoiceTree TestSemanticsOp ->
  ChoiceTree TestSemanticsOp ->
  Prog (ChoiceTree TestSemanticsOp) Integer TestSemanticsType
prog op1 op2 = buildProg [("x", IntType), ("y", IntType)] $ \[x, y] -> do
  r1 <- node1 op1 [x, y]
  r2 <- node1 op2 [x, y]
  return [(r1, IntType), (r2, IntType)]

unsplit1 :: ChoiceTree TestSemanticsOp
unsplit1 = Branch (Split 1 True) [Leaf [Add, DivMod], Leaf [Inc, Double]]

unsplit2 :: ChoiceTree TestSemanticsOp
unsplit2 = Branch (Split 2 False) [Leaf [Add, DivMod], Leaf [Inc, Double]]

prog0 :: Prog (ChoiceTree TestSemanticsOp) Integer TestSemanticsType
prog0 = prog unsplit1 unsplit2

split1 :: ChoiceTree TestSemanticsOp
split1 = Branch NoSplit [Leaf [Add, DivMod]]

split2 :: ChoiceTree TestSemanticsOp
split2 = Branch NoSplit [Leaf [Inc, Double]]

prog10 :: Prog (ChoiceTree TestSemanticsOp) Integer TestSemanticsType
prog10 = prog split1 unsplit2

prog11 :: Prog (ChoiceTree TestSemanticsOp) Integer TestSemanticsType
prog11 = prog split2 unsplit2

prog20 :: Prog (ChoiceTree TestSemanticsOp) Integer TestSemanticsType
prog20 = prog unsplit1 split1

prog21 :: Prog (ChoiceTree TestSemanticsOp) Integer TestSemanticsType
prog21 = prog unsplit1 split2

partitionTest :: Test
partitionTest =
  testGroup
    "Partition"
    [ testCase "lowestSeqNum" $ do
        lowestSeqNum True prog0 @?= Just 1
        lowestSeqNum False prog0 @?= Just 2,
      testCase "partitionSpec inexist" $ do
        partitionSpec 0 prog0 @?= [prog0],
      testCase "partitionSpec 1" $ do
        partitionSpec 1 prog0 @?= [prog10, prog11],
      testCase "partitionSpec 2" $ do
        partitionSpec 2 prog0 @?= [prog20, prog21]
    ]

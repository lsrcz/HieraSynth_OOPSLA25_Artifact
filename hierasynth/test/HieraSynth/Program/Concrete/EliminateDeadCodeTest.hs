{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Program.Concrete.EliminateDeadCodeTest
  ( eliminateDeadCodeTest,
  )
where

import HieraSynth.Operator.OpTyping (DefaultType (DefaultType))
import HieraSynth.Program.Concrete
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
    eliminateDeadCode,
  )
import HieraSynth.TestOperator.TestSemanticsOperator
  ( TestSemanticsOp (Add, DivMod, Double, Inc),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

eliminateDeadCodeTest :: Test
eliminateDeadCodeTest =
  testGroup
    "EliminateDeadCode"
    [ testCase "EliminateDeadCode" $ do
        let prog =
              Prog
                [ProgArg "a" (0 :: Int) DefaultType, ProgArg "b" 1 DefaultType]
                [ Stmt Add [0, 1] [2],
                  Stmt DivMod [0, 1] [3, 4],
                  Stmt Inc [3] [5],
                  Stmt Inc [4] [6],
                  Stmt Double [5] [7]
                ]
                [ProgRes 5 DefaultType]
        let actual = eliminateDeadCode prog
        let expected =
              Prog
                [ProgArg "a" (0 :: Int) DefaultType, ProgArg "b" 1 DefaultType]
                [ Stmt DivMod [0, 1] [3, 4],
                  Stmt Inc [3] [5]
                ]
                [ProgRes 5 DefaultType]
        actual @?= expected
    ]

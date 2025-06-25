{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Program.Concrete.FlattenTest (flattenTest) where

import HieraSynth.Program.Concrete
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import HieraSynth.Program.Concrete.Flatten (flattenSymbolTable)
import HieraSynth.Program.ProgTyping (ProgTyping (typeProg))
import HieraSynth.Program.SymbolTable (SymbolTable (SymbolTable))
import HieraSynth.TestOperator.TestPrettyOperator
  ( TestPrettyOp (PrettyInvokeOp, PrettyOp0, PrettyOp1, PrettyOp2),
    TestPrettyType (PrettyType1, PrettyType2),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

flattenTest :: Test
flattenTest =
  testGroup
    "Flatten"
    [ testCase "No sub program" $ do
        let prog =
              Prog
                [ProgArg "a" 20 PrettyType1, ProgArg "b" 10 PrettyType2]
                [ Stmt PrettyOp2 [20, 10] [40, 30 :: Int],
                  Stmt PrettyOp2 [40, 30] [0, 1]
                ]
                [ProgRes 1 PrettyType1, ProgRes 0 PrettyType2]
        let table = SymbolTable [("test", prog)]
        let expected =
              Prog
                [ProgArg "a" 0 PrettyType1, ProgArg "b" 1 PrettyType2]
                [ Stmt PrettyOp2 [0, 1] [2, 3],
                  Stmt PrettyOp2 [2, 3] [4, 5 :: Int]
                ]
                [ProgRes 5 PrettyType1, ProgRes 4 PrettyType2]
        let expectedTable = SymbolTable [("test", expected)]
        flattenSymbolTable table @?= return expectedTable,
      testCase "With sub program" $ do
        let subProg =
              Prog
                [ProgArg "a" 20 PrettyType1, ProgArg "b" 10 PrettyType2]
                [ Stmt PrettyOp2 [20, 10] [40, 30],
                  Stmt PrettyOp2 [40, 30] [0, 1]
                ]
                [ProgRes 1 PrettyType1, ProgRes 0 PrettyType2]
        let subProgSig = typeProg subProg
        let prog =
              Prog
                [ProgArg "b" 4 PrettyType2]
                [ Stmt PrettyOp0 [] [20 :: Int],
                  Stmt (PrettyInvokeOp subProgSig "subProg") [20, 4] [0, 1],
                  Stmt PrettyOp1 [0] [2],
                  Stmt (PrettyInvokeOp subProgSig "subProg") [2, 1] [5, 6]
                ]
                [ProgRes 2 PrettyType1, ProgRes 5 PrettyType2]
        let table = SymbolTable [("subProg", subProg), ("prog", prog)]
        let expectedSubProg =
              Prog
                [ProgArg "a" 0 PrettyType1, ProgArg "b" 1 PrettyType2]
                [ Stmt PrettyOp2 [0, 1] [2, 3],
                  Stmt PrettyOp2 [2, 3] [4, 5 :: Int]
                ]
                [ProgRes 5 PrettyType1, ProgRes 4 PrettyType2]
        let expected =
              Prog
                [ProgArg "b" 0 PrettyType2]
                [ Stmt PrettyOp0 [] [1],
                  Stmt PrettyOp2 [1, 0] [2, 3],
                  Stmt PrettyOp2 [2, 3] [4, 5],
                  Stmt PrettyOp1 [5] [6],
                  Stmt PrettyOp2 [6, 4] [7, 8],
                  Stmt PrettyOp2 [7, 8] [9, 10 :: Int]
                ]
                [ProgRes 6 PrettyType1, ProgRes 10 PrettyType2]
        let expectedTable =
              SymbolTable [("subProg", expectedSubProg), ("prog", expected)]
        flattenSymbolTable table @?= return expectedTable
    ]

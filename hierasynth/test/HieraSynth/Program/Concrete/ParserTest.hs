{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module HieraSynth.Program.Concrete.ParserTest (parserTest) where

import Data.Void (Void)
import HieraSynth.Combinator.Sum (type (:<:) (inj), type (:|))
import HieraSynth.Operator.OpParser (OpParser (opParser))
import HieraSynth.Program.Concrete
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import HieraSynth.Program.ProgParser (progTableParser)
import HieraSynth.Program.SymbolTable (SymbolTable (SymbolTable))
import HieraSynth.TestOperator.TestSemanticsOperator
  ( TestSemanticsOp (Add, Double, Inc),
    TestSemanticsType (IntType),
  )
import HieraSynth.Util.Parser (symbol)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import Text.Megaparsec (parse, try)

data Op1 = Op1 deriving (Show, Eq)

instance OpParser Op1 where
  opParser = try (symbol "op1" >> return Op1)

data Op2 = Op2 deriving (Show, Eq)

instance OpParser Op2 where
  opParser = try (symbol "op2" >> return Op2)

parserTest :: Test
parserTest =
  testGroup
    "Parser"
    [ testGroup
        "Prog"
        [ testGroup
            "Three statements"
            [ testCase "add, inc, double" $ do
                let txt =
                      "def test(x: IntType, y: IntType) -> IntType:"
                        <> "z = add(x, y)"
                        <> "w = inc(z)"
                        <> "h = double(w)"
                        <> "return h" ::
                        String
                let prog =
                      Prog
                        [ProgArg "x" 0 IntType, ProgArg "y" 1 IntType]
                        [ Stmt Add [0, 1] [2],
                          Stmt Inc [2] [3],
                          Stmt Double [3] [4]
                        ]
                        [ProgRes 4 IntType]
                parse (progTableParser @Void) "test" txt
                  @?= Right (SymbolTable [("test", prog)]),
              testCase "sum prog" $ do
                let txt =
                      "def test(x: IntType, y: IntType) -> IntType:"
                        <> "z = op1(x, y)"
                        <> "w = op2(z)"
                        <> "return w" ::
                        String
                let prog =
                      Prog
                        [ProgArg "x" 0 IntType, ProgArg "y" 1 IntType]
                        [ Stmt (inj Op1) [0, 1] [2],
                          Stmt (inj Op2) [2] [3]
                        ]
                        [ProgRes 3 IntType] ::
                        Prog (Op1 :| Op2) Integer TestSemanticsType
                parse (progTableParser @Void) "test" txt
                  @?= Right (SymbolTable [("test", prog)])
            ]
        ]
    ]

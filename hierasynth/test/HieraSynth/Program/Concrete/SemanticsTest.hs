{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Program.Concrete.SemanticsTest (semanticsTest) where

import Control.Monad.Error.Class (liftEither)
import qualified Data.Text as T
import Grisette (SymInteger, ToSym (toSym))
import HieraSynth.Context (ConcreteContext, SymbolicContext)
import HieraSynth.Program.Concrete
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgMayMultiPath (ProgMayMultiPath),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import HieraSynth.Program.ProgSemantics (ProgSemantics (runProg))
import HieraSynth.TestOperator.TestSemanticsOperator
  ( TestSemanticsObj (TestSemanticsObj),
    TestSemanticsOp (Add, DivMod),
    TestSemanticsType (IntType),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import Test.SymbolicAssertion ((.@?=))

data SemanticsTestCase = SemanticsTestCase
  { semanticsTestCaseName :: String,
    semanticsTestCaseProg :: Prog TestSemanticsOp Integer TestSemanticsType,
    semanticsTestCaseArgs :: [Integer],
    semanticsTestCaseExpected :: Either T.Text [Integer]
  }

semanticsTest :: Test
semanticsTest =
  testGroup "semantics" $ do
    let goodProg =
          Prog
            [ProgArg "x" 0 IntType, ProgArg "y" 1 IntType]
            [ Stmt Add [0, 1] [3],
              Stmt DivMod [3, 0] [4, 5]
            ]
            [ProgRes 4 IntType, ProgRes 5 IntType] ::
            Prog TestSemanticsOp Integer TestSemanticsType
    SemanticsTestCase name prog args expected <-
      [ SemanticsTestCase
          { semanticsTestCaseName = "runProg",
            semanticsTestCaseProg = goodProg,
            semanticsTestCaseArgs = [13, 20],
            semanticsTestCaseExpected = Right [2, 7]
          },
        SemanticsTestCase
          { semanticsTestCaseName = "divByZero",
            semanticsTestCaseProg = goodProg,
            semanticsTestCaseArgs = [0, 20],
            semanticsTestCaseExpected = Left "ArithException: divide by zero"
          },
        SemanticsTestCase
          { semanticsTestCaseName = "incorrect number of arguments",
            semanticsTestCaseProg = goodProg,
            semanticsTestCaseArgs = [0],
            semanticsTestCaseExpected =
              Left "Expected 2 arguments, but got 1 arguments."
          },
        SemanticsTestCase
          { semanticsTestCaseName = "incorrect number of statement results",
            semanticsTestCaseProg =
              Prog
                [ProgArg "x" 0 IntType, ProgArg "y" 1 IntType]
                [ Stmt Add [0, 1] [2, 3]
                ]
                [ProgRes 2 IntType],
            semanticsTestCaseArgs = [1, 2],
            semanticsTestCaseExpected = Left "Incorrect number of results."
          },
        SemanticsTestCase
          { semanticsTestCaseName = "Redefinition of variable",
            semanticsTestCaseProg =
              Prog
                [ProgArg "x" 0 IntType, ProgArg "x" 1 IntType]
                [Stmt Add [0, 1] [1]]
                [ProgRes 1 IntType] ::
                Prog TestSemanticsOp Integer TestSemanticsType,
            semanticsTestCaseArgs = [1, 2],
            semanticsTestCaseExpected = Left "Variable 1 is already defined."
          },
        SemanticsTestCase
          { semanticsTestCaseName = "Undefined variable",
            semanticsTestCaseProg =
              Prog
                [ProgArg "x" 0 IntType]
                [Stmt Add [0, 1] [2]]
                [ProgRes 2 IntType] ::
                Prog TestSemanticsOp Integer TestSemanticsType,
            semanticsTestCaseArgs = [1],
            semanticsTestCaseExpected = Left "Variable 1 is undefined."
          },
        SemanticsTestCase
          { semanticsTestCaseName = "Undefined result",
            semanticsTestCaseProg =
              Prog
                [ProgArg "x" 0 IntType, ProgArg "y" 1 IntType]
                [Stmt Add [0, 1] [2]]
                [ProgRes 3 IntType] ::
                Prog TestSemanticsOp Integer TestSemanticsType,
            semanticsTestCaseArgs = [1, 2],
            semanticsTestCaseExpected = Left "Variable 3 is undefined."
          }
      ]
    [ testCase name $ do
        let actual =
              runProg
                TestSemanticsObj
                mempty
                prog
                args ::
                ConcreteContext [Integer]
        actual @?= expected,
      testCase (name <> "-ProgMayMultiPath") $ do
        let actual =
              runProg
                TestSemanticsObj
                mempty
                (ProgMayMultiPath prog)
                (toSym args) ::
                SymbolicContext [SymInteger]
        actual .@?= toSym (liftEither expected :: SymbolicContext [Integer])
      ]

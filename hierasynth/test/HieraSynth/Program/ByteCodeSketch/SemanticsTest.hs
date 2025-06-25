{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Program.ByteCodeSketch.SemanticsTest
  ( goodConcreteProg,
    semanticsTest,
  )
where

import Control.Monad.Error.Class (MonadError (catchError))
import qualified Data.Text as T
import Grisette
  ( ITEOp (symIte),
    LogicalOp ((.||)),
    Solvable (con),
    SymBool,
    SymEq ((.==)),
    SymInteger,
    mrgIf,
    mrgReturn,
  )
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import HieraSynth.Context (SymbolicContext)
import HieraSynth.Program.ByteCodeSketch
  ( Prog (Prog),
    ProgArg (ProgArg),
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
import Test.SymbolicAssertion ((.@?=))

data ExpectedResult
  = ErrorResult T.Text
  | Result SymBool [SymInteger]

data SemanticsTestCase = SemanticsTestCase
  { semanticsTestCaseName :: String,
    semanticsTestCaseProg ::
      Prog TestSemanticsOp Integer SymInteger TestSemanticsType,
    semanticsTestCaseArgs :: [SymInteger],
    semanticsTestCaseExpected :: ExpectedResult
  }

goodConcreteProg :: Prog TestSemanticsOp Integer SymInteger TestSemanticsType
goodConcreteProg =
  Prog
    [ProgArg "x" 0 IntType, ProgArg "y" 1 IntType]
    [ Stmt Add [0, 1] 2 [3] 1,
      Stmt DivMod [3, 0] 2 [4, 5] 2
    ]
    [ProgRes 4 IntType, ProgRes 5 IntType]

semanticsTest :: Test
semanticsTest = testGroup "Semantics" $ do
  SemanticsTestCase name prog args expected <-
    [ SemanticsTestCase
        { semanticsTestCaseName = "concrete program",
          semanticsTestCaseProg = goodConcreteProg,
          semanticsTestCaseArgs = [13, 20],
          semanticsTestCaseExpected = Result (con True) [2, 7]
        },
      SemanticsTestCase
        { semanticsTestCaseName =
            "symbolic number of statement arguments",
          semanticsTestCaseProg =
            Prog
              [ProgArg "x" 0 IntType, ProgArg "y" 1 IntType]
              [Stmt Add [0, 1] "a" [2] 1]
              [ProgRes 2 IntType],
          semanticsTestCaseArgs = [13, 20],
          semanticsTestCaseExpected =
            Result (("a" :: SymInteger) .== 2) [33]
        },
      SemanticsTestCase
        { semanticsTestCaseName = "symbolic statement argument",
          semanticsTestCaseProg =
            Prog
              [ProgArg "x" 0 IntType, ProgArg "y" 1 IntType]
              [Stmt Add ["a", 1] 2 [2] 1]
              [ProgRes 2 IntType],
          semanticsTestCaseArgs = [13, 20],
          semanticsTestCaseExpected =
            let a = "a" :: SymInteger
             in Result (a .== 0 .|| a .== 1) [symIte (a .== 0) 33 40]
        },
      SemanticsTestCase
        { semanticsTestCaseName =
            "symbolic number of statement results",
          semanticsTestCaseProg =
            Prog
              [ProgArg "x" 0 IntType, ProgArg "y" 1 IntType]
              [Stmt Add [0, 1] 2 [2] "a"]
              [ProgRes 2 IntType],
          semanticsTestCaseArgs = [13, 20],
          semanticsTestCaseExpected =
            Result (("a" :: SymInteger) .== 1) [33]
        },
      SemanticsTestCase
        { semanticsTestCaseName = "symbolic result",
          semanticsTestCaseProg =
            Prog
              [ProgArg "x" 0 IntType, ProgArg "y" 1 IntType]
              [Stmt Add [0, 1] 2 [2] 1]
              [ProgRes "a" IntType],
          semanticsTestCaseArgs = [13, 20],
          semanticsTestCaseExpected =
            let a = "a" :: SymInteger
             in Result
                  (a .== 0 .|| a .== 1 .|| a .== 2)
                  [symIte (a .== 0) 13 $ symIte (a .== 1) 20 33]
        },
      SemanticsTestCase
        { semanticsTestCaseName = "divByZero",
          semanticsTestCaseProg = goodConcreteProg,
          semanticsTestCaseArgs = [0, 20],
          semanticsTestCaseExpected =
            ErrorResult "ArithException: divide by zero"
        },
      SemanticsTestCase
        { semanticsTestCaseName = "incorrect number of arguments",
          semanticsTestCaseProg = goodConcreteProg,
          semanticsTestCaseArgs = [0],
          semanticsTestCaseExpected =
            ErrorResult "Expected 2 arguments, but got 1 arguments."
        },
      SemanticsTestCase
        { semanticsTestCaseName = "incorrect number of statement results",
          semanticsTestCaseProg =
            Prog
              [ProgArg "x" 0 IntType, ProgArg "y" 1 IntType]
              [Stmt Add [0, 1] 2 [2, 3] 2]
              [ProgRes 2 IntType],
          semanticsTestCaseArgs = [1, 2],
          semanticsTestCaseExpected =
            ErrorResult "Incorrect number of results."
        },
      SemanticsTestCase
        { semanticsTestCaseName =
            "incorrect number of statement arguments",
          semanticsTestCaseProg =
            Prog
              [ProgArg "x" 0 IntType, ProgArg "y" 1 IntType]
              [Stmt Add [0] 1 [2] 1]
              [ProgRes 2 IntType],
          semanticsTestCaseArgs = [1, 2],
          semanticsTestCaseExpected =
            ErrorResult $
              "Incorrect number of arguments for add, expected 2 "
                <> "arguments, but got 1 arguments."
        },
      SemanticsTestCase
        { semanticsTestCaseName = "Redefinition of variable",
          semanticsTestCaseProg =
            Prog
              [ProgArg "x" 0 IntType, ProgArg "x" 1 IntType]
              [Stmt Add [0, 1] 2 [1] 1]
              [ProgRes 1 IntType],
          semanticsTestCaseArgs = [1, 2],
          semanticsTestCaseExpected =
            ErrorResult "Variable 1 is already defined."
        },
      SemanticsTestCase
        { semanticsTestCaseName = "Undefined variable",
          semanticsTestCaseProg =
            Prog
              [ProgArg "x" 0 IntType]
              [Stmt Add [0, 1] 2 [2] 1]
              [ProgRes 2 IntType],
          semanticsTestCaseArgs = [1],
          semanticsTestCaseExpected =
            ErrorResult "Variable is undefined."
        },
      SemanticsTestCase
        { semanticsTestCaseName = "Undefined result",
          semanticsTestCaseProg =
            Prog
              [ProgArg "x" 0 IntType, ProgArg "y" 1 IntType]
              [Stmt Add [0, 1] 2 [2] 1]
              [ProgRes 3 IntType],
          semanticsTestCaseArgs = [1, 2],
          semanticsTestCaseExpected =
            ErrorResult "Variable is undefined."
        },
      SemanticsTestCase
        { semanticsTestCaseName = "Extra statement arg num",
          semanticsTestCaseProg =
            Prog
              [ProgArg "x" 0 IntType, ProgArg "y" 1 IntType]
              [Stmt Add [0, 1, 1] 2 [2] 1]
              [ProgRes 2 IntType],
          semanticsTestCaseArgs = [1, 2],
          semanticsTestCaseExpected = Result (con True) [3]
        },
      SemanticsTestCase
        { semanticsTestCaseName = "Insufficient statement args",
          semanticsTestCaseProg =
            Prog
              [ProgArg "x" 0 IntType, ProgArg "y" 1 IntType]
              [Stmt Add [0] 2 [2] 1]
              [ProgRes 2 IntType],
          semanticsTestCaseArgs = [1, 2],
          semanticsTestCaseExpected =
            ErrorResult "The specified argument number is too large."
        },
      SemanticsTestCase
        { semanticsTestCaseName = "Insufficient result ids",
          semanticsTestCaseProg =
            Prog
              [ProgArg "x" 0 IntType, ProgArg "y" 1 IntType]
              [Stmt DivMod [0, 1] 2 [2] 2]
              [ProgRes 2 IntType],
          semanticsTestCaseArgs = [1, 2],
          semanticsTestCaseExpected =
            ErrorResult "Insufficient result IDs."
        }
    ]
  return $ testCase name $ do
    let actual =
          runProg TestSemanticsObj mempty prog args ::
            SymbolicContext [SymInteger]
    case expected of
      ErrorResult expectedError -> actual .@?= mrgThrowError expectedError
      Result preCond expectedIntegers -> do
        let processedActual =
              actual `catchError` const (mrgThrowError "Error")
        let expected =
              mrgIf preCond (mrgReturn expectedIntegers) (mrgThrowError "Error")
        processedActual .@?= expected

{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Program.ComponentSketch.ToConTest (toConTest) where

import Grisette
  ( Solvable (con),
    SymInteger,
    ToCon (toCon),
    Union,
    mrgReturn,
  )
import HieraSynth.Program.ComponentSketch
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import HieraSynth.Program.ComponentSketch.TestProgram (goodConcreteProg)
import qualified HieraSynth.Program.Concrete as Concrete
import HieraSynth.TestOperator.TestSemanticsOperator
  ( TestSemanticsOp (Add, DivMod),
    TestSemanticsType (IntType),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

data ToConTestCase = ToConTestCase
  { toConTestCaseName :: String,
    toConTestCaseProg ::
      Prog (Union TestSemanticsOp) SymInteger TestSemanticsType,
    toConTestCaseExpected ::
      Maybe (Concrete.Prog TestSemanticsOp Integer TestSemanticsType)
  }

toConTest :: Test
toConTest = testGroup "ToCon" $ do
  ToConTestCase name prog expected <-
    [ ToConTestCase
        { toConTestCaseName = "goodConcreteProg",
          toConTestCaseProg = goodConcreteProg,
          toConTestCaseExpected =
            Just $
              Concrete.Prog
                [ Concrete.ProgArg "x" 0 IntType,
                  Concrete.ProgArg "y" 1 IntType
                ]
                [ Concrete.Stmt Add [0, 1] [2],
                  Concrete.Stmt DivMod [2, 0] [3, 4]
                ]
                [Concrete.ProgRes 3 IntType, Concrete.ProgRes 4 IntType]
        },
      ToConTestCase
        { toConTestCaseName = "reorder",
          toConTestCaseProg =
            Prog
              [ProgArg "x" IntType, ProgArg "y" IntType]
              [ Stmt (mrgReturn DivMod) [2, 0] 2 [3, 4] 2 (con False) [],
                Stmt (mrgReturn Add) [0, 1] 2 [2] 1 (con False) []
              ]
              [ProgRes 3 IntType, ProgRes 4 IntType],
          toConTestCaseExpected =
            Just $
              Concrete.Prog
                [ Concrete.ProgArg "x" 0 IntType,
                  Concrete.ProgArg "y" 1 IntType
                ]
                [ Concrete.Stmt Add [0, 1] [2],
                  Concrete.Stmt DivMod [2, 0] [3, 4]
                ]
                [Concrete.ProgRes 3 IntType, Concrete.ProgRes 4 IntType]
        },
      ToConTestCase
        { toConTestCaseName = "shrink by arg/res num",
          toConTestCaseProg =
            Prog
              [ProgArg "x" IntType, ProgArg "y" IntType]
              [ Stmt (mrgReturn DivMod) [2, 0, 3] 2 [4, 5, 6] 2 (con False) [],
                Stmt (mrgReturn Add) [0, 1, 0] 2 [2, 3] 1 (con False) []
              ]
              [ProgRes 4 IntType, ProgRes 5 IntType],
          toConTestCaseExpected =
            Just $
              Concrete.Prog
                [ Concrete.ProgArg "x" 0 IntType,
                  Concrete.ProgArg "y" 1 IntType
                ]
                [ Concrete.Stmt Add [0, 1] [2],
                  Concrete.Stmt DivMod [2, 0] [4, 5]
                ]
                [Concrete.ProgRes 4 IntType, Concrete.ProgRes 5 IntType]
        }
    ]
  return $ testCase name $ toCon prog @?= expected

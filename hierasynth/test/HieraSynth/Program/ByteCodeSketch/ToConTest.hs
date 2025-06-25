{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Program.ByteCodeSketch.ToConTest
  ( toConTest,
  )
where

import Grisette (SymInteger, ToCon (toCon))
import HieraSynth.Program.ByteCodeSketch
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import qualified HieraSynth.Program.Concrete.Program as Concrete
import HieraSynth.TestOperator.TestSemanticsOperator
  ( TestSemanticsOp (Add, DivMod, Inc),
    TestSemanticsType (IntType),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

data ToConTestCase = ToConTestCase
  { toConTestCaseName :: String,
    toConTestCaseProg ::
      Prog TestSemanticsOp Integer SymInteger TestSemanticsType,
    toConTestCaseExpected ::
      Maybe (Concrete.Prog TestSemanticsOp Integer TestSemanticsType)
  }

toConTest :: Test
toConTest = testGroup "ToCon" $ do
  ToConTestCase name prog expected <-
    [ ToConTestCase
        { toConTestCaseName = "goodConcreteProg",
          toConTestCaseProg =
            Prog
              [ProgArg "x" 0 IntType, ProgArg "y" 1 IntType]
              [ Stmt Add [0, 1] 2 [3] 1,
                Stmt DivMod [3, 0] 2 [4, 5] 2
              ]
              [ProgRes 4 IntType, ProgRes 5 IntType],
          toConTestCaseExpected =
            Just $
              Concrete.Prog
                [ Concrete.ProgArg "x" 0 IntType,
                  Concrete.ProgArg "y" 1 IntType
                ]
                [ Concrete.Stmt Add [0, 1] [3],
                  Concrete.Stmt DivMod [3, 0] [4, 5]
                ]
                [Concrete.ProgRes 4 IntType, Concrete.ProgRes 5 IntType]
        },
      ToConTestCase
        { toConTestCaseName = "argNum is less than number of args",
          toConTestCaseProg =
            Prog
              [ProgArg "x" 0 IntType, ProgArg "y" 1 IntType]
              [ Stmt Inc [0, 1] 1 [2, 3] 1
              ]
              [ProgRes 2 IntType],
          toConTestCaseExpected =
            Just $
              Concrete.Prog
                [ Concrete.ProgArg "x" 0 IntType,
                  Concrete.ProgArg "y" 1 IntType
                ]
                [Concrete.Stmt Inc [0] [2]]
                [Concrete.ProgRes 2 IntType]
        }
    ]
  return $ testCase name $ toCon prog @?= expected

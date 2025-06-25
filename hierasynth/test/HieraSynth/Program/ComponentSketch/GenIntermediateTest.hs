{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Program.ComponentSketch.GenIntermediateTest
  ( genIntermediateTest,
  )
where

import Data.Proxy (Proxy (Proxy))
import Grisette
  ( Solvable (isym),
    SymInteger,
    mrgReturn,
    runFreshT,
  )
import HieraSynth.Context (SymbolicContext)
import HieraSynth.Operator.OpTyping (OpTyping (typeOp))
import HieraSynth.Program.ComponentSketch
  ( Intermediates (Intermediates),
    genIntermediates,
    genOpIntermediates,
  )
import HieraSynth.TestOperator.TestSemanticsOperator
  ( TestSemanticsObj (TestSemanticsObj),
    TestSemanticsOp (DivMod),
    TestSemanticsType (IntType),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.SymbolicAssertion ((.@?=))

genIntermediateTest :: Test
genIntermediateTest =
  testGroup
    "HieraSynth.Program.ComponentSketch.GenIntermediateTest"
    [ testCase "genIntermediates" $ do
        let actual =
              flip runFreshT "x" $
                genIntermediates TestSemanticsObj [IntType, IntType] ::
                SymbolicContext [SymInteger]
        let expected = mrgReturn [isym "x" 0, isym "x" 1]
        actual .@?= expected,
      testCase "genOpIntermediates" $ do
        let actual =
              flip runFreshT "x" $
                typeOp DivMod
                  >>= genOpIntermediates
                    (Proxy :: Proxy TestSemanticsType)
                    TestSemanticsObj ::
                SymbolicContext (Intermediates SymInteger)
        let expected =
              mrgReturn $
                Intermediates [isym "x" 0, isym "x" 1] [isym "x" 2, isym "x" 3]
        actual .@?= expected
    ]

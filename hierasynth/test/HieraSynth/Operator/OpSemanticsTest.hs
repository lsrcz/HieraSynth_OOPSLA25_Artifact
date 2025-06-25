{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Operator.OpSemanticsTest (opSemanticsTest) where

import Control.Monad.Error.Class (MonadError (throwError))
import Grisette (LogicalOp ((.||)), SymInteger, Union, mrgIf, mrgReturn)
import Grisette.Lib.Control.Monad.Except (mrgModifyError)
import HieraSynth.Context (SymbolicContext)
import HieraSynth.Operator.OpSemantics (OpSemantics (applyOp))
import HieraSynth.TestOperator.TestSemanticsOperator
  ( TestSemanticsObj (TestSemanticsObj),
    TestSemanticsOp (Add, DivMod, Double, Inc),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.SymbolicAssertion ((.@?=))

opSemanticsTest :: Test
opSemanticsTest =
  testGroup
    "HieraSynth.Operator.OpSemantics"
    [ testCase "Union OpSemantics" $ do
        let op =
              mrgIf "a" (return Add) $
                mrgIf "b" (return DivMod) $
                  mrgIf "c" (mrgReturn Inc) (mrgReturn Double) ::
                Union TestSemanticsOp
        let actual1 =
              mrgModifyError (const "Err") $
                applyOp TestSemanticsObj mempty op [2] ::
                SymbolicContext [SymInteger]
        let expected1 =
              mrgIf ("a" .|| "b") (throwError "Err") $
                mrgIf "c" (mrgReturn [3]) (mrgReturn [4])
        actual1 .@?= expected1
        let actual2 =
              mrgModifyError (const "Err") $
                applyOp TestSemanticsObj mempty op [5, 3] ::
                SymbolicContext [SymInteger]
        let expected2 =
              mrgIf "a" (return [8]) $
                mrgIf "b" (return [1, 2]) $
                  throwError "Err"
        actual2 .@?= expected2
    ]

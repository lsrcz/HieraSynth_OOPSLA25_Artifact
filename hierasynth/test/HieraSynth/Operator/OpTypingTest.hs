{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Operator.OpTypingTest (opTypingTest) where

import Grisette (Union, mrgIf, mrgReturn)
import HieraSynth.Context (SymbolicContext)
import HieraSynth.Operator.OpTyping
  ( OpTyping (typeOp),
    symOpMaximumArgNum,
    symOpMaximumResNum,
  )
import HieraSynth.TestOperator.TestSemanticsOperator
  ( TestSemanticsOp (Add, DivMod, Inc),
    TestSemanticsType (IntType),
  )
import HieraSynth.TypeSignature (TypeSignature (TypeSignature))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import Test.SymbolicAssertion ((.@?=))

opTypingTest :: Test
opTypingTest =
  testGroup
    "HieraSynth.Operator.OpTyping"
    [ testCase "Default OpTyping" $ do
        let expected =
              Right $ TypeSignature [IntType, IntType] [IntType]
        typeOp Add @?= expected,
      testCase "Union OpTyping" $ do
        let op =
              mrgIf "a" (mrgReturn Add) (mrgReturn DivMod) ::
                Union TestSemanticsOp
        let actual =
              typeOp op ::
                SymbolicContext (TypeSignature TestSemanticsType)
        let expected =
              mrgIf
                "a"
                (mrgReturn $ TypeSignature [IntType, IntType] [IntType])
                ( mrgReturn $
                    TypeSignature [IntType, IntType] [IntType, IntType]
                )
        actual .@?= expected,
      testCase "Default SymOpLimits" $ do
        symOpMaximumArgNum Add @?= 2
        symOpMaximumResNum Add @?= 1,
      testCase "Union SymOpLimits" $ do
        symOpMaximumArgNum
          ( mrgIf "a" (mrgReturn Add) (mrgReturn Inc) ::
              Union TestSemanticsOp
          )
          @?= 2
        symOpMaximumResNum
          ( mrgIf "a" (mrgReturn Add) (mrgReturn DivMod) ::
              Union TestSemanticsOp
          )
          @?= 2
    ]

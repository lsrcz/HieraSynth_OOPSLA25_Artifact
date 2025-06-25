{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Program.Concrete.TypingTest (typingTest) where

import HieraSynth.Program.Concrete
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import HieraSynth.Program.ProgTyping (ProgTyping (typeProg))
import HieraSynth.TestOperator.TestSemanticsOperator
  ( TestSemanticsOp (Add, DivMod),
    TestSemanticsType (IntType),
  )
import HieraSynth.TypeSignature (TypeSignature (TypeSignature))
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

typingTest :: Test
typingTest = testCase "typing" $ do
  let prog =
        Prog
          [ProgArg "x" 0 IntType, ProgArg "y" 1 IntType]
          [ Stmt Add [0, 1] [3],
            Stmt DivMod [3, 0] [4, 5]
          ]
          [ProgRes 4 IntType, ProgRes 5 IntType] ::
          Prog TestSemanticsOp Integer TestSemanticsType
  typeProg prog @?= TypeSignature [IntType, IntType] [IntType, IntType]

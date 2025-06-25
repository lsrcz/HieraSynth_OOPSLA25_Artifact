{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Program.ComponentSketch.TypingTest (typingTest) where

import Grisette
  ( SymInteger,
    Union,
    mrgReturn,
  )
import HieraSynth.Program.ComponentSketch
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
import HieraSynth.TypeSignature
  ( TypeSignature (TypeSignature),
  )
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

typingTest :: Test
typingTest = testCase "Typing" $ do
  let prog =
        Prog
          [ProgArg "x" IntType, ProgArg "y" IntType]
          [ Stmt (mrgReturn Add) ["a", "b"] "c" ["d"] "e" "f" [],
            Stmt (mrgReturn DivMod) ["g", "h"] "i" ["j", "k"] "l" "m" []
          ]
          [ProgRes 4 IntType, ProgRes 5 IntType] ::
          Prog (Union TestSemanticsOp) SymInteger TestSemanticsType
  typeProg prog @?= TypeSignature [IntType, IntType] [IntType, IntType]

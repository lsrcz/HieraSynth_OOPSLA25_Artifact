{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Program.ByteCodeSketch.TypingTest
  ( typingTest,
  )
where

import Grisette (SymInteger)
import HieraSynth.Program.ByteCodeSketch
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
typingTest = testCase "Typing" $ do
  let prog =
        Prog
          [ProgArg "x" 0 IntType, ProgArg "y" 1 IntType]
          [ Stmt Add [0, 1] 2 [3] 1,
            Stmt DivMod [3, 0] 2 [4, 5] 2
          ]
          [ProgRes 4 IntType, ProgRes 5 IntType] ::
          Prog TestSemanticsOp Integer SymInteger TestSemanticsType
  typeProg prog @?= TypeSignature [IntType, IntType] [IntType, IntType]

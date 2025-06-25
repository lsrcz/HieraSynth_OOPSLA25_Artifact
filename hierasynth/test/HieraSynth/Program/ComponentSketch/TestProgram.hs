{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Program.ComponentSketch.TestProgram
  ( goodConcreteProg,
  )
where

import Grisette (Solvable (con), SymInteger, Union, mrgReturn)
import HieraSynth.Program.ComponentSketch
  ( Prog (Prog),
    ProgArg (ProgArg),
    ProgRes (ProgRes),
    Stmt (Stmt),
  )
import HieraSynth.TestOperator.TestSemanticsOperator
  ( TestSemanticsOp (Add, DivMod),
    TestSemanticsType (IntType),
  )

goodConcreteProg :: Prog (Union TestSemanticsOp) SymInteger TestSemanticsType
goodConcreteProg =
  Prog
    [ProgArg "x" IntType, ProgArg "y" IntType]
    [ Stmt (mrgReturn Add) [0, 1] 2 [2] 1 (con False) [],
      Stmt (mrgReturn DivMod) [2, 0] 2 [3, 4] 2 (con False) []
    ]
    [ProgRes 3 IntType, ProgRes 4 IntType]

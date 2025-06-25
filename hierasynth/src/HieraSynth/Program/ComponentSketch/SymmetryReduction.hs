module HieraSynth.Program.ComponentSketch.SymmetryReduction
  ( OpSymmetryReduction (..),
  )
where

import Grisette (SymBool, Union, mrgReturn, simpleMerge)

class OpSymmetryReduction op where
  opUnreorderable :: op -> op -> SymBool
  opCommutativeArgPos :: op -> Union [[Int]]
  opCommutativeArgPos = const $ mrgReturn []

instance (OpSymmetryReduction op) => OpSymmetryReduction (Union op) where
  opUnreorderable op1 op2 = simpleMerge $ do
    o1 <- op1
    opUnreorderable o1 <$> op2
  opCommutativeArgPos op = do
    op1 <- op
    opCommutativeArgPos op1

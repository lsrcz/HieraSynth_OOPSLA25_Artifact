{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}

module RVV.Synthesizer.Operator.Common.OpSymmetryReduction
  ( opUnreorderableByType,
    checkOpCommutativeArgPos,
  )
where

import Control.Monad.Except (runExceptT)
import Grisette
  ( LogicalOp (false),
    Mergeable,
    Solvable (con),
    SymBool,
    Union,
    mrgReturn,
    simpleMerge,
  )
import HieraSynth.Context (SymbolicContext)
import HieraSynth.Operator.OpTyping (OpTyping (OpTypeType, typeOp))
import HieraSynth.TypeSignature (TypeSignature (TypeSignature))
import Grisette.Unified (GetData, extractData)
import RVV.EvalMode (MonadEvalMode)
import RVV.Synthesizer.Type (ValueType, isMemType, isVLType)

opUnreorderableByType ::
  (OpTypeType op ~ ValueType, OpTyping op SymbolicContext) =>
  op ->
  op ->
  SymBool
opUnreorderableByType l r = simpleMerge $ do
  r <- runExceptT $ do
    TypeSignature op1Args _ <- typeOp l
    TypeSignature _ op2Ress <- typeOp r
    return $
      con $
        (any isVLType op1Args && any isVLType op2Ress)
          || (any isMemType op1Args && any isMemType op2Ress)
  case r of
    Left _ -> return false
    Right v -> return v :: Union SymBool

checkOpCommutativeArgPos ::
  (Eq op, MonadEvalMode mode Union, Mergeable op) =>
  GetData mode op ->
  [op] ->
  [[Int]] ->
  Union [[Int]]
checkOpCommutativeArgPos op commutativeOp pos = do
  op <- extractData op
  if op `elem` commutativeOp
    then mrgReturn pos
    else mrgReturn []

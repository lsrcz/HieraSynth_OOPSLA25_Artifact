{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Operator.AddImm (AddImm (..), addImm) where

import qualified Data.HashSet as HS
import Data.List ((\\))
import EvalMode (EvalMode, MonadEvalContext, deriveWithEvalMode)
import Grisette
  ( LogicalOp (false),
    PPrint (pformat),
    ToSym (toSym),
    allClasses0,
    mrgReturn,
    pprintClasses,
  )
import HieraSynth.Combinator.Sum (type (:<:) (inj))
import HieraSynth.Operator.OpReachableSymbols
  ( OpReachableSymbols (opReachableSymbols),
  )
import HieraSynth.Operator.OpSemantics
  ( DefaultSem,
    OpSemantics (applyOp),
    pureUnaryOp,
  )
import HieraSynth.Operator.OpTyping
  ( DefaultType,
    OpTyping (OpTypeType, typeOp),
    simpleTyping,
    unaryDefaultType,
  )
import HieraSynth.Program.ComponentSketch
  ( OpSymmetryReduction (opCommutativeArgPos, opUnreorderable),
  )
import HieraSynth.Program.Concrete (OpPPrint (describeArguments))
import Grisette.Unified
  ( EvalModeConvertible (withModeConvertible),
    EvalModeTag (C),
    GetInteger,
  )

newtype AddImm mode = AddImm (GetInteger mode)

addImm :: (AddImm mode :<: op) => GetInteger mode -> op
addImm = inj . AddImm

deriveWithEvalMode [''AddImm] (allClasses0 \\ pprintClasses)

instance (MonadEvalContext mode ctx) => OpTyping (AddImm mode) ctx where
  type OpTypeType (AddImm mode) = DefaultType
  typeOp = simpleTyping $ const unaryDefaultType

instance
  ( MonadEvalContext mode ctx,
    i ~ GetInteger mode1,
    EvalMode mode1,
    EvalModeConvertible mode mode1
  ) =>
  OpSemantics DefaultSem (AddImm mode) i ctx
  where
  applyOp _ _ (AddImm i) =
    pureUnaryOp "Add" (+ withModeConvertible @mode @mode1 (toSym i) (toSym i))

instance PPrint (AddImm 'C) where
  pformat (AddImm v) = "add[imm=" <> pformat v <> "]"

instance OpPPrint (AddImm 'C) where
  describeArguments (AddImm _) = Right [Just "lhs"]

instance OpSymmetryReduction (AddImm mode) where
  opUnreorderable _ _ = false
  opCommutativeArgPos (AddImm _) = mrgReturn []

instance OpReachableSymbols (AddImm mode) where
  opReachableSymbols _ = HS.empty

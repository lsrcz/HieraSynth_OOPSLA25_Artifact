{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Arith (OpCode (..)) where

import qualified Data.HashSet as HS
import Grisette
  ( EvalSym,
    LogicalOp (false),
    Mergeable,
    PPrint (pformat),
    SymEq,
    derive,
    mrgReturn,
  )
import HieraSynth.Context (MonadContext)
import HieraSynth.Operator.OpReachableSymbols
  ( OpReachableSymbols (opReachableSymbols),
  )
import HieraSynth.Operator.OpSemantics
  ( DefaultSem,
    OpSemantics (applyOp),
    pureBinaryOp,
    pureUnaryOp,
  )
import HieraSynth.Operator.OpTyping
  ( DefaultType,
    OpTyping (OpTypeType, typeOp),
    binaryDefaultType,
    simpleTyping,
    unaryDefaultType,
  )
import HieraSynth.Program.ComponentSketch
  ( OpSymmetryReduction (opCommutativeArgPos),
  )
import HieraSynth.Program.ComponentSketch.SymmetryReduction
  ( OpSymmetryReduction (opUnreorderable),
  )
import HieraSynth.Program.Concrete (OpPPrint (describeArguments))

-- * Operators

-- | The operators in this example are simple arithmetic operators.
data OpCode
  = Plus
  | Mul
  | Minus
  | UMinus

derive [''OpCode] [''EvalSym, ''Show, ''Mergeable, ''SymEq]

-- * Semantics and typing.

-- | The semantics of the operators are given with the 'OpSemantics' type class.
-- The ctx here is the context for the evaluation. It is a monadic context that
-- supports error handling and may support merging of multiple paths and fresh
-- variable generation.
--
-- Grisette provide several contexts. For example, the 'ConcreteContext' is for
-- running a program concretely, while an 'AngelicContext' is for running a
-- program symbolically, with the introduction of angelic variables. To use the
-- component encoding for synthesis, you need to use the 'AngelicContext'.
--
-- The 'DefaultSem' is a predefined semantics object. If you don't need to
-- parameterize the semantics, you can use this type. It is also used by the
-- component encoding to generate intermediate values. With 'DefaultSem' and
-- 'DefaultType' (see below), the system is able to resolve whether it can
-- generate intermediate values based on a reasonable default behavior.
instance
  (MonadContext ctx, Num a, Mergeable a) =>
  OpSemantics DefaultSem OpCode a ctx
  where
  applyOp _ _ Plus = pureBinaryOp "Plus" (+)
  applyOp _ _ Mul = pureBinaryOp "Mul" (*)
  applyOp _ _ Minus = pureBinaryOp "Minus" (-)
  applyOp _ _ UMinus = pureUnaryOp "UMinus" negate

-- | The component encoding
-- (https://ieeexplore.ieee.org/abstract/document/6062089) needs to generate
-- intermediate values for the inputs and outputs of the operators. We use type
-- info for that generation.
--
-- Since here we only have one type, the integer type, we can use the
-- 'DefaultType'. It can be used to generate intermediate symbolic integers
-- along with the 'DefaultSem'.
instance (MonadContext ctx) => OpTyping OpCode ctx where
  type OpTypeType OpCode = DefaultType
  typeOp = simpleTyping $ \case
    Plus -> binaryDefaultType
    Mul -> binaryDefaultType
    Minus -> binaryDefaultType
    UMinus -> unaryDefaultType

-- Pretty printing
instance PPrint OpCode where
  pformat Plus = "plus"
  pformat Mul = "mul"
  pformat Minus = "minus"
  pformat UMinus = "uminus"

instance OpPPrint OpCode where
  describeArguments Plus = Right [Just "lhs", Just "rhs"]
  describeArguments Mul = Right [Just "lhs", Just "rhs"]
  describeArguments Minus = Right [Just "lhs", Just "rhs"]
  describeArguments UMinus = Right [Nothing]

instance OpSymmetryReduction OpCode where
  opUnreorderable _ _ = false
  opCommutativeArgPos Plus = mrgReturn [[0, 1]]
  opCommutativeArgPos Mul = mrgReturn [[0, 1]]
  opCommutativeArgPos _ = mrgReturn []

instance OpReachableSymbols OpCode where
  opReachableSymbols _ = HS.empty

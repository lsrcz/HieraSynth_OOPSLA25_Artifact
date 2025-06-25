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

module Operator.Add (Add (..), add) where

import qualified Data.HashSet as HS
import Data.List ((\\))
import EvalMode (MonadEvalContext)
import Grisette
  ( LogicalOp (false),
    PPrint (pformat),
    allClasses0,
    derive,
    mrgReturn,
    pprintClasses,
  )
import HieraSynth.Combinator.Sum (type (:<:) (inj))
import HieraSynth.Context (MonadContext)
import HieraSynth.Operator.OpReachableSymbols
  ( OpReachableSymbols (opReachableSymbols),
  )
import HieraSynth.Operator.OpSemantics
  ( DefaultSem,
    OpSemantics (applyOp),
    pureBinaryOp,
  )
import HieraSynth.Operator.OpTyping
  ( DefaultType,
    OpTyping (OpTypeType, typeOp),
    binaryDefaultType,
    simpleTyping,
  )
import HieraSynth.Program.ComponentSketch
  ( OpSymmetryReduction (opCommutativeArgPos, opUnreorderable),
  )
import HieraSynth.Program.Concrete (OpPPrint (describeArguments))
import Grisette.Unified (GetInteger)

data Add = Add

add :: (Add :<: op) => op
add = inj Add

derive [''Add] (allClasses0 \\ pprintClasses)

instance (MonadContext ctx) => OpTyping Add ctx where
  type OpTypeType Add = DefaultType
  typeOp = simpleTyping $ const binaryDefaultType

instance
  (MonadEvalContext mode ctx, i ~ GetInteger mode) =>
  OpSemantics DefaultSem Add i ctx
  where
  applyOp _ _ Add = pureBinaryOp "Add" (+)

instance PPrint Add where
  pformat Add = "add"

instance OpPPrint Add where
  describeArguments Add = Right [Just "lhs", Just "rhs"]

instance OpSymmetryReduction Add where
  opUnreorderable _ _ = false
  opCommutativeArgPos Add = mrgReturn [[0, 1]]

instance OpReachableSymbols Add where
  opReachableSymbols _ = HS.empty

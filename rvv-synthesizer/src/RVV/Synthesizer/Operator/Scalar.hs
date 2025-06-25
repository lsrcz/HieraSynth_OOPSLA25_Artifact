{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.Operator.Scalar
  ( Scalar (..),
    SketchScalar (..),
    scalarLongImm,
    sketchScalarLongImm,
  )
where

import Control.Lens (makeFieldsNoPrefix)
import Grisette
  ( GenSym (fresh),
    GenSymSimple (simpleFresh),
    Mergeable,
    PPrint (pformat),
    ToSym (toSym),
    mrgReturn,
  )
import HieraSynth.Combinator.Embed ((:<:) (inj))
import HieraSynth.Context (MonadAngelicContext, MonadContext)
import HieraSynth.Operator.OpParser (OpParser (opParser))
import HieraSynth.Operator.OpReachableSymbols
  ( OpReachableSymbols (opReachableSymbols),
  )
import HieraSynth.Operator.OpSemantics (OpSemantics (applyOp))
import HieraSynth.Operator.OpTyping (OpTyping (OpTypeType, typeOp))
import HieraSynth.Program.Choice.Counting (SplitChoice (splitChoice))
import HieraSynth.Program.ComponentSketch.SymmetryReduction
  ( OpSymmetryReduction (opCommutativeArgPos, opUnreorderable),
  )
import HieraSynth.Program.Concrete
  ( OpPPrint (describeArguments, prefixResults),
  )
import HieraSynth.Program.CostModel.PerStmtCostModel (OpCost (opCost))
import HieraSynth.Util.Parser (bracketCommaSep2, named)
import Grisette.Unified (EvalModeTag (C, S), UnifiedSymEq, EvalModeConvertible)
import RVV.EvalMode (MonadEvalMode)
import RVV.Parser.ArgParser (widthMulParser)
import RVV.Semantics.Imm (Imm)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Multiplier (WidthMul)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Synthesizer.CostModel.CostModel (CostModel)
import RVV.Synthesizer.Feature.ExtractFeature (ExtractFeature (extractFeature))
import RVV.Synthesizer.Feature.FeatureSet
  ( FeatureSet,
    widthMulFeature,
  )
import RVV.Synthesizer.Lens
  ( HasImm (imm),
    HasWidthMul (widthMul),
  )
import RVV.Synthesizer.OpSemantics.Scalar
  ( applyScalarLong,
    typeScalarLong,
  )
import RVV.Synthesizer.Operator.Common.ImmSpec (ImmSpec)
import RVV.Synthesizer.Operator.Common.PPrint
  ( PPrintArgList (pformatArgList),
    withName,
  )
import RVV.Synthesizer.Operator.Common.Parser (immParser)
import RVV.Synthesizer.Operator.Common.TypeSketchOp (typeSketchOp)
import RVV.Synthesizer.SketchGen.ToFastSketch (ToFastSketch (toFastSketch))
import RVV.Synthesizer.Specification.ScaleLMul (ScaleLMul (scaleLMul))
import RVV.Synthesizer.Type (ValueType)
import RVV.Synthesizer.Value (Value)
import RVV.Util.Derive
  ( deriveFullExcept,
    deriveNoSymEval,
    firstModeDeriveConfig,
  )
import Text.Megaparsec.Char (string)

-- | Operators for scalar manipulation
data Scalar mode = ScalarLongImm {_widthMul :: WidthMul, _imm :: Imm mode}

-- | Sketch version of Scalar operators
data SketchScalar = SketchScalarLongImm {_widthMul :: WidthMul, _imm :: ImmSpec}

deriveFullExcept
  firstModeDeriveConfig
  [''Scalar]
  [''UnifiedSymEq]
deriveNoSymEval [''SketchScalar]
makeFieldsNoPrefix ''Scalar
makeFieldsNoPrefix ''SketchScalar

-- Smart constructors
scalarLongImm ::
  (Scalar mode :<: op) =>
  WidthMul ->
  Imm mode ->
  op
scalarLongImm xmul i = inj $ ScalarLongImm xmul i

sketchScalarLongImm ::
  (SketchScalar :<: op) =>
  WidthMul ->
  ImmSpec ->
  op
sketchScalarLongImm xmul i = inj $ SketchScalarLongImm xmul i

-- Type class instances
instance
  ( SemConstraint mode ctx,
    EvalModeConvertible opMode mode,
    MonadEvalMode opMode ctx
  ) =>
  OpSemantics MachineConfig (Scalar opMode) (Value mode) ctx
  where
  applyOp machine _ op inputs = case toSym op of
    ScalarLongImm xmul i -> applyScalarLong machine xmul i inputs

instance (MonadContext ctx) => OpTyping (Scalar mode) ctx where
  type OpTypeType (Scalar mode) = ValueType
  typeOp op = case op of
    ScalarLongImm xmul _ -> typeScalarLong xmul

instance (MonadAngelicContext ctx) => OpTyping SketchScalar ctx where
  type OpTypeType SketchScalar = ValueType
  typeOp = typeSketchOp @(Scalar 'S)

instance OpSymmetryReduction (Scalar mode) where
  opUnreorderable = error "Should not be called"
  opCommutativeArgPos _ = mrgReturn []

instance GenSym SketchScalar (Scalar 'S) where
  fresh (SketchScalarLongImm xmul i) = do
    imm <- simpleFresh i
    return $ mrgReturn $ ScalarLongImm xmul imm

instance GenSym (Scalar 'C) (Scalar 'S)

instance GenSymSimple (Scalar 'C) (Scalar 'S) where
  simpleFresh = return . toSym

instance OpPPrint (Scalar 'C) where
  describeArguments _ = return []
  prefixResults _ = return []

instance PPrint (Scalar 'C) where
  pformat op = case op of
    ScalarLongImm xmul i ->
      "scalar" <> pformatArgList (withName "xmul" xmul, withName "imm" i)

instance OpPPrint SketchScalar where
  describeArguments _ = return []
  prefixResults _ = return []

instance PPrint SketchScalar where
  pformat op = case op of
    SketchScalarLongImm xmul i ->
      "scalar" <> pformatArgList (withName "xmul" xmul, withName "imm" i)

instance OpParser (Scalar 'C) where
  opParser = do
    string "scalar"
    (xmul, i) <-
      bracketCommaSep2
        (named "xmul" widthMulParser)
        (named "imm" immParser)
    return $ ScalarLongImm xmul i

instance OpReachableSymbols (Scalar 'C) where
  opReachableSymbols = mempty

instance SplitChoice SketchScalar where
  splitChoice = return

instance
  (MonadContext ctx, Num cost, Mergeable cost) =>
  OpCost CostModel (Scalar mode) cost ctx
  where
  opCost _ _ _ = mrgReturn 0

instance ExtractFeature (Scalar mode) FeatureSet where
  extractFeature op = case op of
    ScalarLongImm xmul _ -> widthMulFeature xmul

instance ToFastSketch (Scalar 'C) SketchScalar where
  toFastSketch op = case op of
    ScalarLongImm xmul i -> SketchScalarLongImm xmul (toFastSketch i)

instance ScaleLMul (Scalar mode) where
  scaleLMul _ op = op

instance ScaleLMul SketchScalar where
  scaleLMul _ op = op

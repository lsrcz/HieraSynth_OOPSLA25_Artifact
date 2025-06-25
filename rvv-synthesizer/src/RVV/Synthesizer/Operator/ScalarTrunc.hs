{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
{-# LANGUAGE DataKinds #-}

module RVV.Synthesizer.Operator.ScalarTrunc
  ( ScalarTrunc (..),
    SketchScalarTrunc (..),
    scalarTrunc,
    sketchScalarTrunc,
  )
where

import Control.Lens (makeFieldsNoPrefix)
import GHC.Generics (Generic)
import Grisette
  ( GenSym,
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
import Grisette.Unified (UnifiedSymEq)
import RVV.EvalMode (MonadEvalMode)
import RVV.Parser.ArgParser (widthMulParser)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Multiplier (WidthMul)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Synthesizer.CostModel.CostModel (CostModel)
import RVV.Synthesizer.Feature.ExtractFeature (ExtractFeature (extractFeature))
import RVV.Synthesizer.Feature.FeatureSet (FeatureSet, widthMulFeature)
import RVV.Synthesizer.Lens
  ( HasWidthMulDest (widthMulDest),
    HasWidthMulSrc (widthMulSrc),
  )
import RVV.Synthesizer.OpSemantics.Scalar (applyScalarTrunc, typeScalarTrunc)
import RVV.Synthesizer.Operator.Common.PPrint
  ( PPrintArgList (pformatArgList),
    withName,
  )
import RVV.Synthesizer.Operator.Common.TypeSketchOp (typeSketchOp)
import RVV.Synthesizer.SketchGen.ToFastSketch (ToFastSketch (toFastSketch))
import RVV.Synthesizer.Specification.ScaleLMul (ScaleLMul (scaleLMul))
import RVV.Synthesizer.Type (ValueType)
import RVV.Synthesizer.Value (Value)
import RVV.Util.Derive
  ( deriveFullExcept,
    deriveNoSymEval,
    noModeDeriveConfig,
  )
import Text.Megaparsec.Char (string)

data ScalarTrunc = ScalarTrunc
  { _widthMulSrc :: WidthMul,
    _widthMulDest :: WidthMul
  }
  deriving (Generic)

data SketchScalarTrunc = SketchScalarTrunc
  { _widthMulSrc :: WidthMul,
    _widthMulDest :: WidthMul
  }
  deriving (Generic)

deriveFullExcept
  noModeDeriveConfig
  [''ScalarTrunc]
  [''UnifiedSymEq]
deriveNoSymEval [''SketchScalarTrunc]
makeFieldsNoPrefix ''SketchScalarTrunc
makeFieldsNoPrefix ''ScalarTrunc

scalarTrunc ::
  (ScalarTrunc :<: op) =>
  WidthMul ->
  WidthMul ->
  op
scalarTrunc src dest = inj $ ScalarTrunc src dest

sketchScalarTrunc ::
  (SketchScalarTrunc :<: op) =>
  WidthMul ->
  WidthMul ->
  op
sketchScalarTrunc src dest = inj $ SketchScalarTrunc src dest

instance
  (SemConstraint mode ctx, MonadEvalMode mode ctx) =>
  OpSemantics MachineConfig ScalarTrunc (Value mode) ctx
  where
  applyOp machine _ (ScalarTrunc src dest) = applyScalarTrunc machine src dest

instance (MonadContext ctx) => OpTyping ScalarTrunc ctx where
  type OpTypeType ScalarTrunc = ValueType
  typeOp (ScalarTrunc src dest) = typeScalarTrunc src dest

instance (MonadAngelicContext ctx) => OpTyping SketchScalarTrunc ctx where
  type OpTypeType SketchScalarTrunc = ValueType
  typeOp = typeSketchOp @ScalarTrunc

instance OpSymmetryReduction ScalarTrunc where
  opUnreorderable = error "Should not be called"
  opCommutativeArgPos _ = mrgReturn []

instance GenSym SketchScalarTrunc ScalarTrunc

instance GenSymSimple SketchScalarTrunc ScalarTrunc where
  simpleFresh (SketchScalarTrunc src dest) = return $ ScalarTrunc src dest

instance GenSym ScalarTrunc ScalarTrunc

instance GenSymSimple ScalarTrunc ScalarTrunc where
  simpleFresh = return . toSym

instance OpPPrint ScalarTrunc where
  describeArguments _ = return []

instance PPrint ScalarTrunc where
  pformat (ScalarTrunc src dest) =
    "trunc" <> pformatArgList (withName "src" src, withName "dest" dest)

instance OpPPrint SketchScalarTrunc where
  describeArguments _ = return []
  prefixResults _ = return []

instance PPrint SketchScalarTrunc where
  pformat (SketchScalarTrunc src dest) =
    "trunc" <> pformatArgList (withName "src" src, withName "dest" dest)

instance OpParser ScalarTrunc where
  opParser = do
    string "trunc"
    (src, dest) <-
      bracketCommaSep2
        (named "src" widthMulParser)
        (named "dest" widthMulParser)
    return $ ScalarTrunc src dest

instance OpReachableSymbols ScalarTrunc where
  opReachableSymbols = mempty

instance SplitChoice SketchScalarTrunc where
  splitChoice (SketchScalarTrunc src dest) = return $ SketchScalarTrunc src dest

instance
  (MonadContext ctx, Num cost, Mergeable cost) =>
  OpCost CostModel ScalarTrunc cost ctx
  where
  opCost _ _ _ = mrgReturn 0

instance ExtractFeature ScalarTrunc FeatureSet where
  extractFeature (ScalarTrunc src dest) =
    widthMulFeature src <> widthMulFeature dest

instance ToFastSketch ScalarTrunc SketchScalarTrunc where
  toFastSketch (ScalarTrunc src dest) = SketchScalarTrunc src dest

instance ScaleLMul ScalarTrunc where
  scaleLMul _ = id

instance ScaleLMul SketchScalarTrunc where
  scaleLMul _ = id

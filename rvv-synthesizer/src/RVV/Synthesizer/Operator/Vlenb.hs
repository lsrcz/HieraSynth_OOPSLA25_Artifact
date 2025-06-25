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

module RVV.Synthesizer.Operator.Vlenb
  ( Vlenb (..),
    SketchVlenb (..),
    vlenb,
    sketchVlenb,
  )
where

import Control.Lens (makeFieldsNoPrefix)
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
import Grisette.Unified (UnifiedSymEq)
import RVV.EvalMode (MonadEvalMode)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Synthesizer.CostModel.CostModel (CostModel)
import RVV.Synthesizer.Feature.ExtractFeature (ExtractFeature (extractFeature))
import RVV.Synthesizer.Feature.FeatureSet (FeatureSet)
import RVV.Synthesizer.OpSemantics.Vlenb (applyVlenb, typeVlenb)
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

data Vlenb = Vlenb

data SketchVlenb = SketchVlenb

deriveFullExcept
  noModeDeriveConfig
  [''Vlenb]
  [''UnifiedSymEq]
deriveNoSymEval [''SketchVlenb]
makeFieldsNoPrefix ''Vlenb
makeFieldsNoPrefix ''SketchVlenb

vlenb :: (Vlenb :<: op) => op
vlenb = inj Vlenb

sketchVlenb :: (SketchVlenb :<: op) => op
sketchVlenb = inj SketchVlenb

instance
  (SemConstraint mode ctx, MonadEvalMode mode ctx) =>
  OpSemantics MachineConfig Vlenb (Value mode) ctx
  where
  applyOp machine _ _ = applyVlenb machine

instance (MonadContext ctx) => OpTyping Vlenb ctx where
  type OpTypeType Vlenb = ValueType
  typeOp _ = typeVlenb

instance (MonadAngelicContext ctx) => OpTyping SketchVlenb ctx where
  type OpTypeType SketchVlenb = ValueType
  typeOp = typeSketchOp @Vlenb

instance OpSymmetryReduction Vlenb where
  opUnreorderable = error "Should not be called"
  opCommutativeArgPos _ = mrgReturn []

instance GenSym SketchVlenb Vlenb

instance GenSymSimple SketchVlenb Vlenb where
  simpleFresh SketchVlenb = return Vlenb

instance GenSym Vlenb Vlenb

instance GenSymSimple Vlenb Vlenb where
  simpleFresh = return . toSym

instance OpPPrint Vlenb where
  describeArguments _ = return []

instance PPrint Vlenb where
  pformat Vlenb = "vlenb"

instance OpPPrint SketchVlenb where
  describeArguments _ = return []
  prefixResults _ = return []

instance PPrint SketchVlenb where
  pformat _ = "vlenb"

instance OpParser Vlenb where
  opParser = do
    string "vlenb"
    return Vlenb

instance OpReachableSymbols Vlenb where
  opReachableSymbols = mempty

instance SplitChoice SketchVlenb where
  splitChoice SketchVlenb = return SketchVlenb

instance
  (MonadContext ctx, Num cost, Mergeable cost) =>
  OpCost CostModel Vlenb cost ctx
  where
  opCost _ _ _ = mrgReturn 0

instance ExtractFeature Vlenb FeatureSet where
  extractFeature _ = mempty

instance ToFastSketch Vlenb SketchVlenb where
  toFastSketch _ = SketchVlenb

instance ScaleLMul Vlenb where
  scaleLMul _ op = op

instance ScaleLMul SketchVlenb where
  scaleLMul _ op = op

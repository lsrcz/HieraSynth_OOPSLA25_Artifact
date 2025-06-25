{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.Operator.MaskLogical
  ( MaskLogical (..),
    SketchMaskLogical (..),
    RVV.Synthesizer.Operator.MaskLogical.maskLogical,
    sketchMaskLogical,
  )
where

import Control.Lens (makeFieldsNoPrefix, (%~), (&))
import Data.Foldable (toList)
import qualified Data.HashSet as HS
import GHC.Generics (Generic)
import Grisette
  ( GenSym,
    GenSymSimple (simpleFresh),
    Mergeable,
    PPrint (pformat),
    ToSym (toSym),
    Union,
    chooseFresh,
    mrgReturn,
  )
import Grisette.Internal.Unified.UnifiedData (extractData)
import HieraSynth.Combinator.Embed ((:<:) (inj))
import HieraSynth.Context (MonadAngelicContext)
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
import HieraSynth.Util.Parser
  ( betweenBrackets,
    named,
  )
import Grisette.Unified
  ( EvalModeConvertible,
    EvalModeTag (C, S),
    GetData,
    UnifiedSymEq,
  )
import RVV.EvalMode (MonadEvalMode)
import RVV.Parser.ArgParser
  ( maskMulParser,
  )
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Multiplier (MaskMul)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Synthesizer.CostModel.CostModel
  ( CostModel (CostModel, maskOpCost, modelMachineConfig, singleWidthCost),
  )
import RVV.Synthesizer.Feature.ExtractFeature (ExtractFeature (extractFeature))
import RVV.Synthesizer.Feature.FeatureSet
  ( Feature (Masking),
    FeatureSet (opFeatures),
    maskMulFeature,
  )
import RVV.Synthesizer.Lens
  ( HasMaskLogicalOpCode (maskLogicalOpCode),
    HasMaskMul (maskMul),
  )
import RVV.Synthesizer.OpSemantics.MaskLogical
  ( applyMaskLogical,
    typeMaskLogical,
  )
import RVV.Synthesizer.Operator.Common.OpSymmetryReduction (checkOpCommutativeArgPos)
import RVV.Synthesizer.Operator.Common.PPrint
  ( PPrintArgList (pformatArgList),
    pformatChoices,
    withName,
  )
import RVV.Synthesizer.Operator.Common.TypeSketchOp (typeSketchOp)
import RVV.Synthesizer.Parameter.MaskLogicalOpCode
  ( MaskLogicalOpCode,
    commutativeMaskLogicalOpCode,
    interpretMaskLogicalOpCodeSemantics,
    interpretMaskLogicalOpCodeUninitialized,
    maskLogicalOpCodeParser,
  )
import RVV.Synthesizer.SketchGen.ToFastSketch (ToFastSketch (toFastSketch))
import RVV.Synthesizer.Specification.ScaleLMul
  ( ScaleLMul (scaleLMul),
  )
import RVV.Synthesizer.Type (ValueType)
import RVV.Synthesizer.Value (Value)
import RVV.Util.Derive
  ( deriveFullExcept,
    deriveNoSymEval,
    firstModeDeriveConfig,
  )

data MaskLogical mode
  = MaskLogical
  { _maskMul :: MaskMul,
    _maskLogicalOpCode :: GetData mode MaskLogicalOpCode
  }
  deriving (Generic)

data SketchMaskLogical
  = SketchMaskLogical
  { _maskMul :: MaskMul,
    _maskLogicalOpCode :: [MaskLogicalOpCode]
  }
  deriving (Generic)

deriveFullExcept firstModeDeriveConfig [''MaskLogical] [''UnifiedSymEq]
deriveNoSymEval [''SketchMaskLogical]
makeFieldsNoPrefix ''MaskLogical
makeFieldsNoPrefix ''SketchMaskLogical

maskLogical ::
  (MaskLogical mode :<: op) =>
  MaskMul ->
  GetData mode MaskLogicalOpCode ->
  op
maskLogical mmul logicalOp =
  inj $ MaskLogical mmul logicalOp

sketchMaskLogical ::
  (SketchMaskLogical :<: op) =>
  MaskMul ->
  [MaskLogicalOpCode] ->
  op
sketchMaskLogical mmul logicalOp =
  inj $ SketchMaskLogical mmul logicalOp

instance
  ( SemConstraint mode ctx,
    EvalModeConvertible opMode mode,
    MonadEvalMode opMode ctx
  ) =>
  OpSemantics MachineConfig (MaskLogical opMode) (Value mode) ctx
  where
  applyOp vconst _ op inputs = do
    case toSym op of
      MaskLogical maskMul (opCode :: GetData opMode MaskLogicalOpCode) -> do
        op <- extractData opCode
        applyMaskLogical
          vconst
          maskMul
          (interpretMaskLogicalOpCodeSemantics op)
          (interpretMaskLogicalOpCodeUninitialized op)
          inputs

instance (SemConstraint mode ctx) => OpTyping (MaskLogical mode) ctx where
  type OpTypeType (MaskLogical mode) = ValueType
  typeOp (MaskLogical maskMul _) = typeMaskLogical maskMul

instance (MonadAngelicContext ctx) => OpTyping SketchMaskLogical ctx where
  type OpTypeType SketchMaskLogical = ValueType
  typeOp = typeSketchOp @(MaskLogical 'S)

instance
  (MonadEvalMode mode Union) =>
  OpSymmetryReduction (MaskLogical mode)
  where
  opUnreorderable = error "Should not be called"
  opCommutativeArgPos (MaskLogical _ opCode) =
    checkOpCommutativeArgPos
      opCode
      commutativeMaskLogicalOpCode
      [[1, 2]]

instance GenSym SketchMaskLogical (MaskLogical 'S)

instance GenSymSimple SketchMaskLogical (MaskLogical 'S) where
  simpleFresh (SketchMaskLogical mmul logicalOp) = do
    logicalOp <- chooseFresh logicalOp
    return $ MaskLogical mmul logicalOp

instance GenSym (MaskLogical 'C) (MaskLogical 'S)

instance GenSymSimple (MaskLogical 'C) (MaskLogical 'S) where
  simpleFresh = return . toSym

instance OpPPrint (MaskLogical 'C) where
  describeArguments _ = return []

instance PPrint (MaskLogical 'C) where
  pformat (MaskLogical maskMul opCode) =
    "v"
      <> pformat opCode
      <> ".mm"
      <> pformatArgList (withName "mmul" maskMul)

instance PPrint SketchMaskLogical where
  pformat (SketchMaskLogical mmul logicalOp) =
    "v"
      <> pformatChoices logicalOp
      <> ".mm"
      <> pformatArgList (withName "mmul" mmul)

instance OpPPrint SketchMaskLogical where
  describeArguments _ = return []
  prefixResults _ = return []

instance OpParser (MaskLogical 'C) where
  opParser = do
    op <- maskLogicalOpCodeParser "v" ".mm"
    maskMul <- betweenBrackets (named "mmul" maskMulParser)
    return $ MaskLogical maskMul op

instance OpReachableSymbols (MaskLogical 'C) where
  opReachableSymbols = mempty

instance SplitChoice SketchMaskLogical where
  splitChoice (SketchMaskLogical mmul logicalOp) = do
    logicalOp <- logicalOp
    return $ SketchMaskLogical mmul [logicalOp]

instance
  (SemConstraint mode ctx, Num cost, Mergeable cost) =>
  OpCost CostModel (MaskLogical mode) cost ctx
  where
  opCost CostModel {..} _ (MaskLogical mmul _) = do
    mrgReturn $ fromIntegral $ maskOpCost mmul

instance ExtractFeature (MaskLogical 'C) FeatureSet where
  extractFeature (MaskLogical mmul _) =
    maskMulFeature mmul <> mempty {opFeatures = HS.singleton Masking}

instance ToFastSketch (MaskLogical 'C) SketchMaskLogical where
  toFastSketch (MaskLogical mmul logicalOp) =
    SketchMaskLogical mmul (toList logicalOp)

instance ScaleLMul (MaskLogical 'C) where
  scaleLMul ratio op = op & (maskMul %~ scaleLMul ratio)

instance ScaleLMul SketchMaskLogical where
  scaleLMul ratio op = op & (maskMul %~ scaleLMul ratio)

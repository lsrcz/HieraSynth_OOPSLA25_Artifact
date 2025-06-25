{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.Operator.SetVectorLength
  ( SetVectorLength (..),
    SketchSetVectorLength (..),
    setVectorLength,
    setMaxVectorLength,
    setRelayedVectorLength,
    sketchSetVectorLength,
    sketchSetMaxVectorLength,
    sketchSetRelayedVectorLength,
  )
where

import Control.Applicative ((<|>))
import Control.Lens (makeFieldsNoPrefix, (%~), (&))
import qualified Data.HashSet as HS
import GHC.Generics (Generic)
import Grisette
  ( GenSym,
    GenSymSimple (simpleFresh),
    Mergeable,
    PPrint (pformat),
    ToSym (toSym),
    Union,
    chooseSimpleFresh,
    mrgReturn,
  )
import HieraSynth.Combinator.Embed (type (:<:) (inj))
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
  ( CharParser,
    bracketCommaSep2,
    bracketCommaSep3,
    named,
  )
import Grisette.Unified (EvalModeConvertible, EvalModeTag (C, S), UnifiedSymEq)
import RVV.EvalMode (MonadEvalMode)
import RVV.Parser.ArgParser (maskMulParser, policyParser)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Multiplier (MaskMul)
import RVV.Semantics.Policy (Policy)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Synthesizer.CostModel.CostModel (CostModel (CostModel, maskOpCost))
import RVV.Synthesizer.Feature.ExtractFeature (ExtractFeature (extractFeature))
import RVV.Synthesizer.Feature.FeatureSet
  ( Feature (FullVL, PartialVL),
    FeatureSet (maskMulFeatures, opFeatures, policyFeatures),
  )
import RVV.Synthesizer.Lens
  ( HasMaskMul (maskMul),
    HasMaskMulSrc (maskMulSrc),
    HasPolicy (policy),
  )
import RVV.Synthesizer.OpSemantics.Misc
  ( applyVSetVL,
    applyVSetVLMax,
    applyVSetVLRelay,
    typeVSetVL,
    typeVSetVLMax,
    typeVSetVLRelay,
  )
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
    firstModeDeriveConfig,
  )
import Text.Megaparsec.Char (string)

data SetVectorLength mode
  = SetVectorLength {_maskMul :: MaskMul, _policy :: Policy mode}
  | SetMaxVectorLength {_maskMul :: MaskMul, _policy :: Policy mode}
  | SetRelayedVectorLength
      { _maskMul :: MaskMul,
        _maskMulSrc :: MaskMul,
        _policy :: Policy mode
      }
  deriving (Generic)

data SketchSetVectorLength
  = SketchSetVectorLength {_maskMul :: MaskMul, _policy :: [Policy 'C]}
  | SketchSetMaxVectorLength {_maskMul :: MaskMul, _policy :: [Policy 'C]}
  | SketchSetRelayedVectorLength
      { _maskMul :: MaskMul,
        _maskMulSrc :: MaskMul,
        _policy :: [Policy 'C]
      }
  deriving (Generic)

deriveFullExcept
  firstModeDeriveConfig
  [''SetVectorLength]
  [''UnifiedSymEq]
deriveNoSymEval [''SketchSetVectorLength]
makeFieldsNoPrefix ''SetVectorLength
makeFieldsNoPrefix ''SketchSetVectorLength

setVectorLength ::
  (SetVectorLength mode :<: op) =>
  MaskMul ->
  Policy mode ->
  op
setVectorLength mmul policy = inj $ SetVectorLength mmul policy

setMaxVectorLength ::
  (SetVectorLength mode :<: op) =>
  MaskMul ->
  Policy mode ->
  op
setMaxVectorLength mmul policy = inj $ SetMaxVectorLength mmul policy

setRelayedVectorLength ::
  (SetVectorLength mode :<: op) =>
  MaskMul ->
  MaskMul ->
  Policy mode ->
  op
setRelayedVectorLength mmul mmulSrc policy =
  inj $ SetRelayedVectorLength mmul mmulSrc policy

sketchSetVectorLength ::
  (SketchSetVectorLength :<: op) =>
  MaskMul ->
  [Policy 'C] ->
  op
sketchSetVectorLength mmul policy = inj $ SketchSetVectorLength mmul policy

sketchSetMaxVectorLength ::
  (SketchSetVectorLength :<: op) =>
  MaskMul ->
  [Policy 'C] ->
  op
sketchSetMaxVectorLength mmul policy =
  inj $ SketchSetMaxVectorLength mmul policy

sketchSetRelayedVectorLength ::
  (SketchSetVectorLength :<: op) =>
  MaskMul ->
  MaskMul ->
  [Policy 'C] ->
  op
sketchSetRelayedVectorLength mmul mmulSrc policy =
  inj $ SketchSetRelayedVectorLength mmul mmulSrc policy

instance
  ( SemConstraint mode ctx,
    EvalModeConvertible opMode mode,
    MonadEvalMode opMode ctx
  ) =>
  OpSemantics MachineConfig (SetVectorLength opMode) (Value mode) ctx
  where
  applyOp machine _ op inputs =
    case toSym op of
      SetVectorLength maskMul policy ->
        applyVSetVL machine maskMul policy inputs
      SetMaxVectorLength maskMul policy ->
        applyVSetVLMax machine maskMul policy inputs
      SetRelayedVectorLength maskMul maskMulSrc policy ->
        applyVSetVLRelay machine maskMul maskMulSrc policy inputs

instance (SemConstraint mode ctx) => OpTyping (SetVectorLength mode) ctx where
  type OpTypeType (SetVectorLength mode) = ValueType
  typeOp (SetVectorLength maskMul _) = typeVSetVL maskMul
  typeOp (SetMaxVectorLength maskMul _) = typeVSetVLMax maskMul
  typeOp (SetRelayedVectorLength maskMul maskMulSrc _) =
    typeVSetVLRelay maskMul maskMulSrc

instance (MonadAngelicContext ctx) => OpTyping SketchSetVectorLength ctx where
  type OpTypeType SketchSetVectorLength = ValueType
  typeOp = typeSketchOp @(SetVectorLength 'S)

instance (MonadEvalMode mode Union) => OpSymmetryReduction (SetVectorLength mode) where
  opUnreorderable = error "Should not be called"
  opCommutativeArgPos _ = mrgReturn []

instance GenSym SketchSetVectorLength (SetVectorLength 'S)

instance GenSymSimple SketchSetVectorLength (SetVectorLength 'S) where
  simpleFresh (SketchSetVectorLength mmul policy) = do
    policy <- chooseSimpleFresh (toSym policy)
    return $ SetVectorLength mmul policy
  simpleFresh (SketchSetMaxVectorLength mmul policy) = do
    policy <- chooseSimpleFresh (toSym policy)
    return $ SetMaxVectorLength mmul policy
  simpleFresh (SketchSetRelayedVectorLength mmul mmulSrc policy) = do
    policy <- chooseSimpleFresh (toSym policy)
    return $ SetRelayedVectorLength mmul mmulSrc policy

instance GenSym (SetVectorLength 'C) (SetVectorLength 'S)

instance GenSymSimple (SetVectorLength 'C) (SetVectorLength 'S) where
  simpleFresh = return . toSym

instance OpPPrint (SetVectorLength 'C) where
  describeArguments _ = return []

instance PPrint (SetVectorLength 'C) where
  pformat (SetVectorLength maskMul policy) =
    "vsetvl" <> pformatArgList (withName "mmul" maskMul, policy)
  pformat (SetMaxVectorLength maskMul policy) =
    "vsetvlmax" <> pformatArgList (withName "mmul" maskMul, policy)
  pformat (SetRelayedVectorLength maskMul maskMulSrc policy) =
    "vsetvlrelay"
      <> pformatArgList
        (withName "mmul" maskMul, withName "src_mmul" maskMulSrc, policy)

instance PPrint SketchSetVectorLength where
  pformat (SketchSetVectorLength mmul policy) =
    "vsetvl" <> pformatArgList (withName "mmul" mmul, policy)
  pformat (SketchSetMaxVectorLength mmul policy) =
    "vsetvlmax" <> pformatArgList (withName "mmul" mmul, policy)
  pformat (SketchSetRelayedVectorLength mmul mmulSrc policy) =
    "vsetvlrelay"
      <> pformatArgList (withName "mmul" mmul, withName "src_mmul" mmulSrc, policy)

instance OpPPrint SketchSetVectorLength where
  describeArguments _ = return []
  prefixResults _ = return []

vsetvlParser :: (CharParser e s m) => m (SetVectorLength 'C)
vsetvlParser = do
  string "vsetvl"
  (mmul, policy) <- bracketCommaSep2 (named "mmul" maskMulParser) policyParser
  return $ SetVectorLength mmul policy

vsetvlmaxParser :: (CharParser e s m) => m (SetVectorLength 'C)
vsetvlmaxParser = do
  string "vsetvlmax"
  (mmul, policy) <- bracketCommaSep2 (named "mmul" maskMulParser) policyParser
  return $ SetMaxVectorLength mmul policy

vsetvlrelayParser :: (CharParser e s m) => m (SetVectorLength 'C)
vsetvlrelayParser = do
  string "vsetvlrelay"
  (mmul, srcMMul, policy) <-
    bracketCommaSep3
      (named "mmul" maskMulParser)
      (named "src_mmul" maskMulParser)
      policyParser
  return $ SetRelayedVectorLength mmul srcMMul policy

instance OpParser (SetVectorLength 'C) where
  opParser = vsetvlmaxParser <|> vsetvlrelayParser <|> vsetvlParser

instance OpReachableSymbols (SetVectorLength 'C) where
  opReachableSymbols = mempty

instance SplitChoice SketchSetVectorLength where
  splitChoice (SketchSetVectorLength mmul policy) = do
    policy <- policy
    return $ SketchSetVectorLength mmul [policy]
  splitChoice (SketchSetMaxVectorLength mmul policy) = do
    policy <- policy
    return $ SketchSetMaxVectorLength mmul [policy]
  splitChoice (SketchSetRelayedVectorLength mmul mmulSrc policy) = do
    policy <- policy
    return $ SketchSetRelayedVectorLength mmul mmulSrc [policy]

instance
  (SemConstraint mode ctx, Num cost, Mergeable cost) =>
  OpCost CostModel (SetVectorLength mode) cost ctx
  where
  opCost CostModel {..} _ _ = mrgReturn 0

instance ExtractFeature (SetVectorLength 'C) FeatureSet where
  extractFeature (SetVectorLength maskMul policy) =
    mempty
      { maskMulFeatures = HS.singleton maskMul,
        policyFeatures = HS.singleton policy,
        opFeatures = HS.singleton PartialVL
      }
  extractFeature (SetMaxVectorLength mmul policy) =
    mempty
      { maskMulFeatures = HS.fromList [mmul],
        policyFeatures = HS.singleton policy,
        opFeatures = HS.singleton FullVL
      }
  extractFeature (SetRelayedVectorLength mmul srcMMul policy) =
    mempty
      { maskMulFeatures = HS.fromList [mmul, srcMMul],
        policyFeatures = HS.singleton policy,
        opFeatures = HS.singleton PartialVL
      }

instance ToFastSketch (SetVectorLength 'C) SketchSetVectorLength where
  toFastSketch (SetVectorLength mmul policy) =
    SketchSetVectorLength mmul [toSym policy]
  toFastSketch (SetMaxVectorLength mmul policy) =
    SketchSetMaxVectorLength mmul [toSym policy]
  toFastSketch (SetRelayedVectorLength mmul mmulSrc policy) =
    SketchSetRelayedVectorLength mmul mmulSrc [toSym policy]

instance ScaleLMul (SetVectorLength 'C) where
  scaleLMul ratio op =
    op
      & (maskMul %~ scaleLMul ratio)
      & (maskMulSrc %~ scaleLMul ratio)

instance ScaleLMul SketchSetVectorLength where
  scaleLMul ratio op =
    op
      & (maskMul %~ scaleLMul ratio)
      & (maskMulSrc %~ scaleLMul ratio)

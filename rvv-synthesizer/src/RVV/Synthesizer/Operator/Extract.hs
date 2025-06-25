{-# LANGUAGE DataKinds #-}
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

module RVV.Synthesizer.Operator.Extract
  ( Extract (..),
    SketchExtract (..),
    extractVector,
    sketchExtractVector,
    extractMask,
    sketchExtractMask,
    vectorTruncate,
    sketchVectorTruncate,
  )
where

import Control.Applicative ((<|>))
import Control.Lens (makeFieldsNoPrefix)
import qualified Data.HashSet as HS
import Grisette
  ( GenSym (fresh),
    GenSymSimple (simpleFresh),
    Mergeable,
    PPrint (pformat),
    ToSym (toSym),
    chooseFresh,
    mrgFmap,
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
import HieraSynth.Util.Parser
  ( bracketCommaSep2,
    bracketCommaSep3,
    named,
  )
import Grisette.Unified (UnifiedSymEq)
import RVV.EvalMode (MonadEvalMode)
import RVV.Parser.ArgParser (maskMulParser, vectorConfigParser)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Multiplier (MaskMul)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.VectorConfig (VectorConfig (lengthMul))
import RVV.Synthesizer.CostModel.CostModel
  ( CostModel (CostModel, broadcastCost, slideCost),
  )
import RVV.Synthesizer.Feature.ExtractFeature (ExtractFeature (extractFeature))
import RVV.Synthesizer.Feature.FeatureSet
  ( Feature (GetSet),
    FeatureSet (opFeatures),
    maskMulFeature,
    vectorConfigFeature,
  )
import RVV.Synthesizer.Lens
  ( HasIndex (index),
    HasMaskMulPart (maskMulPart),
    HasMaskMulSrc (maskMulSrc),
    HasVectorConfigPart (vectorConfigPart),
    HasVectorConfigSrc (vectorConfigSrc),
  )
import RVV.Synthesizer.OpSemantics.VGet
  ( applyExtractMask,
    applyLMulTruncate,
    applyVGet,
    typeExtractMask,
    typeLMulTruncate,
    typeVGet,
  )
import RVV.Synthesizer.Operator.Common.PPrint
  ( PPrintArgList (pformatArgList),
    choices,
    withName,
  )
import RVV.Synthesizer.Operator.Common.TypeSketchOp (typeSketchOp)
import RVV.Synthesizer.Parameter.SlideDirection (SlideDirection (SlideDown))
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
import Text.Megaparsec.Char.Lexer (decimal)

data Extract
  = ExtractVector -- corresponds to VGet
      { _vectorConfigPart :: VectorConfig,
        _vectorConfigSrc :: VectorConfig,
        _index :: Int
      }
  | ExtractMask -- corresponds to ExtractMask
      { _maskMulPart :: MaskMul,
        _maskMulSrc :: MaskMul,
        _index :: Int
      }
  | VectorTruncate -- corresponds to VLMulTruncate
      { _vectorConfigPart :: VectorConfig,
        _vectorConfigSrc :: VectorConfig
      }

data SketchExtract
  = SketchExtractVector
      { _vectorConfigPart :: VectorConfig,
        _vectorConfigSrc :: VectorConfig,
        _index :: [Int]
      }
  | SketchExtractMask
      { _maskMulPart :: MaskMul,
        _maskMulSrc :: MaskMul,
        _index :: [Int]
      }
  | SketchVectorTruncate
      { _vectorConfigPart :: VectorConfig,
        _vectorConfigSrc :: VectorConfig
      }

deriveFullExcept
  noModeDeriveConfig
  [''Extract]
  [''UnifiedSymEq]
deriveNoSymEval [''SketchExtract]
makeFieldsNoPrefix ''Extract
makeFieldsNoPrefix ''SketchExtract

extractVector ::
  (Extract :<: op) =>
  VectorConfig ->
  VectorConfig ->
  Int ->
  op
extractVector part src idx = inj $ ExtractVector part src idx

sketchExtractVector ::
  (SketchExtract :<: op) =>
  VectorConfig ->
  VectorConfig ->
  [Int] ->
  op
sketchExtractVector part src idx = inj $ SketchExtractVector part src idx

extractMask ::
  (Extract :<: op) =>
  MaskMul ->
  MaskMul ->
  Int ->
  op
extractMask part src idx = inj $ ExtractMask part src idx

sketchExtractMask ::
  (SketchExtract :<: op) =>
  MaskMul ->
  MaskMul ->
  [Int] ->
  op
sketchExtractMask part src idx = inj $ SketchExtractMask part src idx

vectorTruncate ::
  (Extract :<: op) =>
  VectorConfig ->
  VectorConfig ->
  op
vectorTruncate part src = inj $ VectorTruncate part src

sketchVectorTruncate ::
  (SketchExtract :<: op) =>
  VectorConfig ->
  VectorConfig ->
  op
sketchVectorTruncate part src = inj $ SketchVectorTruncate part src

instance
  (SemConstraint mode ctx, MonadEvalMode mode ctx) =>
  OpSemantics MachineConfig Extract (Value mode) ctx
  where
  applyOp machine _ op inputs = case op of
    ExtractVector part src idx -> applyVGet machine part src idx inputs
    ExtractMask part src idx -> applyExtractMask machine part src idx inputs
    VectorTruncate part src -> applyLMulTruncate machine part src inputs

instance (MonadContext ctx) => OpTyping Extract ctx where
  type OpTypeType Extract = ValueType
  typeOp op = case op of
    ExtractVector part src _ -> typeVGet part src
    ExtractMask part src _ -> typeExtractMask part src
    VectorTruncate part src -> typeLMulTruncate part src

instance (MonadAngelicContext ctx) => OpTyping SketchExtract ctx where
  type OpTypeType SketchExtract = ValueType
  typeOp = typeSketchOp @Extract

instance OpSymmetryReduction Extract where
  opUnreorderable = error "Should not be called"
  opCommutativeArgPos _ = mrgReturn []

instance GenSym SketchExtract Extract where
  fresh (SketchExtractVector part src idx) = do
    idx <- chooseFresh idx
    return $ mrgFmap (ExtractVector part src) idx
  fresh (SketchExtractMask part src idx) = do
    idx <- chooseFresh idx
    return $ mrgFmap (ExtractMask part src) idx
  fresh (SketchVectorTruncate part src) =
    return $ mrgReturn $ VectorTruncate part src

instance GenSym Extract Extract

instance GenSymSimple Extract Extract where
  simpleFresh = return . toSym

instance OpPPrint Extract where
  describeArguments _ = return []
  prefixResults _ = return []

instance PPrint Extract where
  pformat op = case op of
    ExtractVector part src idx ->
      "vget"
        <> pformatArgList
          ( withName "part" part,
            withName "src" src,
            withName "idx" idx
          )
    ExtractMask part src idx ->
      "extract_mask"
        <> pformatArgList
          ( withName "part" part,
            withName "src" src,
            withName "idx" idx
          )
    VectorTruncate part src ->
      "vlmul_truncate"
        <> pformatArgList (withName "part" part, withName "src" src)

instance OpPPrint SketchExtract where
  describeArguments _ = return []
  prefixResults _ = return []

instance PPrint SketchExtract where
  pformat op = case op of
    SketchExtractVector part src idx ->
      "vget"
        <> pformatArgList
          ( withName "part" part,
            withName "src" src,
            withName "idx" $ choices idx
          )
    SketchExtractMask part src idx ->
      "extract_mask"
        <> pformatArgList
          ( withName "part" part,
            withName "src" src,
            withName "idx" $ choices idx
          )
    SketchVectorTruncate part src ->
      "vlmul_truncate"
        <> pformatArgList (withName "part" part, withName "src" src)

instance OpParser Extract where
  opParser = extractVectorParser <|> extractMaskParser <|> vectorTruncateParser
    where
      extractVectorParser = do
        string "vget"
        (part, src, idx) <-
          bracketCommaSep3
            (named "part" vectorConfigParser)
            (named "src" vectorConfigParser)
            (named "idx" decimal)
        return $ ExtractVector part src idx
      extractMaskParser = do
        string "extract_mask"
        (part, src, idx) <-
          bracketCommaSep3
            (named "part" maskMulParser)
            (named "src" maskMulParser)
            (named "idx" decimal)
        return $ ExtractMask part src idx
      vectorTruncateParser = do
        string "vlmul_truncate"
        (part, src) <-
          bracketCommaSep2
            (named "part" vectorConfigParser)
            (named "src" vectorConfigParser)
        return $ VectorTruncate part src

instance OpReachableSymbols Extract where
  opReachableSymbols = mempty

instance SplitChoice SketchExtract where
  splitChoice (SketchExtractVector part src idx) = do
    idx <- idx
    return $ SketchExtractVector part src [idx]
  splitChoice (SketchExtractMask part src idx) = do
    idx <- idx
    return $ SketchExtractMask part src [idx]
  splitChoice (SketchVectorTruncate part src) =
    return $ SketchVectorTruncate part src

instance
  (MonadContext ctx, Num cost, Mergeable cost) =>
  OpCost CostModel Extract cost ctx
  where
  opCost CostModel {..} _ op = case op of
    ExtractVector part src idx ->
      if lengthMul part >= 1 || idx == 0
        then mrgReturn 0
        else mrgReturn $ fromIntegral $ slideCost SlideDown src
    ExtractMask {} -> error "unsupported"
    VectorTruncate _ _ -> mrgReturn 0

instance ExtractFeature Extract FeatureSet where
  extractFeature op = case op of
    ExtractVector part src _ ->
      vectorConfigFeature part
        <> vectorConfigFeature src
        <> mempty {opFeatures = HS.singleton GetSet}
    ExtractMask part src _ ->
      maskMulFeature part
        <> maskMulFeature src
        <> mempty {opFeatures = HS.singleton GetSet}
    VectorTruncate part src ->
      vectorConfigFeature part
        <> vectorConfigFeature src
        <> mempty {opFeatures = HS.singleton GetSet}

instance ToFastSketch Extract SketchExtract where
  toFastSketch op = case op of
    ExtractVector part src idx -> SketchExtractVector part src [idx]
    ExtractMask part src idx -> SketchExtractMask part src [idx]
    VectorTruncate part src -> SketchVectorTruncate part src

instance ScaleLMul Extract where
  scaleLMul ratio op = case op of
    ExtractVector part src idx -> ExtractVector (scaleLMul ratio part) (scaleLMul ratio src) idx
    ExtractMask part src idx -> ExtractMask (scaleLMul ratio part) (scaleLMul ratio src) idx
    VectorTruncate part src -> VectorTruncate (scaleLMul ratio part) (scaleLMul ratio src)

instance ScaleLMul SketchExtract where
  scaleLMul ratio op = case op of
    SketchExtractVector part src idx -> SketchExtractVector (scaleLMul ratio part) (scaleLMul ratio src) idx
    SketchExtractMask part src idx -> SketchExtractMask (scaleLMul ratio part) (scaleLMul ratio src) idx
    SketchVectorTruncate part src -> SketchVectorTruncate (scaleLMul ratio part) (scaleLMul ratio src)

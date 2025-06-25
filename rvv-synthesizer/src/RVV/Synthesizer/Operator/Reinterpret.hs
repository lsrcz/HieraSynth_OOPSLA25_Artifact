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

module RVV.Synthesizer.Operator.Reinterpret
  ( Reinterpret (..),
    SketchReinterpret (..),
    vectorToVector,
    sketchVectorToVector,
    maskToVector,
    sketchMaskToVector,
    vectorToMask,
    sketchVectorToMask,
    maskToMask,
    sketchMaskToMask,
    ptrToScalar,
    sketchPtrToScalar,
    scalarToPtr,
    sketchScalarToPtr,
    vlToScalar,
    sketchVLToScalar,
  )
where

import Control.Applicative ((<|>))
import Control.Lens (makeFieldsNoPrefix, (%~), (&))
import qualified Data.HashSet as HS
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
import HieraSynth.Util.Parser (betweenBrackets, bracketCommaSep2, named)
import Grisette.Unified (UnifiedSymEq)
import RVV.EvalMode (MonadEvalMode)
import RVV.Parser.ArgParser (maskMulParser, vectorConfigParser, widthMulParser)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Memory (BlockId)
import RVV.Semantics.Multiplier (MaskMul, WidthMul)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.VectorConfig (VectorConfig)
import RVV.Synthesizer.CostModel.CostModel (CostModel)
import RVV.Synthesizer.Feature.ExtractFeature (ExtractFeature (extractFeature))
import RVV.Synthesizer.Feature.FeatureSet
  ( Feature (GetSet, VLToScalarFeature),
    FeatureSet (opFeatures),
    maskMulFeature,
    vectorConfigFeature,
  )
import RVV.Synthesizer.Lens
  ( HasBlockId (blockId),
    HasMaskMul (maskMul),
    HasMaskMulDest (maskMulDest),
    HasMaskMulSrc (maskMulSrc),
    HasVectorConfigDest (vectorConfigDest),
    HasVectorConfigSrc (vectorConfigSrc),
    HasWidthMul (widthMul),
  )
import RVV.Synthesizer.OpSemantics.Convert
  ( applyPtrToScalar,
    applyScalarToPtr,
    applyVLToScalar,
    typePtrToScalar,
    typeScalarToPtr,
    typeVLToScalar,
  )
import RVV.Synthesizer.OpSemantics.Reinterpret
  ( applyMaskCast,
    applyMaskToVector,
    applyVectorToMask,
    applyVectorToVector,
    typeMaskCast,
    typeMaskToVector,
    typeVectorToMask,
    typeVectorToVector,
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
    noModeDeriveConfig,
  )
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Char.Lexer as L (decimal)

data Reinterpret
  = VectorToVector
      { _vectorConfigSrc :: VectorConfig,
        _vectorConfigDest :: VectorConfig
      }
  | MaskToVector
      { _maskMulSrc :: MaskMul,
        _vectorConfigDest :: VectorConfig
      }
  | VectorToMask
      { _vectorConfigSrc :: VectorConfig,
        _maskMulDest :: MaskMul
      }
  | MaskToMask
      { _maskMulSrc :: MaskMul,
        _maskMulDest :: MaskMul
      }
  | PtrToScalar {_widthMul :: WidthMul, _blockId :: BlockId}
  | ScalarToPtr {_widthMul :: WidthMul, _blockId :: BlockId}
  | VLToScalar {_maskMul :: MaskMul}

data SketchReinterpret
  = SketchVectorToVector
      { _vectorConfigSrc :: VectorConfig,
        _vectorConfigDest :: VectorConfig
      }
  | SketchMaskToVector
      { _maskMulSrc :: MaskMul,
        _vectorConfigDest :: VectorConfig
      }
  | SketchVectorToMask
      { _vectorConfigSrc :: VectorConfig,
        _maskMulDest :: MaskMul
      }
  | SketchMaskToMask
      { _maskMulSrc :: MaskMul,
        _maskMulDest :: MaskMul
      }
  | SketchPtrToScalar {_widthMul :: WidthMul, _blockId :: BlockId}
  | SketchScalarToPtr {_widthMul :: WidthMul, _blockId :: BlockId}
  | SketchVLToScalar {_maskMul :: MaskMul}

deriveFullExcept
  noModeDeriveConfig
  [''Reinterpret]
  [''UnifiedSymEq]
deriveNoSymEval [''SketchReinterpret]
makeFieldsNoPrefix ''Reinterpret
makeFieldsNoPrefix ''SketchReinterpret

vectorToVector ::
  (Reinterpret :<: op) =>
  VectorConfig ->
  VectorConfig ->
  op
vectorToVector src dest = inj $ VectorToVector src dest

sketchVectorToVector ::
  (SketchReinterpret :<: op) =>
  VectorConfig ->
  VectorConfig ->
  op
sketchVectorToVector src dest = inj $ SketchVectorToVector src dest

maskToVector ::
  (Reinterpret :<: op) =>
  MaskMul ->
  VectorConfig ->
  op
maskToVector src dest = inj $ MaskToVector src dest

sketchMaskToVector ::
  (SketchReinterpret :<: op) =>
  MaskMul ->
  VectorConfig ->
  op
sketchMaskToVector src dest = inj $ SketchMaskToVector src dest

vectorToMask ::
  (Reinterpret :<: op) =>
  VectorConfig ->
  MaskMul ->
  op
vectorToMask src dest = inj $ VectorToMask src dest

sketchVectorToMask ::
  (SketchReinterpret :<: op) =>
  VectorConfig ->
  MaskMul ->
  op
sketchVectorToMask src dest = inj $ SketchVectorToMask src dest

maskToMask ::
  (Reinterpret :<: op) =>
  MaskMul ->
  MaskMul ->
  op
maskToMask src dest = inj $ MaskToMask src dest

sketchMaskToMask ::
  (SketchReinterpret :<: op) =>
  MaskMul ->
  MaskMul ->
  op
sketchMaskToMask src dest = inj $ SketchMaskToMask src dest

ptrToScalar ::
  (Reinterpret :<: op) =>
  WidthMul ->
  BlockId ->
  op
ptrToScalar xmul blockId = inj $ PtrToScalar xmul blockId

sketchPtrToScalar ::
  (SketchReinterpret :<: op) =>
  WidthMul ->
  BlockId ->
  op
sketchPtrToScalar xmul blockId = inj $ SketchPtrToScalar xmul blockId

scalarToPtr ::
  (Reinterpret :<: op) =>
  WidthMul ->
  BlockId ->
  op
scalarToPtr xmul blockId = inj $ ScalarToPtr xmul blockId

sketchScalarToPtr ::
  (SketchReinterpret :<: op) =>
  WidthMul ->
  BlockId ->
  op
sketchScalarToPtr xmul blockId = inj $ SketchScalarToPtr xmul blockId

vlToScalar ::
  (Reinterpret :<: op) =>
  MaskMul ->
  op
vlToScalar maskMul = inj $ VLToScalar maskMul

sketchVLToScalar ::
  (SketchReinterpret :<: op) =>
  MaskMul ->
  op
sketchVLToScalar maskMul = inj $ SketchVLToScalar maskMul

instance
  (SemConstraint mode ctx, MonadEvalMode mode ctx) =>
  OpSemantics MachineConfig Reinterpret (Value mode) ctx
  where
  applyOp machine _ op inputs = case op of
    VectorToVector src dest -> applyVectorToVector machine src dest inputs
    MaskToVector src dest -> applyMaskToVector machine src dest inputs
    VectorToMask src dest -> applyVectorToMask machine src dest inputs
    MaskToMask src dest -> applyMaskCast machine src dest inputs
    PtrToScalar xmul blockId -> applyPtrToScalar machine xmul blockId inputs
    ScalarToPtr xmul blockId -> applyScalarToPtr machine xmul blockId inputs
    VLToScalar maskMul -> applyVLToScalar machine maskMul inputs

instance (MonadContext ctx) => OpTyping Reinterpret ctx where
  type OpTypeType Reinterpret = ValueType
  typeOp op = case op of
    VectorToVector src dest -> typeVectorToVector src dest
    MaskToVector src dest -> typeMaskToVector src dest
    VectorToMask src dest -> typeVectorToMask src dest
    MaskToMask src dest -> typeMaskCast src dest
    PtrToScalar xmul blockId -> typePtrToScalar xmul blockId
    ScalarToPtr xmul blockId -> typeScalarToPtr xmul blockId
    VLToScalar maskMul -> typeVLToScalar maskMul

instance (MonadAngelicContext ctx) => OpTyping SketchReinterpret ctx where
  type OpTypeType SketchReinterpret = ValueType
  typeOp = typeSketchOp @Reinterpret

instance OpSymmetryReduction Reinterpret where
  opUnreorderable = error "Should not be called"
  opCommutativeArgPos _ = mrgReturn []

instance GenSym SketchReinterpret Reinterpret where
  fresh (SketchVectorToVector src dest) = return $ mrgReturn $ VectorToVector src dest
  fresh (SketchMaskToVector src dest) = return $ mrgReturn $ MaskToVector src dest
  fresh (SketchVectorToMask src dest) = return $ mrgReturn $ VectorToMask src dest
  fresh (SketchMaskToMask src dest) = return $ mrgReturn $ MaskToMask src dest
  fresh (SketchPtrToScalar xmul blockId) = return $ mrgReturn $ PtrToScalar xmul blockId
  fresh (SketchScalarToPtr xmul blockId) = return $ mrgReturn $ ScalarToPtr xmul blockId
  fresh (SketchVLToScalar maskMul) = return $ mrgReturn $ VLToScalar maskMul

instance GenSym Reinterpret Reinterpret

instance GenSymSimple Reinterpret Reinterpret where
  simpleFresh = return . toSym

instance OpPPrint Reinterpret where
  describeArguments _ = return []
  prefixResults _ = return []

instance PPrint Reinterpret where
  pformat op = case op of
    VectorToVector src dest ->
      "vec_to_vec" <> pformatArgList (withName "src" src, withName "dest" dest)
    MaskToVector src dest ->
      "mask_to_vec" <> pformatArgList (withName "src" src, withName "dest" dest)
    VectorToMask src dest ->
      "vec_to_mask" <> pformatArgList (withName "src" src, withName "dest" dest)
    MaskToMask src dest ->
      "mask_to_mask" <> pformatArgList (withName "src" src, withName "dest" dest)
    PtrToScalar xmul blockId ->
      "ptr_to_scalar" <> pformatArgList (withName "xmul" xmul, withName "block" blockId)
    ScalarToPtr xmul blockId ->
      "scalar_to_ptr" <> pformatArgList (withName "xmul" xmul, withName "block" blockId)
    VLToScalar maskMul ->
      "vl_to_scalar" <> pformatArgList (withName "mmul" maskMul)

instance OpPPrint SketchReinterpret where
  describeArguments _ = return []
  prefixResults _ = return []

instance PPrint SketchReinterpret where
  pformat op = case op of
    SketchVectorToVector src dest ->
      "vec_to_vec" <> pformatArgList (withName "src" src, withName "dest" dest)
    SketchMaskToVector src dest ->
      "mask_to_vec" <> pformatArgList (withName "src" src, withName "dest" dest)
    SketchVectorToMask src dest ->
      "vec_to_mask" <> pformatArgList (withName "src" src, withName "dest" dest)
    SketchMaskToMask src dest ->
      "mask_to_mask" <> pformatArgList (withName "src" src, withName "dest" dest)
    SketchPtrToScalar xmul blockId ->
      "ptr_to_scalar" <> pformatArgList (withName "xmul" xmul, withName "block" blockId)
    SketchScalarToPtr xmul blockId ->
      "scalar_to_ptr" <> pformatArgList (withName "xmul" xmul, withName "block" blockId)
    SketchVLToScalar maskMul ->
      "vl_to_scalar" <> pformatArgList (withName "mmul" maskMul)

instance OpParser Reinterpret where
  opParser = vectorToVectorParser <|> maskToVectorParser <|> vectorToMaskParser <|> maskToMaskParser <|> ptrToScalarParser <|> scalarToPtrParser <|> vlToScalarParser
    where
      vectorToVectorParser = do
        string "vec_to_vec"
        (src, dest) <-
          bracketCommaSep2
            (named "src" vectorConfigParser)
            (named "dest" vectorConfigParser)
        return $ VectorToVector src dest
      maskToVectorParser = do
        string "mask_to_vec"
        (src, dest) <-
          bracketCommaSep2
            (named "src" maskMulParser)
            (named "dest" vectorConfigParser)
        return $ MaskToVector src dest
      vectorToMaskParser = do
        string "vec_to_mask"
        (src, dest) <-
          bracketCommaSep2
            (named "src" vectorConfigParser)
            (named "dest" maskMulParser)
        return $ VectorToMask src dest
      maskToMaskParser = do
        string "mask_to_mask"
        (src, dest) <-
          bracketCommaSep2
            (named "src" maskMulParser)
            (named "dest" maskMulParser)
        return $ MaskToMask src dest
      ptrToScalarParser = do
        string "ptr_to_scalar"
        (xmul, blockId) <-
          bracketCommaSep2
            (named "xmul" widthMulParser)
            (named "block" L.decimal)
        return $ PtrToScalar xmul blockId
      scalarToPtrParser = do
        string "scalar_to_ptr"
        (xmul, blockId) <-
          bracketCommaSep2
            (named "xmul" widthMulParser)
            (named "block" L.decimal)
        return $ ScalarToPtr xmul blockId
      vlToScalarParser = do
        string "vl_to_scalar"
        mmul <- betweenBrackets $ named "mmul" maskMulParser
        return $ VLToScalar mmul

instance OpParser SketchReinterpret where
  opParser = sketchVectorToVectorParser <|> sketchMaskToVectorParser <|> sketchVectorToMaskParser <|> sketchMaskToMaskParser <|> sketchPtrToScalarParser <|> sketchScalarToPtrParser <|> sketchVLToScalarParser
    where
      sketchVectorToVectorParser = do
        string "sketch_vec_to_vec"
        (src, dest) <-
          bracketCommaSep2
            (named "src" vectorConfigParser)
            (named "dest" vectorConfigParser)
        return $ SketchVectorToVector src dest
      sketchMaskToVectorParser = do
        string "sketch_mask_to_vec"
        (src, dest) <-
          bracketCommaSep2
            (named "src" maskMulParser)
            (named "dest" vectorConfigParser)
        return $ SketchMaskToVector src dest
      sketchVectorToMaskParser = do
        string "sketch_vec_to_mask"
        (src, dest) <-
          bracketCommaSep2
            (named "src" vectorConfigParser)
            (named "dest" maskMulParser)
        return $ SketchVectorToMask src dest
      sketchMaskToMaskParser = do
        string "sketch_mask_to_mask"
        (src, dest) <-
          bracketCommaSep2
            (named "src" maskMulParser)
            (named "dest" maskMulParser)
        return $ SketchMaskToMask src dest
      sketchPtrToScalarParser = do
        string "sketch_ptr_to_scalar"
        (xmul, blockId) <-
          bracketCommaSep2
            (named "xmul" widthMulParser)
            (named "block" L.decimal)
        return $ SketchPtrToScalar xmul blockId
      sketchScalarToPtrParser = do
        string "sketch_scalar_to_ptr"
        (xmul, blockId) <-
          bracketCommaSep2
            (named "xmul" widthMulParser)
            (named "block" L.decimal)
        return $ SketchScalarToPtr xmul blockId
      sketchVLToScalarParser = do
        string "sketch_vl_to_scalar"
        mmul <- named "mmul" maskMulParser
        return $ SketchVLToScalar mmul

instance OpReachableSymbols Reinterpret where
  opReachableSymbols = mempty

instance SplitChoice SketchReinterpret where
  splitChoice = return

instance
  (MonadContext ctx, Num cost, Mergeable cost) =>
  OpCost CostModel Reinterpret cost ctx
  where
  opCost _ _ _ = mrgReturn 0

instance ExtractFeature Reinterpret FeatureSet where
  extractFeature op = case op of
    VectorToVector src dest ->
      vectorConfigFeature src
        <> vectorConfigFeature dest
        <> mempty {opFeatures = HS.singleton GetSet}
    MaskToVector src dest ->
      maskMulFeature src
        <> vectorConfigFeature dest
        <> mempty {opFeatures = HS.singleton GetSet}
    VectorToMask src dest ->
      vectorConfigFeature src
        <> maskMulFeature dest
        <> mempty {opFeatures = HS.singleton GetSet}
    MaskToMask src dest ->
      maskMulFeature src
        <> maskMulFeature dest
        <> mempty {opFeatures = HS.singleton GetSet}
    PtrToScalar _ _ -> error "Not implemented"
    ScalarToPtr _ _ -> error "Not implemented"
    VLToScalar mmul ->
      maskMulFeature mmul <> mempty {opFeatures = HS.singleton VLToScalarFeature}

instance ToFastSketch Reinterpret SketchReinterpret where
  toFastSketch op = case op of
    VectorToVector src dest -> SketchVectorToVector src dest
    MaskToVector src dest -> SketchMaskToVector src dest
    VectorToMask src dest -> SketchVectorToMask src dest
    MaskToMask src dest -> SketchMaskToMask src dest
    PtrToScalar xmul blockId -> SketchPtrToScalar xmul blockId
    ScalarToPtr xmul blockId -> SketchScalarToPtr xmul blockId
    VLToScalar maskMul -> SketchVLToScalar maskMul

instance ScaleLMul Reinterpret where
  scaleLMul ratio op = op & (maskMul %~ scaleLMul ratio)

instance ScaleLMul SketchReinterpret where
  scaleLMul ratio op = op & (maskMul %~ scaleLMul ratio)

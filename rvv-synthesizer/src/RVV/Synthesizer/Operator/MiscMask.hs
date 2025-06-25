{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.Operator.MiscMask
  ( MiscMask (..),
    SketchMiscMask (..),
    maskPopCount,
    sketchMaskPopCount,
    maskFirstOne,
    sketchMaskFirstOne,
    maskSetBit,
    sketchMaskSetBit,
  )
where

import Control.Applicative ((<|>))
import Control.Lens (makeFieldsNoPrefix, (%~), (&))
import Data.Foldable (Foldable (toList))
import qualified Data.HashSet as HS
import Grisette
  ( GenSym (fresh),
    GenSymSimple (simpleFresh),
    Mergeable,
    PPrint (pformat),
    ToSym (toSym),
    chooseFresh,
    mrgReturn,
  )
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
import HieraSynth.Util.Parser (bracketCommaSep2, bracketCommaSep3, named)
import Grisette.Unified (EvalModeTag (C, S), GetData, UnifiedSymEq, extractData, EvalModeConvertible)
import RVV.EvalMode (MonadEvalMode)
import RVV.Parser.ArgParser (destinationParser, maskMulParser, maskingParser)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Multiplier (MaskMul)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Synthesizer.CostModel.CostModel (CostModel (CostModel, maskOpCost))
import RVV.Synthesizer.Feature.ExtractFeature (ExtractFeature (extractFeature))
import RVV.Synthesizer.Feature.FeatureSet
  ( Feature (SetMask),
    FeatureSet (opFeatures),
    maskMulFeature,
  )
import RVV.Synthesizer.Lens
  ( HasDestination (destination),
    HasMaskMul (maskMul),
    HasMasking (masking),
    HasSetMaskMethod (setMaskMethod),
  )
import RVV.Synthesizer.OpSemantics.MiscMask
  ( applyVCPop,
    applyVFirst,
    applyVSetMask,
    typeVCPop,
    typeVFirst,
    typeVSetMask,
  )
import RVV.Synthesizer.Operator.Common.PPrint
  ( PPrintArgList (pformatArgList),
    choices,
    pformatChoices,
    withName,
  )
import RVV.Synthesizer.Operator.Common.TypeSketchOp (typeSketchOp)
import RVV.Synthesizer.Parameter.Destination (Destination)
import RVV.Synthesizer.Parameter.Masking (Masking)
import RVV.Synthesizer.Parameter.SetMaskMethod
  ( SetMaskMethod,
    setCurBitWithMethod,
    setMaskMethodParser,
  )
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

-- | Operators for mask manipulation
data MiscMask mode
  = MaskPopCount
      { _maskMul :: MaskMul,
        _masking :: GetData mode Masking
      }
  | MaskFirstOne
      { _maskMul :: MaskMul,
        _masking :: GetData mode Masking
      }
  | MaskSetBit
      { _setMaskMethod :: GetData mode SetMaskMethod,
        _maskMul :: MaskMul,
        _destination :: GetData mode Destination,
        _masking :: GetData mode Masking
      }

-- | Sketch version of MiscMask operators
data SketchMiscMask
  = SketchMaskPopCount
      { _maskMul :: MaskMul,
        _masking :: [Masking]
      }
  | SketchMaskFirstOne
      { _maskMul :: MaskMul,
        _masking :: [Masking]
      }
  | SketchMaskSetBit
      { _setMaskMethod :: [SetMaskMethod],
        _maskMul :: MaskMul,
        _destination :: [Destination],
        _masking :: [Masking]
      }

deriveFullExcept
  firstModeDeriveConfig
  [''MiscMask]
  [''UnifiedSymEq]
deriveNoSymEval [''SketchMiscMask]
makeFieldsNoPrefix ''MiscMask
makeFieldsNoPrefix ''SketchMiscMask

-- Smart constructors
maskPopCount ::
  (MiscMask mode :<: op) =>
  MaskMul ->
  GetData mode Masking ->
  op
maskPopCount mmul m = inj $ MaskPopCount mmul m

sketchMaskPopCount ::
  (SketchMiscMask :<: op) =>
  MaskMul ->
  [Masking] ->
  op
sketchMaskPopCount mmul m = inj $ SketchMaskPopCount mmul m

maskFirstOne ::
  (MiscMask mode :<: op) =>
  MaskMul ->
  GetData mode Masking ->
  op
maskFirstOne mmul m = inj $ MaskFirstOne mmul m

sketchMaskFirstOne ::
  (SketchMiscMask :<: op) =>
  MaskMul ->
  [Masking] ->
  op
sketchMaskFirstOne mmul m = inj $ SketchMaskFirstOne mmul m

maskSetBit ::
  (MiscMask mode :<: op) =>
  GetData mode SetMaskMethod ->
  MaskMul ->
  GetData mode Destination ->
  GetData mode Masking ->
  op
maskSetBit method mmul dest m = inj $ MaskSetBit method mmul dest m

sketchMaskSetBit ::
  (SketchMiscMask :<: op) =>
  [SetMaskMethod] ->
  MaskMul ->
  [Destination] ->
  [Masking] ->
  op
sketchMaskSetBit method mmul dest m = inj $ SketchMaskSetBit method mmul dest m

-- Type class instances
instance
  ( SemConstraint mode ctx,
    EvalModeConvertible opMode mode,
    MonadEvalMode opMode ctx
  ) =>
  OpSemantics MachineConfig (MiscMask opMode) (Value mode) ctx
  where
  applyOp machine _ op inputs = case toSym op of
    MaskPopCount mmul m -> applyVCPop machine mmul m inputs
    MaskFirstOne mmul m -> applyVFirst machine mmul m inputs
    MaskSetBit method mmul dest m -> do
      method <- extractData method
      applyVSetMask (setCurBitWithMethod method) machine mmul dest m inputs

instance (SemConstraint mode ctx) => OpTyping (MiscMask mode) ctx where
  type OpTypeType (MiscMask mode) = ValueType
  typeOp op = case op of
    MaskPopCount mmul m -> typeVCPop mmul m
    MaskFirstOne mmul m -> typeVFirst mmul m
    MaskSetBit _ mmul dest m -> typeVSetMask mmul dest m

instance (MonadAngelicContext ctx) => OpTyping SketchMiscMask ctx where
  type OpTypeType SketchMiscMask = ValueType
  typeOp = typeSketchOp @(MiscMask 'S)

instance OpSymmetryReduction (MiscMask mode) where
  opUnreorderable = error "Should not be called"
  opCommutativeArgPos _ = mrgReturn []

instance GenSym SketchMiscMask (MiscMask 'S) where
  fresh (SketchMaskPopCount mmul masking) = do
    masking <- chooseFresh masking
    return $ mrgReturn $ MaskPopCount mmul masking
  fresh (SketchMaskFirstOne mmul masking) = do
    masking <- chooseFresh masking
    return $ mrgReturn $ MaskFirstOne mmul masking
  fresh (SketchMaskSetBit method mmul dest masking) = do
    method <- chooseFresh method
    dest <- chooseFresh dest
    masking <- chooseFresh masking
    return $ mrgReturn $ MaskSetBit method mmul dest masking

instance GenSym (MiscMask 'C) (MiscMask 'S)

instance GenSymSimple (MiscMask 'C) (MiscMask 'S) where
  simpleFresh = return . toSym

instance OpPPrint (MiscMask 'C) where
  describeArguments _ = return []
  prefixResults _ = return []

instance PPrint (MiscMask 'C) where
  pformat op = case op of
    MaskPopCount mmul m ->
      "vcpop.m" <> pformatArgList (withName "mmul" mmul, m)
    MaskFirstOne mmul m ->
      "vfirst.m" <> pformatArgList (withName "mmul" mmul, m)
    MaskSetBit method mmul dest m ->
      "vm"
        <> pformat method
        <> ".m"
        <> pformatArgList (withName "mmul" mmul, dest, m)

instance OpPPrint SketchMiscMask where
  describeArguments _ = return []
  prefixResults _ = return []

instance PPrint SketchMiscMask where
  pformat op = case op of
    SketchMaskPopCount mmul m ->
      "vcpop.m" <> pformatArgList (withName "mmul" mmul, choices m)
    SketchMaskFirstOne mmul m ->
      "vfirst.m" <> pformatArgList (withName "mmul" mmul, choices m)
    SketchMaskSetBit method mmul dest m ->
      "vm"
        <> pformatChoices method
        <> ".m"
        <> pformatArgList (withName "mmul" mmul, choices dest, choices m)

instance OpParser (MiscMask 'C) where
  opParser = maskPopCountParser <|> maskFirstOneParser <|> maskSetBitParser
    where
      maskPopCountParser = do
        string "vcpop.m"
        (mmul, m) <-
          bracketCommaSep2
            (named "mmul" maskMulParser)
            maskingParser
        return $ MaskPopCount mmul m
      maskFirstOneParser = do
        string "vfirst.m"
        (mmul, m) <-
          bracketCommaSep2
            (named "mmul" maskMulParser)
            maskingParser
        return $ MaskFirstOne mmul m
      maskSetBitParser = do
        method <- setMaskMethodParser ".m"
        (mmul, dest, m) <-
          bracketCommaSep3
            (named "mmul" maskMulParser)
            destinationParser
            maskingParser
        return $ MaskSetBit method mmul dest m

instance OpReachableSymbols (MiscMask 'C) where
  opReachableSymbols = mempty

instance SplitChoice SketchMiscMask where
  splitChoice (SketchMaskPopCount mmul m) = do
    m <- m
    return $ SketchMaskPopCount mmul [m]
  splitChoice (SketchMaskFirstOne mmul m) = do
    m <- m
    return $ SketchMaskFirstOne mmul [m]
  splitChoice (SketchMaskSetBit method mmul dest m) = do
    method <- method
    dest <- dest
    m <- m
    return $ SketchMaskSetBit [method] mmul [dest] [m]

instance
  (SemConstraint mode ctx, Num cost, Mergeable cost) =>
  OpCost CostModel (MiscMask mode) cost ctx
  where
  opCost CostModel {..} _ op = case op of
    MaskPopCount mmul _ -> mrgReturn $ fromIntegral $ maskOpCost mmul
    MaskFirstOne mmul _ -> mrgReturn $ fromIntegral $ maskOpCost mmul
    MaskSetBit _ mmul _ _ -> mrgReturn $ fromIntegral $ maskOpCost mmul

instance ExtractFeature (MiscMask 'C) FeatureSet where
  extractFeature op = case op of
    MaskPopCount mmul _ ->
      maskMulFeature mmul
    MaskFirstOne mmul _ ->
      maskMulFeature mmul
    MaskSetBit _ mmul _ _ ->
      maskMulFeature mmul
        <> mempty {opFeatures = HS.singleton SetMask}

instance ToFastSketch (MiscMask 'C) SketchMiscMask where
  toFastSketch op = case op of
    MaskPopCount mmul m -> SketchMaskPopCount mmul (toList m)
    MaskFirstOne mmul m -> SketchMaskFirstOne mmul (toList m)
    MaskSetBit method mmul dest m ->
      SketchMaskSetBit (toList method) mmul (toList dest) (toList m)

instance ScaleLMul (MiscMask mode) where
  scaleLMul ratio op = op & maskMul %~ scaleLMul ratio

instance ScaleLMul SketchMiscMask where
  scaleLMul ratio op = op & maskMul %~ scaleLMul ratio

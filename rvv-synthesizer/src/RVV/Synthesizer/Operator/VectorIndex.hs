{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.Operator.VectorIndex
  ( vectorId,
    vectorIota,
    sketchVectorId,
    sketchVectorIota,
    VectorIndex (..),
    SketchVectorIndex (..),
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Lens (Identity (Identity), makeFieldsNoPrefix)
import Data.Foldable (Foldable (toList))
import qualified Data.HashSet as HS
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
import HieraSynth.Combinator.Embed (type (:<:) (inj))
import HieraSynth.Context (MonadAngelicContext)
import HieraSynth.Operator.OpParser (OpParser (opParser))
import HieraSynth.Operator.OpReachableSymbols
  ( OpReachableSymbols (opReachableSymbols),
  )
import HieraSynth.Operator.OpSemantics (OpSemantics (applyOp))
import HieraSynth.Operator.OpTyping (OpTyping (OpTypeType, typeOp))
import HieraSynth.Program.Choice.Counting
  ( SplitChoice (splitChoice),
  )
import HieraSynth.Program.ComponentSketch.SymmetryReduction
  ( OpSymmetryReduction (opCommutativeArgPos, opUnreorderable),
  )
import HieraSynth.Program.Concrete
  ( OpPPrint (describeArguments, prefixResults),
  )
import HieraSynth.Program.CostModel.PerStmtCostModel (OpCost (opCost))
import HieraSynth.Util.Parser (comma, leftBracket, rightBracket)
import Grisette.Unified
  ( EvalModeConvertible,
    EvalModeTag (C, S),
    GetData,
    UnifiedSymEq,
  )
import RVV.EvalMode (EvalMode, MonadEvalMode)
import RVV.Parser.ArgParser (destinationParser, maskingParser, vectorConfigParser)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.VectorConfig (VectorConfig)
import RVV.Synthesizer.CostModel.CostModel
  ( CostModel (CostModel, idCost, iotaCost),
  )
import RVV.Synthesizer.Feature.ExtractFeature (ExtractFeature (extractFeature))
import RVV.Synthesizer.Feature.FeatureSet
  ( Feature (Masking),
    FeatureSet (opFeatures),
    maskingFeature,
    vectorConfigFeature,
  )
import RVV.Synthesizer.Lens
  ( HasDestination (destination),
    HasMasking (masking),
    HasVectorConfig (vectorConfig),
  )
import RVV.Synthesizer.OpSemantics.Iota
  ( applyVId,
    applyVIota,
    typeVId,
    typeVIota,
  )
import RVV.Synthesizer.Operator.Common.PPrint
  ( PPrintArgList (pformatArgList),
    choices,
  )
import RVV.Synthesizer.Operator.Common.TypeSketchOp (typeSketchOp)
import RVV.Synthesizer.Parameter.Destination (Destination)
import RVV.Synthesizer.Parameter.Masking (Masking)
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

data VectorIndex mode
  = VectorId
      { _vectorConfig :: VectorConfig,
        _destination :: GetData mode Destination,
        _masking :: GetData mode Masking
      }
  | VectorIota
      { _vectorConfig :: VectorConfig,
        _destination :: GetData mode Destination,
        _masking :: GetData mode Masking
      }

vectorId ::
  (VectorIndex mode :<: op) =>
  VectorConfig ->
  GetData mode Destination ->
  GetData mode Masking ->
  op
vectorId vtype dest mask =
  inj $ VectorId vtype dest mask

vectorIota ::
  (VectorIndex mode :<: op) =>
  VectorConfig ->
  GetData mode Destination ->
  GetData mode Masking ->
  op
vectorIota vtype dest mask =
  inj $ VectorIota vtype dest mask

data SketchVectorIndex
  = SketchVectorId
      { _vectorConfig :: VectorConfig,
        _destination :: [Destination],
        _masking :: [Masking]
      }
  | SketchVectorIota
      { _vectorConfig :: VectorConfig,
        _destination :: [Destination],
        _masking :: [Masking]
      }

sketchVectorId ::
  (SketchVectorIndex :<: op) =>
  VectorConfig ->
  [Destination] ->
  [Masking] ->
  op
sketchVectorId config dest mask =
  inj $ SketchVectorId config dest mask

sketchVectorIota ::
  (SketchVectorIndex :<: op) =>
  VectorConfig ->
  [Destination] ->
  [Masking] ->
  op
sketchVectorIota config dest mask =
  inj $ SketchVectorIota config dest mask

deriveFullExcept
  firstModeDeriveConfig
  [''VectorIndex]
  [''UnifiedSymEq]
deriveNoSymEval [''SketchVectorIndex]
makeFieldsNoPrefix ''VectorIndex
makeFieldsNoPrefix ''SketchVectorIndex

instance
  ( SemConstraint mode ctx,
    EvalModeConvertible opMode mode,
    MonadEvalMode opMode ctx,
    UnifiedSymEq mode Destination,
    UnifiedSymEq mode Masking
  ) =>
  OpSemantics MachineConfig (VectorIndex opMode) (Value mode) ctx
  where
  applyOp vconst _ op inputs = do
    case toSym op of
      VectorId vtype dest mask -> do
        applyVId vconst vtype dest mask inputs
      VectorIota vtype dest mask -> do
        applyVIota vconst vtype dest mask inputs

instance
  (SemConstraint mode ctx) =>
  OpTyping (VectorIndex mode) ctx
  where
  type OpTypeType (VectorIndex mode) = ValueType
  typeOp (VectorId vtype destination masking) =
    typeVId vtype destination masking
  typeOp (VectorIota vtype destination masking) =
    typeVIota vtype destination masking

instance
  (MonadAngelicContext ctx) =>
  OpTyping SketchVectorIndex ctx
  where
  type OpTypeType SketchVectorIndex = ValueType
  typeOp = typeSketchOp @(VectorIndex 'S)

instance
  (MonadEvalMode mode Union) =>
  OpSymmetryReduction (VectorIndex mode)
  where
  opUnreorderable = error "Should not be called"
  opCommutativeArgPos _ = mrgReturn []

instance GenSym SketchVectorIndex (VectorIndex 'S)

instance GenSymSimple SketchVectorIndex (VectorIndex 'S) where
  simpleFresh (SketchVectorId vtype dest mask) = do
    dest <- chooseFresh dest
    mask <- chooseFresh mask
    return $ VectorId vtype dest mask
  simpleFresh (SketchVectorIota vtype dest mask) = do
    dest <- chooseFresh dest
    mask <- chooseFresh mask
    return $ VectorIota vtype dest mask

instance GenSym (VectorIndex 'C) (VectorIndex 'S)

instance
  GenSymSimple
    (VectorIndex 'C)
    (VectorIndex 'S)
  where
  simpleFresh = return . toSym

instance OpPPrint (VectorIndex 'C) where
  describeArguments _ = return []

instance PPrint (VectorIndex 'C) where
  pformat (VectorId vtype dest mask) =
    "vid.v" <> pformatArgList (vtype, dest, mask)
  pformat (VectorIota vtype dest mask) =
    "viota.m" <> pformatArgList (vtype, dest, mask)

instance OpPPrint SketchVectorIndex where
  describeArguments _ = return []
  prefixResults _ = return []

instance PPrint SketchVectorIndex where
  pformat (SketchVectorId vtype dest mask) =
    "vid.v" <> pformatArgList (vtype, choices dest, choices mask)
  pformat (SketchVectorIota vtype dest mask) =
    "viota.m" <> pformatArgList (vtype, choices dest, choices mask)

instance OpParser (VectorIndex 'C) where
  opParser = do
    op <- (string "vid.v" >> return VectorId) <|> (string "viota.m" >> return VectorIota)
    leftBracket
    vtype <- vectorConfigParser
    comma
    dest <- destinationParser
    comma
    mask <- maskingParser
    rightBracket
    return $ op vtype dest mask

instance OpReachableSymbols (VectorIndex 'C) where
  opReachableSymbols = mempty

instance SplitChoice SketchVectorIndex where
  splitChoice (SketchVectorId vtype dest mask) = do
    dest <- dest
    mask <- mask
    return $ SketchVectorId vtype [dest] [mask]
  splitChoice (SketchVectorIota vtype dest mask) = do
    dest <- dest
    mask <- mask
    return $ SketchVectorIota vtype [dest] [mask]

instance
  (SemConstraint mode ctx, Num cost, Mergeable cost) =>
  OpCost CostModel (VectorIndex mode) cost ctx
  where
  opCost CostModel {..} _ op = do
    mrgReturn $ fromIntegral $ case op of
      VectorId {..} -> idCost _vectorConfig
      VectorIota {..} -> iotaCost _vectorConfig

instance (ExtractFeature (VectorIndex 'C) FeatureSet) where
  extractFeature op =
    case op of
      VectorId config _ (Identity masking) ->
        vectorConfigFeature config
          <> maskingFeature masking
          <> mempty {opFeatures = HS.singleton Masking}
      VectorIota config _ (Identity masking) ->
        vectorConfigFeature config
          <> maskingFeature masking
          <> mempty {opFeatures = HS.singleton Masking}

instance
  ToFastSketch
    (VectorIndex 'C)
    SketchVectorIndex
  where
  toFastSketch op =
    case op of
      VectorId vtype dest mask ->
        SketchVectorId vtype (toList dest) (toList mask)
      VectorIota vtype dest mask ->
        SketchVectorIota vtype (toList dest) (toList mask)

instance (EvalMode mode) => ScaleLMul (VectorIndex mode) where
  scaleLMul ratio op = case op of
    VectorId {..} -> VectorId (scaleLMul ratio _vectorConfig) _destination _masking
    VectorIota {..} -> VectorIota (scaleLMul ratio _vectorConfig) _destination _masking

instance ScaleLMul SketchVectorIndex where
  scaleLMul ratio op = case op of
    SketchVectorId {..} -> SketchVectorId (scaleLMul ratio _vectorConfig) _destination _masking
    SketchVectorIota {..} -> SketchVectorIota (scaleLMul ratio _vectorConfig) _destination _masking

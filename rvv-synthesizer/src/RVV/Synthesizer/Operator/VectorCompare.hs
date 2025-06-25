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

module RVV.Synthesizer.Operator.VectorCompare
  ( vectorCompare,
    sketchVectorCompare,
    VectorCompare (..),
    SketchVectorCompare (..),
  )
where

import Control.Applicative ((<|>))
import Control.Lens (Identity (Identity), makeFieldsNoPrefix, (%~), (&))
import Data.Foldable (Foldable (toList))
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
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
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
import HieraSynth.Program.ComponentSketch
  ( OpSymmetryReduction (opCommutativeArgPos, opUnreorderable),
  )
import HieraSynth.Program.Concrete
  ( OpPPrint (describeArguments, prefixResults),
  )
import HieraSynth.Program.CostModel.PerStmtCostModel (OpCost (opCost))
import HieraSynth.Util.Parser (comma, leftBracket, named, rightBracket)
import Grisette.Unified
  ( EvalModeConvertible,
    EvalModeTag (C, S),
    GetData,
    UnifiedSymEq,
    extractData,
  )
import RVV.EvalMode (EvalMode, MonadEvalMode)
import RVV.Parser.ArgParser
  ( destinationParser,
    maskingParser,
    vectorConfigParser,
  )
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.VectorConfig (VectorConfig)
import RVV.Synthesizer.CostModel.CostModel (CostModel (CostModel, compareCost))
import RVV.Synthesizer.Feature.ExtractFeature (ExtractFeature (extractFeature))
import RVV.Synthesizer.Feature.FeatureSet
  ( FeatureSet,
    maskingFeature,
    vectorConfigFeature,
  )
import RVV.Synthesizer.Lens
  ( HasDestination (destination),
    HasIntCompareOpCode (intCompareOpCode),
    HasMasking (masking),
    HasRhs (rhs),
    HasVectorConfig (vectorConfig),
  )
import RVV.Synthesizer.OpSemantics.Compare
  ( applyCompare,
    typeCompareVI,
    typeCompareVV,
    typeCompareVX,
  )
import RVV.Synthesizer.Operator.Common.OpSymmetryReduction
  ( checkOpCommutativeArgPos,
  )
import RVV.Synthesizer.Operator.Common.PPrint
  ( PPrintArgList (pformatArgList),
    choices,
    pformatChoices,
    withName,
  )
import RVV.Synthesizer.Operator.Common.Parser (immParser)
import RVV.Synthesizer.Operator.Common.RHSSpec
  ( RHSSpec (ImmRHS, ScalarRHS, VectorRHS),
    SketchRHSSpec,
    augmentElementWiseVIArgList,
    augmentElementWiseVXArgList,
  )
import RVV.Synthesizer.Operator.Common.TypeSketchOp (typeSketchOp)
import RVV.Synthesizer.Parameter.Destination (Destination)
import RVV.Synthesizer.Parameter.IntCompareOpCode
  ( IntCompareOpCode,
    commutativeIntCompareOpCode,
    intCompareOpCodeFeature,
    intCompareOpCodeParser,
    interpretIntCompareOpCode,
  )
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

data VectorCompare mode
  = VectorCompare
  { _vectorConfig :: VectorConfig,
    _intCompareOpCode :: GetData mode IntCompareOpCode,
    _destination :: GetData mode Destination,
    _masking :: GetData mode Masking,
    _rhs :: RHSSpec mode
  }

vectorCompare ::
  (VectorCompare mode :<: op) =>
  VectorConfig ->
  GetData mode IntCompareOpCode ->
  GetData mode Destination ->
  GetData mode Masking ->
  RHSSpec mode ->
  op
vectorCompare vtype op dest mask rhs =
  inj $ VectorCompare vtype op dest mask rhs

data SketchVectorCompare
  = SketchVectorCompare
  { _vectorConfig :: VectorConfig,
    _intCompareOpCode :: [IntCompareOpCode],
    _destination :: [Destination],
    _masking :: [Masking],
    _rhs :: SketchRHSSpec
  }

sketchVectorCompare ::
  (SketchVectorCompare :<: op) =>
  VectorConfig ->
  [IntCompareOpCode] ->
  [Destination] ->
  [Masking] ->
  SketchRHSSpec ->
  op
sketchVectorCompare config op dest mask rhs =
  inj $ SketchVectorCompare config op dest mask rhs

deriveFullExcept
  firstModeDeriveConfig
  [''VectorCompare]
  [''UnifiedSymEq]
deriveNoSymEval [''SketchVectorCompare]
makeFieldsNoPrefix ''VectorCompare
makeFieldsNoPrefix ''SketchVectorCompare

instance
  ( SemConstraint mode ctx,
    EvalModeConvertible opMode mode,
    MonadEvalMode opMode ctx
  ) =>
  OpSemantics MachineConfig (VectorCompare opMode) (Value mode) ctx
  where
  applyOp vconst _ op inputs = do
    case toSym op of
      VectorCompare vtype binOp dest mask VectorRHS -> do
        binOp <- extractData binOp
        applyCompare vconst vtype (interpretIntCompareOpCode binOp) dest mask inputs
      VectorCompare vtype binOp dest mask ScalarRHS -> do
        binOp <- extractData binOp
        newInputs <- augmentElementWiseVXArgList vconst vtype 2 inputs
        applyCompare vconst vtype (interpretIntCompareOpCode binOp) dest mask newInputs
      VectorCompare vtype binOp dest mask (ImmRHS imm) -> do
        binOp <- extractData binOp
        newInputs <- augmentElementWiseVIArgList vconst vtype imm 2 inputs
        applyCompare vconst vtype (interpretIntCompareOpCode binOp) dest mask newInputs
      VectorCompare {} ->
        mrgThrowError "VectorCompare: rhs should be VectorRHS, ScalarRHS or ImmRHS"

instance
  (SemConstraint mode ctx) =>
  OpTyping (VectorCompare mode) ctx
  where
  type OpTypeType (VectorCompare mode) = ValueType
  typeOp
    (VectorCompare vtype _ destination masking VectorRHS) =
      typeCompareVV vtype destination masking
  typeOp
    (VectorCompare vtype _ destination masking ScalarRHS) =
      typeCompareVX vtype destination masking
  typeOp
    (VectorCompare vtype _ destination masking (ImmRHS _)) =
      typeCompareVI vtype destination masking
  typeOp (VectorCompare {}) =
    mrgThrowError "VectorCompare: rhs should be VectorRHS, ScalarRHS or ImmRHS"

instance
  (MonadAngelicContext ctx) =>
  OpTyping SketchVectorCompare ctx
  where
  type OpTypeType SketchVectorCompare = ValueType
  typeOp = typeSketchOp @(VectorCompare 'S)

instance
  (MonadEvalMode mode Union) =>
  OpSymmetryReduction (VectorCompare mode)
  where
  opUnreorderable = error "Should not be called"
  opCommutativeArgPos (VectorCompare _ cop _ _ VectorRHS) =
    checkOpCommutativeArgPos
      cop
      commutativeIntCompareOpCode
      [[1, 2]]
  opCommutativeArgPos _ = mrgReturn []

instance GenSym SketchVectorCompare (VectorCompare 'S)

instance GenSymSimple SketchVectorCompare (VectorCompare 'S) where
  simpleFresh (SketchVectorCompare vtype op dest mask rhs) = do
    dest <- chooseFresh dest
    op <- chooseFresh op
    mask <- chooseFresh mask
    rhs <- simpleFresh rhs
    return $ VectorCompare vtype op dest mask rhs

instance GenSym (VectorCompare 'C) (VectorCompare 'S)

instance
  GenSymSimple
    (VectorCompare 'C)
    (VectorCompare 'S)
  where
  simpleFresh = return . toSym

instance OpPPrint (VectorCompare 'C) where
  describeArguments _ = return []

instance PPrint (VectorCompare 'C) where
  pformat (VectorCompare vtype op dest mask rhs) =
    "v"
      <> pformat op
      <> "."
      <> postFix
      <> case rhs of
        ImmRHS imm -> pformatArgList (vtype, dest, mask, withName "rhs" imm)
        _ -> pformatArgList (vtype, dest, mask)
    where
      postFix = case rhs of
        ImmRHS _ -> "vi"
        ScalarRHS -> "vx"
        VectorRHS -> "vv"
        _ -> error "VectorCompare: rhs should be VectorRHS, ScalarRHS or ImmRHS"

instance OpPPrint SketchVectorCompare where
  describeArguments _ = return []
  prefixResults _ = return []

instance PPrint SketchVectorCompare where
  pformat (SketchVectorCompare vtype op dest mask rhs) =
    "v"
      <> pformatChoices op
      <> pformatArgList (vtype, choices dest, choices mask, rhs)

instance OpParser (VectorCompare 'C) where
  opParser = do
    (op, rhs) <- attrWithOp
    leftBracket
    vtype <- vectorConfigParser
    comma
    dest <- destinationParser
    comma
    mask <- maskingParser
    rhs <- case rhs of
      ImmRHS _ -> do
        comma
        imm <- named "rhs" immParser
        return $ ImmRHS imm
      _ -> return rhs
    rightBracket
    return $ VectorCompare vtype op dest mask rhs
    where
      attrWithOp = do
        op <- intCompareOpCodeParser ".v"
        rhs <-
          (string "i" >> return (ImmRHS undefined))
            <|> (string "v" >> return VectorRHS)
            <|> (string "x" >> return ScalarRHS)
        return (op, rhs)

instance OpReachableSymbols (VectorCompare 'C) where
  opReachableSymbols = mempty

instance SplitChoice SketchVectorCompare where
  splitChoice (SketchVectorCompare vtype op dest mask rhs) = do
    op <- op
    dest <- dest
    mask <- mask
    rhs <- splitChoice rhs
    return $ SketchVectorCompare vtype [op] [dest] [mask] rhs

instance
  (SemConstraint mode ctx, Num cost, Mergeable cost) =>
  OpCost CostModel (VectorCompare mode) cost ctx
  where
  opCost CostModel {..} _ (VectorCompare {..}) = do
    mrgReturn $ fromIntegral $ compareCost _vectorConfig

instance (ExtractFeature (VectorCompare 'C) FeatureSet) where
  extractFeature
    (VectorCompare config (Identity op) _ (Identity masking) _) =
      vectorConfigFeature config
        <> maskingFeature masking
        <> intCompareOpCodeFeature op

instance ToFastSketch (VectorCompare 'C) SketchVectorCompare where
  toFastSketch (VectorCompare vtype op dest mask rhs) =
    SketchVectorCompare
      vtype
      (toList op)
      (toList dest)
      (toList mask)
      (toFastSketch rhs)

instance (EvalMode mode) => ScaleLMul (VectorCompare mode) where
  scaleLMul ratio op = op & (vectorConfig %~ scaleLMul ratio)

instance ScaleLMul SketchVectorCompare where
  scaleLMul ratio op = op & (vectorConfig %~ scaleLMul ratio)

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

module RVV.Synthesizer.Operator.SingleWidthIntBinary
  ( singleWidthIntBinary,
    sketchSingleWidthIntBinary,
    SingleWidthIntBinary (..),
    SketchSingleWidthIntBinary (..),
    singleWidthIntBinaryVectorRHSSupportedOpCode,
    singleWidthIntBinaryScalarRHSSupportedOpCode,
    singleWidthIntBinaryFullScalarRHSSupportedOpCode,
    singleWidthIntBinaryImmRHSSupportedOpCode,
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
import HieraSynth.Util.Parser (comma, leftBracket, named, rightBracket)
import Grisette.Unified
  ( EvalModeConvertible,
    EvalModeTag (C, S),
    GetData,
    UnifiedSymEq,
    extractData,
  )
import RVV.EvalMode (EvalMode, MonadEvalMode)
import RVV.Parser.ArgParser (destinationParser, maskingParser, vectorConfigParser)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.VectorConfig (VectorConfig)
import RVV.Synthesizer.CostModel.CostModel
  ( CostModel (CostModel, singleWidthCost),
  )
import RVV.Synthesizer.Feature.ExtractFeature (ExtractFeature (extractFeature))
import RVV.Synthesizer.Feature.FeatureSet
  ( FeatureSet,
    maskingFeature,
    vectorConfigFeature,
  )
import RVV.Synthesizer.Lens
  ( HasDestination (destination),
    HasMasking (masking),
    HasRhs (rhs),
    HasSingleWidthIntBinaryOpCode (singleWidthIntBinaryOpCode),
    HasVectorConfig (vectorConfig),
  )
import RVV.Synthesizer.OpSemantics.ElementWise
  ( applySingleWidthVVElementWise,
    typeFullScalarVX,
    typeSingleWidthVI,
    typeSingleWidthVVElementWise,
    typeSingleWidthVXElementWise,
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
  ( RHSSpec (FullScalarRHS, ImmRHS, ScalarRHS, VectorRHS),
    SketchRHSSpec,
    augmentElementWiseVIArgList,
    augmentElementWiseVXArgList,
    augmentFullScalarVXArgList,
  )
import RVV.Synthesizer.Operator.Common.TypeSketchOp (typeSketchOp)
import RVV.Synthesizer.Parameter.Common (checkUnsupportedOpCode)
import RVV.Synthesizer.Parameter.Destination (Destination)
import RVV.Synthesizer.Parameter.Masking (Masking)
import RVV.Synthesizer.Parameter.SingleWidthIntBinaryOpCode
  ( SingleWidthIntBinaryOpCode
      ( Add,
        And,
        Div,
        Divu,
        Max,
        Maxu,
        Min,
        Minu,
        Mul,
        Mulh,
        Mulhsu,
        Mulhu,
        Or,
        RSub,
        Rem,
        Remu,
        SAdd,
        SAddu,
        SSub,
        SSubu,
        Sll,
        Sra,
        Srl,
        Sub,
        Xor
      ),
    binaryOpCodeUseAnyInvalidSemantics,
    commutativeSingleWidthIntBinaryOp,
    interpretSingleWidthBinaryOpCode,
    singleWidthIntBinaryOpCodeFeature,
    singleWidthIntBinaryOpCodeParser,
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

data SingleWidthIntBinary mode
  = SingleWidthIntBinary
  { _vectorConfig :: VectorConfig,
    _singleWidthIntBinaryOpCode :: GetData mode SingleWidthIntBinaryOpCode,
    _destination :: GetData mode Destination,
    _masking :: GetData mode Masking,
    _rhs :: RHSSpec mode
  }

singleWidthIntBinary ::
  (SingleWidthIntBinary mode :<: op) =>
  VectorConfig ->
  GetData mode SingleWidthIntBinaryOpCode ->
  GetData mode Destination ->
  GetData mode Masking ->
  RHSSpec mode ->
  op
singleWidthIntBinary vtype binOp dest mask rhs =
  inj $ SingleWidthIntBinary vtype binOp dest mask rhs

data SketchSingleWidthIntBinary
  = SketchSingleWidthIntBinary
  { _vectorConfig :: VectorConfig,
    _singleWidthIntBinaryOpCode :: [SingleWidthIntBinaryOpCode],
    _destination :: [Destination],
    _masking :: [Masking],
    _rhs :: SketchRHSSpec
  }

sketchSingleWidthIntBinary ::
  (SketchSingleWidthIntBinary :<: op) =>
  VectorConfig ->
  [SingleWidthIntBinaryOpCode] ->
  [Destination] ->
  [Masking] ->
  SketchRHSSpec ->
  op
sketchSingleWidthIntBinary config binOp dest mask rhs =
  inj $ SketchSingleWidthIntBinary config binOp dest mask rhs

deriveFullExcept
  firstModeDeriveConfig
  [''SingleWidthIntBinary]
  [''UnifiedSymEq]
deriveNoSymEval [''SketchSingleWidthIntBinary]
makeFieldsNoPrefix ''SingleWidthIntBinary
makeFieldsNoPrefix ''SketchSingleWidthIntBinary

singleWidthIntBinaryVectorRHSSupportedOpCode ::
  [SingleWidthIntBinaryOpCode]
singleWidthIntBinaryVectorRHSSupportedOpCode =
  [ Add,
    Sub,
    RSub,
    And,
    Or,
    Xor,
    Sll,
    Srl,
    Sra,
    Min,
    Minu,
    Max,
    Maxu,
    Mul,
    Mulh,
    Mulhu,
    Mulhsu,
    Div,
    Divu,
    Rem,
    Remu,
    SAddu,
    SAdd,
    SSub,
    SSubu
  ]

singleWidthIntBinaryScalarRHSSupportedOpCode :: [SingleWidthIntBinaryOpCode]
singleWidthIntBinaryScalarRHSSupportedOpCode =
  [ Add,
    Sub,
    RSub,
    And,
    Or,
    Xor,
    Sll,
    Srl,
    Sra,
    Min,
    Minu,
    Max,
    Maxu,
    Mul,
    Mulh,
    Mulhu,
    Mulhsu,
    Div,
    Divu,
    Rem,
    Remu,
    SAddu,
    SAdd,
    SSub,
    SSubu
  ]

singleWidthIntBinaryFullScalarRHSSupportedOpCode ::
  [SingleWidthIntBinaryOpCode]
singleWidthIntBinaryFullScalarRHSSupportedOpCode = [Sll, Srl, Sra]

singleWidthIntBinaryImmRHSSupportedOpCode ::
  [SingleWidthIntBinaryOpCode]
singleWidthIntBinaryImmRHSSupportedOpCode =
  singleWidthIntBinaryScalarRHSSupportedOpCode

applySingleWidthIntBinary ::
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  GetData mode SingleWidthIntBinaryOpCode ->
  GetData mode Destination ->
  GetData mode Masking ->
  [Value mode] ->
  ctx [Value mode]
applySingleWidthIntBinary vconst config wrappedBinOp destination masking inputs = do
  binOp <- extractData wrappedBinOp
  applySingleWidthVVElementWise
    vconst
    config
    ( \a b -> do
        mrgReturn $
          binaryOpCodeUseAnyInvalidSemantics
            (interpretSingleWidthBinaryOpCode binOp)
            a
            b
    )
    destination
    masking
    inputs

instance
  ( SemConstraint mode ctx,
    EvalModeConvertible opMode mode,
    MonadEvalMode opMode ctx
  ) =>
  OpSemantics MachineConfig (SingleWidthIntBinary opMode) (Value mode) ctx
  where
  applyOp vconst _ op inputs = do
    case toSym op of
      SingleWidthIntBinary vtype wrappedBinOp destination masking VectorRHS -> do
        checkUnsupportedOpCode
          "SingleWidthIntBinary (VectorRHS)"
          wrappedBinOp
          singleWidthIntBinaryVectorRHSSupportedOpCode
        applySingleWidthIntBinary vconst vtype wrappedBinOp destination masking inputs
      SingleWidthIntBinary vtype wrappedBinOp destination masking FullScalarRHS -> do
        checkUnsupportedOpCode
          "SingleWidthIntBinary (FullScalarRHS)"
          wrappedBinOp
          singleWidthIntBinaryFullScalarRHSSupportedOpCode
        newInputs <- augmentFullScalarVXArgList vconst vtype 2 inputs
        applySingleWidthIntBinary vconst vtype wrappedBinOp destination masking newInputs
      SingleWidthIntBinary vtype wrappedBinOp destination masking ScalarRHS -> do
        checkUnsupportedOpCode
          "SingleWidthIntBinary (ScalarRHS)"
          wrappedBinOp
          singleWidthIntBinaryScalarRHSSupportedOpCode
        newInputs <- augmentElementWiseVXArgList vconst vtype 2 inputs
        applySingleWidthIntBinary vconst vtype wrappedBinOp destination masking newInputs
      SingleWidthIntBinary vtype wrappedBinOp destination masking (ImmRHS imm) -> do
        checkUnsupportedOpCode
          "SingleWidthIntBinary (ImmRHS)"
          wrappedBinOp
          singleWidthIntBinaryImmRHSSupportedOpCode
        newInputs <- augmentElementWiseVIArgList vconst vtype imm 2 inputs
        applySingleWidthIntBinary vconst vtype wrappedBinOp destination masking newInputs

instance
  (SemConstraint mode ctx) =>
  OpTyping (SingleWidthIntBinary mode) ctx
  where
  type OpTypeType (SingleWidthIntBinary mode) = ValueType
  typeOp
    (SingleWidthIntBinary vtype _ destination masking VectorRHS) =
      typeSingleWidthVVElementWise vtype destination masking
  typeOp
    (SingleWidthIntBinary vtype _ destination masking FullScalarRHS) =
      typeFullScalarVX vtype destination masking
  typeOp
    (SingleWidthIntBinary vtype _ destination masking ScalarRHS) =
      typeSingleWidthVXElementWise vtype destination masking
  typeOp
    (SingleWidthIntBinary vtype _ destination masking (ImmRHS _)) =
      typeSingleWidthVI vtype destination masking

instance
  (MonadAngelicContext ctx) =>
  OpTyping SketchSingleWidthIntBinary ctx
  where
  type OpTypeType SketchSingleWidthIntBinary = ValueType
  typeOp = typeSketchOp @(SingleWidthIntBinary 'S)

instance
  (MonadEvalMode mode Union) =>
  OpSymmetryReduction (SingleWidthIntBinary mode)
  where
  opUnreorderable = error "Should not be called"
  opCommutativeArgPos (SingleWidthIntBinary _ binOp _ _ VectorRHS) =
    checkOpCommutativeArgPos
      binOp
      commutativeSingleWidthIntBinaryOp
      [[1, 2]]
  opCommutativeArgPos _ = mrgReturn []

instance GenSym SketchSingleWidthIntBinary (SingleWidthIntBinary 'S)

instance GenSymSimple SketchSingleWidthIntBinary (SingleWidthIntBinary 'S) where
  simpleFresh (SketchSingleWidthIntBinary vtype binOp dest mask rhs) = do
    dest <- chooseFresh dest
    binOp <- chooseFresh binOp
    mask <- chooseFresh mask
    rhs <- simpleFresh rhs
    return $ SingleWidthIntBinary vtype binOp dest mask rhs

instance GenSym (SingleWidthIntBinary 'C) (SingleWidthIntBinary 'S)

instance
  GenSymSimple
    (SingleWidthIntBinary 'C)
    (SingleWidthIntBinary 'S)
  where
  simpleFresh = return . toSym

instance OpPPrint (SingleWidthIntBinary 'C) where
  describeArguments _ = return []

instance PPrint (SingleWidthIntBinary 'C) where
  pformat (SingleWidthIntBinary vtype binOp dest mask rhs) =
    "v"
      <> pformat binOp
      <> "."
      <> postFix
      <> case rhs of
        ImmRHS imm -> pformatArgList (vtype, dest, mask, withName "rhs" imm)
        _ -> pformatArgList (vtype, dest, mask)
    where
      postFix = case rhs of
        ImmRHS _ -> "vi"
        ScalarRHS -> "vx"
        FullScalarRHS -> "vx.full"
        VectorRHS -> "vv"

instance OpPPrint SketchSingleWidthIntBinary where
  describeArguments _ = return []
  prefixResults _ = return []

instance PPrint SketchSingleWidthIntBinary where
  pformat (SketchSingleWidthIntBinary vtype binOp dest mask rhs) =
    "v"
      <> pformatChoices binOp
      <> pformatArgList (vtype, choices dest, choices mask, rhs)

instance OpParser (SingleWidthIntBinary 'C) where
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
    return $ SingleWidthIntBinary vtype op dest mask rhs
    where
      attrWithOp = do
        op <- singleWidthIntBinaryOpCodeParser "v" ".v"
        rhs <-
          (string "i" >> return (ImmRHS undefined))
            <|> (string "v" >> return VectorRHS)
            <|> (string "x.full" >> return FullScalarRHS)
            <|> (string "x" >> return ScalarRHS)
        return (op, rhs)

instance OpReachableSymbols (SingleWidthIntBinary 'C) where
  opReachableSymbols = mempty

instance SplitChoice SketchSingleWidthIntBinary where
  splitChoice (SketchSingleWidthIntBinary vtype binOp dest mask rhs) = do
    binOp <- binOp
    dest <- dest
    mask <- mask
    rhs <- splitChoice rhs
    return $ SketchSingleWidthIntBinary vtype [binOp] [dest] [mask] rhs

instance
  (SemConstraint mode ctx, Num cost, Mergeable cost) =>
  OpCost CostModel (SingleWidthIntBinary mode) cost ctx
  where
  opCost CostModel {..} _ (SingleWidthIntBinary {..}) = do
    op <- extractData _singleWidthIntBinaryOpCode
    mrgReturn $ fromIntegral $ singleWidthCost op _vectorConfig

instance (ExtractFeature (SingleWidthIntBinary 'C) FeatureSet) where
  extractFeature
    (SingleWidthIntBinary config (Identity op) _ (Identity masking) _) =
      vectorConfigFeature config
        <> maskingFeature masking
        <> singleWidthIntBinaryOpCodeFeature op

instance
  ToFastSketch
    (SingleWidthIntBinary 'C)
    SketchSingleWidthIntBinary
  where
  toFastSketch (SingleWidthIntBinary vtype binOp dest mask rhs) =
    SketchSingleWidthIntBinary
      vtype
      (toList binOp)
      (toList dest)
      (toList mask)
      (toFastSketch rhs)

instance (EvalMode mode) => ScaleLMul (SingleWidthIntBinary mode) where
  scaleLMul ratio op = op & (vectorConfig %~ scaleLMul ratio)

instance ScaleLMul SketchSingleWidthIntBinary where
  scaleLMul ratio op = op & (vectorConfig %~ scaleLMul ratio)

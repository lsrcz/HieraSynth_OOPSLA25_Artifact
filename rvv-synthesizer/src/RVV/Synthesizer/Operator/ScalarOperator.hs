{-# LANGUAGE ConstraintKinds #-}
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

module RVV.Synthesizer.Operator.ScalarOperator
  ( ScalarOperator (..),
    SketchScalarOperator (..),
    scalarBin,
    sketchScalarBin,
    scalarUnary,
    sketchScalarUnary,
    scalarBinVectorLength,
    sketchScalarBinVectorLength,
  )
where

import Control.Applicative ((<|>))
import Control.Lens (Identity (Identity), makeFieldsNoPrefix)
import Data.Foldable (Foldable (toList))
import Grisette
  ( GenSym,
    GenSymSimple (simpleFresh),
    Mergeable,
    PPrint (pformat),
    ToSym (toSym),
    chooseFresh,
    mrgReturn,
  )
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
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
import HieraSynth.Util.Parser (betweenBrackets, comma, leftBracket, named, rightBracket)
import Grisette.Unified (EvalModeConvertible, EvalModeTag (C, S), GetData, UnifiedSymEq, extractData)
import RVV.EvalMode (MonadEvalMode)
import RVV.Parser.ArgParser (maskMulParser, widthMulParser)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Multiplier (MaskMul, WidthMul)
import RVV.Semantics.PrimOp.ImmToReg (immToScalar)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Synthesizer.CostModel.CostModel (CostModel (CostModel, scalarBinCost, scalarUnaryCost))
import RVV.Synthesizer.Feature.ExtractFeature (ExtractFeature (extractFeature))
import RVV.Synthesizer.Feature.FeatureSet
  ( FeatureSet,
    maskMulFeature,
    widthMulFeature,
  )
import RVV.Synthesizer.Lens
  ( HasMaskMul (maskMul),
    HasRhs (rhs),
    HasSingleWidthIntBinaryOpCode (singleWidthIntBinaryOpCode),
    HasSingleWidthIntUnaryOpCode (singleWidthIntUnaryOpCode),
    HasWidthMul (widthMul),
  )
import RVV.Synthesizer.OpSemantics.Scalar
  ( applyScalarBin,
    applyScalarBinVL,
    applyScalarUnary,
    typeScalarBin,
    typeScalarBinImm,
    typeScalarBinVL,
    typeScalarBinVLImm,
    typeScalarUnary,
  )
import RVV.Synthesizer.Operator.Common.PPrint
  ( PPrintArgList (pformatArgList),
    pformatChoices,
    withName,
  )
import RVV.Synthesizer.Operator.Common.Parser (immParser)
import RVV.Synthesizer.Operator.Common.RHSSpec (RHSSpec (ImmRHS, ScalarRHS), SketchRHSSpec)
import RVV.Synthesizer.Operator.Common.TypeSketchOp (typeSketchOp)
import RVV.Synthesizer.Parameter.SingleWidthIntBinaryOpCode
  ( SingleWidthIntBinaryOpCode,
    interpretSingleWidthBinaryOpCode,
    singleWidthIntBinaryOpCodeFeature,
    singleWidthIntBinaryOpCodeParser,
  )
import RVV.Synthesizer.Parameter.SingleWidthIntUnaryOpCode
  ( SingleWidthIntUnaryOpCode,
    interpretSingleWidthIntUnaryOpCode,
    singleWidthIntUnaryOpCodeFeature,
    singleWidthIntUnaryOpParser,
  )
import RVV.Synthesizer.SketchGen.ToFastSketch (ToFastSketch (toFastSketch))
import RVV.Synthesizer.Specification.ScaleLMul (ScaleLMul (scaleLMul))
import RVV.Synthesizer.Type (ValueType)
import RVV.Synthesizer.Value (Value (ScalarValue))
import RVV.Util.Derive
  ( deriveFullExcept,
    deriveNoSymEval,
    firstModeDeriveConfig,
  )
import Text.Megaparsec.Char (string)

data ScalarOperator mode
  = ScalarBin
      { _singleWidthIntBinaryOpCode :: GetData mode SingleWidthIntBinaryOpCode,
        _widthMul :: WidthMul,
        _rhs :: RHSSpec mode
      }
  | ScalarUnary
      { _singleWidthIntUnaryOpCode :: GetData mode SingleWidthIntUnaryOpCode,
        _widthMul :: WidthMul
      }
  | ScalarBinVectorLength
      { _singleWidthIntBinaryOpCode :: GetData mode SingleWidthIntBinaryOpCode,
        _maskMul :: MaskMul,
        _rhs :: RHSSpec mode
      }

data SketchScalarOperator
  = SketchScalarBin
      { _singleWidthIntBinaryOpCode :: [SingleWidthIntBinaryOpCode],
        _widthMul :: WidthMul,
        _rhs :: SketchRHSSpec
      }
  | SketchScalarUnary
      { _singleWidthIntUnaryOpCode :: [SingleWidthIntUnaryOpCode],
        _widthMul :: WidthMul
      }
  | SketchScalarBinVectorLength
      { _singleWidthIntBinaryOpCode :: [SingleWidthIntBinaryOpCode],
        _maskMul :: MaskMul,
        _rhs :: SketchRHSSpec
      }

deriveFullExcept
  firstModeDeriveConfig
  [''ScalarOperator]
  [''UnifiedSymEq]
deriveNoSymEval [''SketchScalarOperator]
makeFieldsNoPrefix ''ScalarOperator
makeFieldsNoPrefix ''SketchScalarOperator

scalarBin ::
  (ScalarOperator mode :<: op) =>
  GetData mode SingleWidthIntBinaryOpCode ->
  WidthMul ->
  RHSSpec mode ->
  op
scalarBin opcode widthMul rhs = inj $ ScalarBin opcode widthMul rhs

sketchScalarBin ::
  (SketchScalarOperator :<: op) =>
  [SingleWidthIntBinaryOpCode] ->
  WidthMul ->
  SketchRHSSpec ->
  op
sketchScalarBin opcode widthMul rhs = inj $ SketchScalarBin opcode widthMul rhs

scalarUnary ::
  (ScalarOperator mode :<: op) =>
  GetData mode SingleWidthIntUnaryOpCode ->
  WidthMul ->
  op
scalarUnary opcode widthMul = inj $ ScalarUnary opcode widthMul

sketchScalarUnary ::
  (SketchScalarOperator :<: op) =>
  [SingleWidthIntUnaryOpCode] ->
  WidthMul ->
  op
sketchScalarUnary opcode widthMul = inj $ SketchScalarUnary opcode widthMul

scalarBinVectorLength ::
  (ScalarOperator mode :<: op) =>
  GetData mode SingleWidthIntBinaryOpCode ->
  MaskMul ->
  RHSSpec mode ->
  op
scalarBinVectorLength opcode maskMul rhs =
  inj $ ScalarBinVectorLength opcode maskMul rhs

sketchScalarBinVectorLength ::
  (SketchScalarOperator :<: op) =>
  [SingleWidthIntBinaryOpCode] ->
  MaskMul ->
  SketchRHSSpec ->
  op
sketchScalarBinVectorLength opcode maskMul rhs =
  inj $ SketchScalarBinVectorLength opcode maskMul rhs

instance
  ( SemConstraint mode ctx,
    EvalModeConvertible opMode mode,
    MonadEvalMode opMode ctx
  ) =>
  OpSemantics MachineConfig (ScalarOperator opMode) (Value mode) ctx
  where
  applyOp machine _ op inputs = case toSym op :: ScalarOperator mode of
    ScalarBin opcode widthMul ScalarRHS -> do
      opcode <- extractData opcode
      applyScalarBin machine widthMul (interpretSingleWidthBinaryOpCode opcode) inputs
    ScalarBin opcode widthMul (ImmRHS imm) -> do
      opcode <- extractData opcode
      rhs <- immToScalar machine True widthMul imm
      applyScalarBin machine widthMul (interpretSingleWidthBinaryOpCode opcode) (inputs ++ [ScalarValue rhs])
    ScalarUnary opcode widthMul -> do
      opcode <- extractData opcode
      applyScalarUnary machine widthMul (interpretSingleWidthIntUnaryOpCode opcode) inputs
    ScalarBinVectorLength opcode maskMul ScalarRHS -> do
      opcode <- extractData opcode
      applyScalarBinVL machine maskMul (interpretSingleWidthBinaryOpCode opcode) inputs
    ScalarBinVectorLength opcode maskMul (ImmRHS imm) -> do
      opcode <- extractData opcode
      rhs <- immToScalar machine True 1 imm
      applyScalarBinVL machine maskMul (interpretSingleWidthBinaryOpCode opcode) (inputs ++ [ScalarValue rhs])
    _ -> mrgThrowError "Unsupported scalar operator"

instance (MonadContext ctx) => OpTyping (ScalarOperator mode) ctx where
  type OpTypeType (ScalarOperator mode) = ValueType
  typeOp op = case op of
    ScalarBin _ widthMul ScalarRHS -> typeScalarBin widthMul
    ScalarBin _ widthMul (ImmRHS _) -> typeScalarBinImm widthMul
    ScalarUnary _ widthMul -> typeScalarUnary widthMul
    ScalarBinVectorLength _ maskMul ScalarRHS -> typeScalarBinVL maskMul
    ScalarBinVectorLength _ maskMul (ImmRHS _) -> typeScalarBinVLImm maskMul
    _ -> mrgThrowError "Unsupported scalar operator"

instance (MonadAngelicContext ctx) => OpTyping SketchScalarOperator ctx where
  type OpTypeType SketchScalarOperator = ValueType
  typeOp = typeSketchOp @(ScalarOperator 'S)

instance OpSymmetryReduction (ScalarOperator mode) where
  opUnreorderable = error "Should not be called"
  opCommutativeArgPos _ = mrgReturn []

instance GenSym SketchScalarOperator (ScalarOperator 'S)

instance GenSymSimple SketchScalarOperator (ScalarOperator 'S) where
  simpleFresh (SketchScalarBin opcode widthMul rhs) = do
    opcode <- chooseFresh opcode
    rhs <- simpleFresh rhs
    return $ ScalarBin opcode widthMul rhs
  simpleFresh (SketchScalarUnary opcode widthMul) = do
    opcode <- chooseFresh opcode
    return $ ScalarUnary opcode widthMul
  simpleFresh (SketchScalarBinVectorLength opcode maskMul rhs) = do
    opcode <- chooseFresh opcode
    rhs <- simpleFresh rhs
    return $ ScalarBinVectorLength opcode maskMul rhs

instance GenSym (ScalarOperator 'C) (ScalarOperator 'S)

instance GenSymSimple (ScalarOperator 'C) (ScalarOperator 'S) where
  simpleFresh = return . toSym

instance OpPPrint (ScalarOperator 'C) where
  describeArguments _ = return []
  prefixResults _ = return []

instance PPrint (ScalarOperator 'C) where
  pformat op = case op of
    ScalarBin opcode widthMul ScalarRHS ->
      pformat opcode <> ".xx" <> pformatArgList (withName "width" widthMul)
    ScalarBin opcode widthMul (ImmRHS imm) ->
      pformat opcode <> ".xi" <> pformatArgList (withName "width" widthMul, withName "rhs" imm)
    ScalarUnary opcode widthMul ->
      pformat opcode <> ".x" <> pformatArgList (withName "width" widthMul)
    ScalarBinVectorLength opcode maskMul ScalarRHS ->
      pformat opcode <> ".vl.x" <> pformatArgList (withName "mmul" maskMul)
    ScalarBinVectorLength opcode maskMul (ImmRHS imm) ->
      pformat opcode <> ".vl.i" <> pformatArgList (withName "mmul" maskMul, withName "rhs" imm)
    _ -> "unsupported"

instance OpPPrint SketchScalarOperator where
  describeArguments _ = return []
  prefixResults _ = return []

instance PPrint SketchScalarOperator where
  pformat op = case op of
    SketchScalarBin opcode widthMul rhs ->
      pformatChoices opcode <> pformatArgList (withName "width" widthMul, rhs)
    SketchScalarUnary opcode widthMul ->
      pformatChoices opcode <> pformatArgList (withName "width" widthMul)
    SketchScalarBinVectorLength opcode maskMul rhs ->
      pformatChoices opcode <> pformatArgList (withName "mmul" maskMul, rhs)

instance OpParser (ScalarOperator 'C) where
  opParser = scalarBinParser <|> scalarUnaryParser <|> scalarBinVectorLengthParser
    where
      attrWithOp post = do
        op <- singleWidthIntBinaryOpCodeParser "" post
        rhs <-
          (string "i" >> return (ImmRHS undefined))
            <|> (string "x" >> return ScalarRHS)
        return (op, rhs)
      scalarBinParser = do
        (op, rhs) <- attrWithOp ".x"
        leftBracket
        width <- named "width" widthMulParser
        rhs <- case rhs of
          ImmRHS _ -> do
            comma
            imm <- named "rhs" immParser
            return $ ImmRHS imm
          _ -> return rhs
        rightBracket
        return $ ScalarBin op width rhs
      scalarUnaryParser = do
        op <- singleWidthIntUnaryOpParser
        string ".x"
        width <- betweenBrackets (named "width" widthMulParser)
        return $ ScalarUnary op width
      scalarBinVectorLengthParser = do
        (op, rhs) <- attrWithOp ".vl."
        leftBracket
        maskMul <- named "mmul" maskMulParser
        comma
        rhs <- case rhs of
          ImmRHS _ -> do
            comma
            imm <- named "rhs" immParser
            return $ ImmRHS imm
          _ -> return rhs
        rightBracket
        return $ ScalarBinVectorLength op maskMul rhs

instance OpReachableSymbols (ScalarOperator 'C) where
  opReachableSymbols = mempty

instance SplitChoice SketchScalarOperator where
  splitChoice (SketchScalarBin opcode widthMul rhs) = do
    opcode <- opcode
    rhs <- splitChoice rhs
    return $ SketchScalarBin [opcode] widthMul rhs
  splitChoice (SketchScalarUnary opcode widthMul) = do
    opcode <- opcode
    return $ SketchScalarUnary [opcode] widthMul
  splitChoice (SketchScalarBinVectorLength opcode maskMul rhs) = do
    opcode <- opcode
    rhs <- splitChoice rhs
    return $ SketchScalarBinVectorLength [opcode] maskMul rhs

instance
  (SemConstraint mode ctx, Num cost, Mergeable cost) =>
  OpCost CostModel (ScalarOperator mode) cost ctx
  where
  opCost CostModel {..} _ (ScalarBin opcode widthMul _) = do
    op <- extractData opcode
    mrgReturn $ fromIntegral $ scalarBinCost op widthMul
  opCost CostModel {..} _ (ScalarUnary opcode widthMul) = do
    op <- extractData opcode
    mrgReturn $ fromIntegral $ scalarUnaryCost op widthMul
  opCost CostModel {..} _ (ScalarBinVectorLength opcode _ _) = do
    op <- extractData opcode
    mrgReturn $ fromIntegral $ scalarBinCost op 1

instance ExtractFeature (ScalarOperator 'C) FeatureSet where
  extractFeature op = case op of
    ScalarBin (Identity opcode) widthMul _ ->
      singleWidthIntBinaryOpCodeFeature opcode <> widthMulFeature widthMul
    ScalarUnary (Identity opcode) widthMul ->
      singleWidthIntUnaryOpCodeFeature opcode <> widthMulFeature widthMul
    ScalarBinVectorLength (Identity opcode) maskMul _ ->
      maskMulFeature maskMul
        <> singleWidthIntBinaryOpCodeFeature opcode
        <> widthMulFeature 1

instance ToFastSketch (ScalarOperator 'C) SketchScalarOperator where
  toFastSketch op = case op of
    ScalarBin opcode widthMul rhs ->
      SketchScalarBin (toList opcode) widthMul (toFastSketch rhs)
    ScalarUnary opcode widthMul -> SketchScalarUnary (toList opcode) widthMul
    ScalarBinVectorLength opcode maskMul rhs ->
      SketchScalarBinVectorLength (toList opcode) maskMul (toFastSketch rhs)

instance ScaleLMul (ScalarOperator mode) where
  scaleLMul ratio op = case op of
    ScalarBinVectorLength opcode maskMul rhs ->
      ScalarBinVectorLength opcode (scaleLMul ratio maskMul) rhs
    _ -> op

instance ScaleLMul SketchScalarOperator where
  scaleLMul ratio op = case op of
    SketchScalarBinVectorLength opcode maskMul rhs ->
      SketchScalarBinVectorLength opcode (scaleLMul ratio maskMul) rhs
    _ -> op

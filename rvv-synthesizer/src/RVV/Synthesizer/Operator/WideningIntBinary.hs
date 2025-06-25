{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.Operator.WideningIntBinary
  ( WideningIntBinary (..),
    SketchWideningIntBinary (..),
    wideningIntBinary,
    sketchWideningIntBinary,
  )
where

import Control.Applicative ((<|>))
import Control.Lens (Identity (Identity), makeFieldsNoPrefix, (%~), (&))
import Data.Foldable (toList)
import GHC.Generics (Generic)
import Grisette
  ( GenSym,
    GenSymSimple (simpleFresh),
    LogicalOp (false, true),
    Mergeable,
    PPrint (pformat),
    ToSym (toSym),
    Union,
    chooseFresh,
    mrgReturn,
  )
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
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
import HieraSynth.Util.Parser (comma, leftBracket, named, rightBracket)
import Grisette.Unified
  ( EvalModeConvertible,
    EvalModeTag (C, S),
    GetBool,
    GetData,
    UnifiedSymEq,
    extractData,
    mrgIf,
  )
import RVV.EvalMode (EvalMode, MonadEvalMode)
import RVV.Parser.ArgParser
  ( destinationParser,
    maskingParser,
    vectorConfigParser,
  )
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.VectorConfig (VectorConfig, narrowVectorConfig)
import RVV.Synthesizer.CostModel.CostModel (CostModel (CostModel, wideningCost))
import RVV.Synthesizer.Feature.ExtractFeature (ExtractFeature (extractFeature))
import RVV.Synthesizer.Feature.FeatureSet
  ( FeatureSet,
    maskingFeature,
    vectorConfigFeature,
  )
import RVV.Synthesizer.Lens
  ( HasDestination (destination),
    HasHasWideLhs (hasWideLhs),
    HasMasking (masking),
    HasRhs (rhs),
    HasVectorConfigWide (vectorConfigWide),
    HasWideningIntBinaryOpCode (wideningIntBinaryOpCode),
  )
import RVV.Synthesizer.OpSemantics.ElementWise
  ( applyWideningVVElementWise,
    applyWideningWVElementWise,
    typeWideningVIElementWise,
    typeWideningVVElementWise,
    typeWideningVXElementWise,
    typeWideningWIElementWise,
    typeWideningWVElementWise,
    typeWideningWXElementWise,
  )
import RVV.Synthesizer.Operator.Common.OpSymmetryReduction (checkOpCommutativeArgPos)
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
import RVV.Synthesizer.Parameter.Common (checkUnsupportedOpCode)
import RVV.Synthesizer.Parameter.Destination (Destination)
import RVV.Synthesizer.Parameter.Masking (Masking)
import RVV.Synthesizer.Parameter.SingleWidthIntBinaryOpCode
  ( binaryOpCodeUseAnyInvalidSemantics,
  )
import RVV.Synthesizer.Parameter.WideningIntBinaryOpCode
  ( WideningIntBinaryOpCode (WAdd, WAddu, WMul, WMulsu, WMulu, WSub, WSubu),
    commutativeWideningIntBinaryOpCode,
    interpretWideningIntBinaryOpCode,
    wideningIntBinaryOpCodeFeature,
    wideningIntBinaryOpCodeParser,
    wideningIntBinaryOpCodeWidenLhs,
    wideningIntBinaryOpCodeWidenRhs,
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

data WideningIntBinary mode
  = WideningIntBinary
  { _vectorConfigWide :: VectorConfig,
    _wideningIntBinaryOpCode :: GetData mode WideningIntBinaryOpCode,
    _destination :: GetData mode Destination,
    _masking :: GetData mode Masking,
    _hasWideLhs :: GetBool mode,
    _rhs :: RHSSpec mode
  }
  deriving (Generic)

data SketchWideningIntBinary
  = SketchWideningIntBinary
  { _vectorConfigWide :: VectorConfig,
    _wideningIntBinaryOpCode :: [WideningIntBinaryOpCode],
    _destination :: [Destination],
    _masking :: [Masking],
    _hasWideLhs :: [Bool],
    _rhs :: SketchRHSSpec
  }
  deriving (Generic)

deriveFullExcept
  firstModeDeriveConfig
  [''WideningIntBinary]
  [''UnifiedSymEq]
deriveNoSymEval [''SketchWideningIntBinary]
makeFieldsNoPrefix ''WideningIntBinary
makeFieldsNoPrefix ''SketchWideningIntBinary

wideningIntBinary ::
  forall mode op.
  (WideningIntBinary mode :<: op) =>
  VectorConfig ->
  GetData mode WideningIntBinaryOpCode ->
  GetData mode Destination ->
  GetData mode Masking ->
  GetBool mode ->
  RHSSpec mode ->
  op
wideningIntBinary config binOp dest mask hasWideLhs rhs =
  inj $ WideningIntBinary config binOp dest mask hasWideLhs rhs

sketchWideningIntBinary ::
  (SketchWideningIntBinary :<: op) =>
  VectorConfig ->
  [WideningIntBinaryOpCode] ->
  [Destination] ->
  [Masking] ->
  [Bool] ->
  SketchRHSSpec ->
  op
sketchWideningIntBinary config binOp dest mask hasWideLhs rhs =
  inj $ SketchWideningIntBinary config binOp dest mask hasWideLhs rhs

wideningIntBinaryNarrowLhsSupportedOp :: [WideningIntBinaryOpCode]
wideningIntBinaryNarrowLhsSupportedOp =
  [WAddu, WSubu, WAdd, WSub, WMul, WMulu, WMulsu]

wideningIntBinaryWideLhsSupportedOp :: [WideningIntBinaryOpCode]
wideningIntBinaryWideLhsSupportedOp = [WAddu, WSubu, WAdd, WSub]

applyWideningIntBinaryNarrowLhs ::
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  GetData mode WideningIntBinaryOpCode ->
  GetData mode Destination ->
  GetData mode Masking ->
  [Value mode] ->
  ctx [Value mode]
applyWideningIntBinaryNarrowLhs vconst config wrappedBinOp dest mask l = do
  binOp <- extractData wrappedBinOp
  applyWideningVVElementWise
    vconst
    config
    (wideningIntBinaryOpCodeWidenLhs binOp)
    (wideningIntBinaryOpCodeWidenRhs binOp)
    ( \a b ->
        mrgReturn $
          binaryOpCodeUseAnyInvalidSemantics
            (interpretWideningIntBinaryOpCode binOp)
            a
            b
    )
    dest
    mask
    l

applyWideningIntBinaryWideLhs ::
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  GetData mode WideningIntBinaryOpCode ->
  GetData mode Destination ->
  GetData mode Masking ->
  [Value mode] ->
  ctx [Value mode]
applyWideningIntBinaryWideLhs vconst config wrappedBinOp dest mask l = do
  binOp <- extractData wrappedBinOp
  applyWideningWVElementWise
    vconst
    config
    (wideningIntBinaryOpCodeWidenRhs binOp)
    ( \a b ->
        mrgReturn $
          binaryOpCodeUseAnyInvalidSemantics
            (interpretWideningIntBinaryOpCode binOp)
            a
            b
    )
    dest
    mask
    l

instance
  ( SemConstraint mode ctx,
    EvalModeConvertible opMode mode,
    MonadEvalMode opMode ctx
  ) =>
  OpSemantics MachineConfig (WideningIntBinary opMode) (Value mode) ctx
  where
  applyOp machine _ op inputs =
    case toSym op of
      WideningIntBinary config wrappedBinOp dest mask hasWideLhs rhs -> do
        inputs <- case rhs of
          VectorRHS -> return inputs
          ScalarRHS ->
            augmentElementWiseVXArgList machine (narrowVectorConfig config) 2 inputs
          ImmRHS imm ->
            augmentElementWiseVIArgList machine (narrowVectorConfig config) imm 2 inputs
          _ -> mrgThrowError "WideningIntBinary: Unsupported RHS"
        mrgIf @mode
          hasWideLhs
          ( do
              checkUnsupportedOpCode
                "WideningIntBinary (WideLhs)"
                wrappedBinOp
                wideningIntBinaryWideLhsSupportedOp
              applyWideningIntBinaryWideLhs machine config wrappedBinOp dest mask inputs
          )
          ( do
              checkUnsupportedOpCode
                "WideningIntBinary (NarrowLhs)"
                wrappedBinOp
                wideningIntBinaryNarrowLhsSupportedOp
              applyWideningIntBinaryNarrowLhs machine config wrappedBinOp dest mask inputs
          )

instance
  (SemConstraint mode ctx) =>
  OpTyping (WideningIntBinary mode) ctx
  where
  type OpTypeType (WideningIntBinary mode) = ValueType
  typeOp (WideningIntBinary vtype _ destination masking hasWideLhs rhs) =
    mrgIf
      hasWideLhs
      ( case rhs of
          VectorRHS -> typeWideningWVElementWise vtype destination masking
          ScalarRHS -> typeWideningWXElementWise vtype destination masking
          ImmRHS _ -> typeWideningWIElementWise vtype destination masking
          _ -> mrgThrowError "WideningIntBinary: Unsupported RHS"
      )
      ( case rhs of
          VectorRHS -> typeWideningVVElementWise vtype destination masking
          ScalarRHS -> typeWideningVXElementWise vtype destination masking
          ImmRHS _ -> typeWideningVIElementWise vtype destination masking
          _ -> mrgThrowError "WideningIntBinary: Unsupported RHS"
      )

instance (MonadAngelicContext ctx) => OpTyping SketchWideningIntBinary ctx where
  type OpTypeType SketchWideningIntBinary = ValueType
  typeOp = typeSketchOp @(WideningIntBinary 'S)

instance (MonadEvalMode mode Union) => OpSymmetryReduction (WideningIntBinary mode) where
  opUnreorderable = error "Should not be called"
  opCommutativeArgPos (WideningIntBinary _ binOp _ _ hasWideLhs VectorRHS) =
    mrgIf hasWideLhs (mrgReturn []) $
      checkOpCommutativeArgPos
        binOp
        commutativeWideningIntBinaryOpCode
        [[1, 2]]
  opCommutativeArgPos _ = mrgReturn []

instance GenSym SketchWideningIntBinary (WideningIntBinary 'S)

instance GenSymSimple SketchWideningIntBinary (WideningIntBinary 'S) where
  simpleFresh (SketchWideningIntBinary vtype binOp dest mask hasWideLhs rhs) = do
    dest <- chooseFresh dest
    binOp <- chooseFresh binOp
    mask <- chooseFresh mask
    hasWideLhs <-
      if
        | null hasWideLhs -> simpleFresh ()
        | and hasWideLhs -> return true
        | all not hasWideLhs -> return false
        | otherwise -> simpleFresh ()
    rhs <- simpleFresh rhs
    return $ WideningIntBinary vtype binOp dest mask hasWideLhs rhs

instance GenSym (WideningIntBinary 'C) (WideningIntBinary 'S)

instance GenSymSimple (WideningIntBinary 'C) (WideningIntBinary 'S) where
  simpleFresh = return . toSym

instance OpPPrint (WideningIntBinary 'C) where
  describeArguments _ = return []

instance PPrint (WideningIntBinary 'C) where
  pformat (WideningIntBinary vtype binOp dest mask hasWideLhs rhs) =
    "v"
      <> pformat binOp
      <> "."
      <> postFix
      <> case rhs of
        ImmRHS imm ->
          pformatArgList
            ( withName "wide" vtype,
              dest,
              mask,
              withName "rhs" imm
            )
        _ ->
          pformatArgList
            ( withName "wide" vtype,
              dest,
              mask
            )
    where
      postFix =
        (if hasWideLhs then "w" else "v") <> case rhs of
          VectorRHS -> "v"
          ScalarRHS -> "x"
          ImmRHS _ -> "i"
          _ -> error "WideningIntBinary: Unsupported RHS"

instance OpPPrint SketchWideningIntBinary where
  describeArguments _ = return []
  prefixResults _ = return []

instance PPrint SketchWideningIntBinary where
  pformat (SketchWideningIntBinary vtype binOp dest mask hasWideLhs rhs) =
    "v"
      <> pformatChoices binOp
      <> pformatArgList
        ( withName "wide" vtype,
          choices dest,
          choices mask,
          withName "wide_lhs" $ choices hasWideLhs,
          rhs
        )

instance OpParser (WideningIntBinary 'C) where
  opParser = do
    (op, hasWideLhs, rhs) <- attrWithOp
    leftBracket
    vtype <- named "wide" vectorConfigParser
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
    return $ WideningIntBinary vtype op dest mask hasWideLhs rhs
    where
      attrWithOp = do
        op <- wideningIntBinaryOpCodeParser "."
        hasWideLhs <- (string "w" >> return True) <|> (string "v" >> return False)
        rhs <-
          (string "i" >> return (ImmRHS undefined))
            <|> (string "v" >> return VectorRHS)
            <|> (string "x" >> return ScalarRHS)
        return (op, hasWideLhs, rhs)

instance OpReachableSymbols (WideningIntBinary 'C) where
  opReachableSymbols = mempty

instance SplitChoice SketchWideningIntBinary where
  splitChoice (SketchWideningIntBinary vtype binOp dest mask hasWideLhs rhs) = do
    binOp <- binOp
    dest <- dest
    mask <- mask
    hasWideLhs <- hasWideLhs
    rhs <- splitChoice rhs
    return $ SketchWideningIntBinary vtype [binOp] [dest] [mask] [hasWideLhs] rhs

instance
  (SemConstraint mode ctx, Num cost, Mergeable cost) =>
  OpCost CostModel (WideningIntBinary mode) cost ctx
  where
  opCost CostModel {..} _ (WideningIntBinary {..}) = do
    op <- extractData _wideningIntBinaryOpCode
    mrgReturn $ fromIntegral $ wideningCost op _vectorConfigWide

instance ExtractFeature (WideningIntBinary 'C) FeatureSet where
  extractFeature
    (WideningIntBinary config (Identity op) _ (Identity masking) _ _) =
      vectorConfigFeature config
        <> maskingFeature masking
        <> wideningIntBinaryOpCodeFeature op

instance ToFastSketch (WideningIntBinary 'C) SketchWideningIntBinary where
  toFastSketch (WideningIntBinary vtype binOp dest mask hasWideLhs rhs) =
    SketchWideningIntBinary
      vtype
      (toList binOp)
      (toList dest)
      (toList mask)
      [hasWideLhs]
      (toFastSketch rhs)

instance (EvalMode mode) => ScaleLMul (WideningIntBinary mode) where
  scaleLMul ratio op = op & (vectorConfigWide %~ scaleLMul ratio)

instance ScaleLMul SketchWideningIntBinary where
  scaleLMul ratio op = op & (vectorConfigWide %~ scaleLMul ratio)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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

module RVV.Synthesizer.Operator.WideningMulAdd
  ( WideningMulAdd (..),
    SketchWideningMulAdd (..),
    wideningMulAdd,
    sketchWideningMulAdd,
  )
where

import Control.Applicative ((<|>))
import Control.Lens (Identity (Identity), makeFieldsNoPrefix, (%~), (&))
import Data.Foldable (Foldable (toList))
import GHC.Generics (Generic)
import Grisette
  ( GenSym,
    GenSymSimple (simpleFresh),
    Mergeable,
    PPrint (pformat),
    ToSym (toSym),
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
    (./=),
  )
import RVV.EvalMode (MonadEvalMode)
import RVV.Parser.ArgParser (maskingParser, vectorConfigParser)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.VectorConfig (VectorConfig, narrowVectorConfig)
import RVV.Synthesizer.CostModel.CostModel (CostModel (CostModel, wideningCost))
import RVV.Synthesizer.Feature.ExtractFeature (ExtractFeature (extractFeature))
import RVV.Synthesizer.Feature.FeatureSet
  ( FeatureSet,
    linearArithFeature,
    maskingFeature,
    multiplicationFeature,
    wideningVectorConfigFeature,
  )
import RVV.Synthesizer.Lens
  ( HasMasking (masking),
    HasRhs (rhs),
    HasVectorConfigWide (vectorConfigWide),
    HasWideningMulAddOpCode (wideningMulAddOpCode),
  )
import RVV.Synthesizer.OpSemantics.MulAdd
  ( applyWideningVVMulAdd,
    typeWideningVIMulAdd,
    typeWideningVVMulAdd,
    typeWideningVXMulAdd,
  )
import RVV.Synthesizer.Operator.Common.PPrint (PPrintArgList (pformatArgList), choices, pformatChoices, withName)
import RVV.Synthesizer.Operator.Common.Parser (immParser)
import RVV.Synthesizer.Operator.Common.RHSSpec (RHSSpec (ImmRHS, ScalarRHS, VectorRHS), SketchRHSSpec, augmentMulAddImmArgList, augmentMulAddScalarArgList)
import RVV.Synthesizer.Operator.Common.TypeSketchOp (typeSketchOp)
import RVV.Synthesizer.Parameter.Masking (Masking)
import RVV.Synthesizer.Parameter.WideningIntBinaryOpCode (WideningIntBinaryOpCode (WMul))
import RVV.Synthesizer.Parameter.WideningMulAddOpCode
  ( WideningMulAddOpCode,
    mkWMAccus,
    wideningMulAddOpCodeParser,
    wideningMulAddOpCodeWidenLhs,
    wideningMulAddOpCodeWidenRhs,
  )
import RVV.Synthesizer.SketchGen.ToFastSketch (ToFastSketch (toFastSketch))
import RVV.Synthesizer.Specification.ScaleLMul (ScaleLMul (scaleLMul))
import RVV.Synthesizer.Type (ValueType)
import RVV.Synthesizer.Value (Value)
import RVV.Util.Context (assert)
import RVV.Util.Derive (deriveFullExcept, deriveNoSymEval, firstModeDeriveConfig)
import Text.Megaparsec.Char (string)

data WideningMulAdd mode = WideningMulAdd
  { _vectorConfigWide :: VectorConfig,
    _wideningMulAddOpCode :: GetData mode WideningMulAddOpCode,
    _masking :: GetData mode Masking,
    _rhs :: RHSSpec mode
  }
  deriving (Generic)

data SketchWideningMulAdd = SketchWideningMulAdd
  { _vectorConfigWide :: VectorConfig,
    _wideningMulAddOpCode :: [WideningMulAddOpCode],
    _masking :: [Masking],
    _rhs :: SketchRHSSpec
  }
  deriving (Generic)

deriveFullExcept
  firstModeDeriveConfig
  [''WideningMulAdd]
  [''UnifiedSymEq]
deriveNoSymEval [''SketchWideningMulAdd]
makeFieldsNoPrefix ''SketchWideningMulAdd
makeFieldsNoPrefix ''WideningMulAdd

wideningMulAdd ::
  (WideningMulAdd mode :<: op) =>
  VectorConfig ->
  GetData mode WideningMulAddOpCode ->
  GetData mode Masking ->
  RHSSpec mode ->
  op
wideningMulAdd config opcode mask rhs =
  inj $ WideningMulAdd config opcode mask rhs

sketchWideningMulAdd ::
  (SketchWideningMulAdd :<: op) =>
  VectorConfig ->
  [WideningMulAddOpCode] ->
  [Masking] ->
  SketchRHSSpec ->
  op
sketchWideningMulAdd config opcode mask rhs =
  inj $ SketchWideningMulAdd config opcode mask rhs

applyWideningMulAdd ::
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  GetData mode WideningMulAddOpCode ->
  GetData mode Masking ->
  [Value mode] ->
  ctx [Value mode]
applyWideningMulAdd vconst config opcode mask values = do
  opcode <- extractData opcode
  applyWideningVVMulAdd
    (wideningMulAddOpCodeWidenLhs opcode)
    (wideningMulAddOpCodeWidenRhs opcode)
    vconst
    config
    mask
    values

instance
  ( SemConstraint mode ctx,
    EvalModeConvertible opMode mode,
    MonadEvalMode opMode ctx
  ) =>
  OpSemantics MachineConfig (WideningMulAdd opMode) (Value mode) ctx
  where
  applyOp vconst _ op values =
    case toSym op of
      WideningMulAdd config opcode mask VectorRHS ->
        applyWideningMulAdd vconst config opcode mask values
      WideningMulAdd config opcode mask (ImmRHS imm) -> do
        assert
          "Unsupported operation for WideningMulAdd[imm_rhs]"
          (opcode ./= mkWMAccus :: GetBool mode)
        newValues <-
          augmentMulAddImmArgList
            vconst
            (narrowVectorConfig config)
            imm
            1
            values
        applyWideningMulAdd vconst config opcode mask newValues
      WideningMulAdd config opcode mask ScalarRHS -> do
        assert
          "Unsupported operation for WideningMulAdd[scalar_rhs]"
          (opcode ./= mkWMAccus :: GetBool mode)
        newValues <-
          augmentMulAddScalarArgList
            vconst
            (narrowVectorConfig config)
            1
            values
        applyWideningMulAdd vconst config opcode mask newValues
      _ -> error "Not supported"

instance (SemConstraint mode ctx) => OpTyping (WideningMulAdd mode) ctx where
  type OpTypeType (WideningMulAdd mode) = ValueType
  typeOp (WideningMulAdd config _ mask VectorRHS) =
    typeWideningVVMulAdd config mask
  typeOp (WideningMulAdd config _ mask (ImmRHS _)) =
    typeWideningVIMulAdd config mask
  typeOp (WideningMulAdd config _ mask ScalarRHS) =
    typeWideningVXMulAdd config mask
  typeOp _ = error "Not supported"

instance (MonadAngelicContext ctx) => OpTyping SketchWideningMulAdd ctx where
  type OpTypeType SketchWideningMulAdd = ValueType
  typeOp = typeSketchOp @(WideningMulAdd 'S)

instance OpSymmetryReduction (WideningMulAdd mode) where
  opUnreorderable = error "Should not be called"
  opCommutativeArgPos _ = mrgReturn []

instance GenSym SketchWideningMulAdd (WideningMulAdd 'S)

instance GenSymSimple SketchWideningMulAdd (WideningMulAdd 'S) where
  simpleFresh (SketchWideningMulAdd config opcode mask rhs) = do
    opcode <- chooseFresh opcode
    mask <- chooseFresh mask
    rhs <- simpleFresh rhs
    return $ WideningMulAdd config opcode mask rhs

instance GenSym (WideningMulAdd 'C) (WideningMulAdd 'S)

instance GenSymSimple (WideningMulAdd 'C) (WideningMulAdd 'S) where
  simpleFresh = return . toSym

instance PPrint (WideningMulAdd 'C) where
  pformat (WideningMulAdd config opcode mask rhs) =
    "v"
      <> pformat opcode
      <> postFix
      <> case rhs of
        ImmRHS imm ->
          pformatArgList
            (withName "wide" config, mask, withName "rhs" imm)
        _ -> pformatArgList (withName "wide" config, mask)
    where
      postFix = case rhs of
        VectorRHS -> ".vv"
        ScalarRHS -> ".vx"
        ImmRHS _ -> ".vi"
        _ -> error "Not supported"

instance OpPPrint (WideningMulAdd 'C) where
  describeArguments _ = return []

instance OpPPrint SketchWideningMulAdd where
  describeArguments _ = return []
  prefixResults _ = return []

instance PPrint SketchWideningMulAdd where
  pformat (SketchWideningMulAdd config opcode mask rhs) =
    "v"
      <> pformatChoices opcode
      <> pformatArgList (config, choices mask, rhs)

instance OpParser (WideningMulAdd 'C) where
  opParser = do
    (op, rhs) <- attrWithOp
    leftBracket
    config <- named "wide" vectorConfigParser
    comma
    mask <- maskingParser
    comma
    rhs <- case rhs of
      ImmRHS _ -> do
        comma
        imm <- named "rhs" immParser
        return $ ImmRHS imm
      _ -> return rhs
    rightBracket
    return $ WideningMulAdd config op mask rhs
    where
      attrWithOp = do
        op <- wideningMulAddOpCodeParser ".v"
        rhs <-
          (string "i" >> return (ImmRHS undefined))
            <|> (string "v" >> return VectorRHS)
            <|> (string "x" >> return ScalarRHS)
        return (op, rhs)

instance OpReachableSymbols (WideningMulAdd 'C) where
  opReachableSymbols _ = mempty

instance SplitChoice SketchWideningMulAdd where
  splitChoice (SketchWideningMulAdd config opcode mask rhs) = do
    opcode <- opcode
    mask <- mask
    rhs <- splitChoice rhs
    return $ SketchWideningMulAdd config [opcode] [mask] rhs

instance
  (SemConstraint mode ctx, Num cost, Mergeable cost) =>
  OpCost CostModel (WideningMulAdd mode) cost ctx
  where
  opCost CostModel {..} _ (WideningMulAdd config _ _ _) =
    mrgReturn $ fromIntegral $ wideningCost WMul config

instance ExtractFeature (WideningMulAdd 'C) FeatureSet where
  extractFeature (WideningMulAdd config _ (Identity mask) _) =
    wideningVectorConfigFeature config
      <> maskingFeature mask
      <> multiplicationFeature
      <> linearArithFeature

instance ToFastSketch (WideningMulAdd 'C) SketchWideningMulAdd where
  toFastSketch (WideningMulAdd config opcode mask rhs) =
    SketchWideningMulAdd config (toList opcode) (toList mask) (toFastSketch rhs)

instance ScaleLMul (WideningMulAdd 'C) where
  scaleLMul ratio op = op & (vectorConfigWide %~ scaleLMul ratio)

instance ScaleLMul SketchWideningMulAdd where
  scaleLMul ratio op = op & (vectorConfigWide %~ scaleLMul ratio)

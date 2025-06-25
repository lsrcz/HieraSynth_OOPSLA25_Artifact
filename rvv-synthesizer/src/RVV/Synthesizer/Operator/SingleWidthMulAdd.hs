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

module RVV.Synthesizer.Operator.SingleWidthMulAdd
  ( SingleWidthMulAdd (..),
    SketchSingleWidthMulAdd (..),
    singleWidthMulAdd,
    sketchSingleWidthMulAdd,
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
    GetData,
    UnifiedSymEq,
    extractData,
  )
import RVV.EvalMode (MonadEvalMode)
import RVV.Parser.ArgParser (maskingParser, vectorConfigParser)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.VectorConfig (VectorConfig)
import RVV.Synthesizer.CostModel.CostModel (CostModel (CostModel, singleWidthCost))
import RVV.Synthesizer.Feature.ExtractFeature (ExtractFeature (extractFeature))
import RVV.Synthesizer.Feature.FeatureSet
  ( FeatureSet,
    linearArithFeature,
    maskingFeature,
    multiplicationFeature,
    vectorConfigFeature,
  )
import RVV.Synthesizer.Lens
  ( HasMasking (masking),
    HasRhs (rhs),
    HasSingleWidthMulAddOpCode (singleWidthMulAddOpCode),
    HasVectorConfig (vectorConfig),
  )
import RVV.Synthesizer.OpSemantics.MulAdd
  ( applySingleWidthVVMulAdd,
    typeSingleWidthVIMulAdd,
    typeSingleWidthVVMulAdd,
    typeSingleWidthVXMulAdd,
  )
import RVV.Synthesizer.Operator.Common.PPrint (PPrintArgList (pformatArgList), choices, pformatChoices, withName)
import RVV.Synthesizer.Operator.Common.Parser (immParser)
import RVV.Synthesizer.Operator.Common.RHSSpec (RHSSpec (ImmRHS, ScalarRHS, VectorRHS), SketchRHSSpec, augmentMulAddImmArgList, augmentMulAddScalarArgList)
import RVV.Synthesizer.Operator.Common.TypeSketchOp (typeSketchOp)
import RVV.Synthesizer.Parameter.Masking (Masking)
import RVV.Synthesizer.Parameter.SingleWidthIntBinaryOpCode
  ( SingleWidthIntBinaryOpCode (Mul),
  )
import RVV.Synthesizer.Parameter.SingleWidthMulAddOpCode
  ( SingleWidthMulAddOpCode,
    interpretSingleWidthMulAddOpCode,
    singleWidthMulAddOpCodeParser,
  )
import RVV.Synthesizer.SketchGen.ToFastSketch (ToFastSketch (toFastSketch))
import RVV.Synthesizer.Specification.ScaleLMul (ScaleLMul (scaleLMul))
import RVV.Synthesizer.Type (ValueType)
import RVV.Synthesizer.Value (Value)
import RVV.Util.Derive (deriveFullExcept, deriveNoSymEval, firstModeDeriveConfig)
import Text.Megaparsec.Char (string)

data SingleWidthMulAdd mode = SingleWidthMulAdd
  { _vectorConfig :: VectorConfig,
    _singleWidthMulAddOpCode :: GetData mode SingleWidthMulAddOpCode,
    _masking :: GetData mode Masking,
    _rhs :: RHSSpec mode
  }
  deriving (Generic)

data SketchSingleWidthMulAdd = SketchSingleWidthMulAdd
  { _vectorConfig :: VectorConfig,
    _singleWidthMulAddOpCode :: [SingleWidthMulAddOpCode],
    _masking :: [Masking],
    _rhs :: SketchRHSSpec
  }
  deriving (Generic)

deriveFullExcept
  firstModeDeriveConfig
  [''SingleWidthMulAdd]
  [''UnifiedSymEq]
deriveNoSymEval [''SketchSingleWidthMulAdd]
makeFieldsNoPrefix ''SketchSingleWidthMulAdd
makeFieldsNoPrefix ''SingleWidthMulAdd

singleWidthMulAdd ::
  (SingleWidthMulAdd mode :<: op) =>
  VectorConfig ->
  GetData mode SingleWidthMulAddOpCode ->
  GetData mode Masking ->
  RHSSpec mode ->
  op
singleWidthMulAdd config opcode mask rhs =
  inj $ SingleWidthMulAdd config opcode mask rhs

sketchSingleWidthMulAdd ::
  (SketchSingleWidthMulAdd :<: op) =>
  VectorConfig ->
  [SingleWidthMulAddOpCode] ->
  [Masking] ->
  SketchRHSSpec ->
  op
sketchSingleWidthMulAdd config opcode mask rhs =
  inj $ SketchSingleWidthMulAdd config opcode mask rhs

applySingleWidthMulAdd ::
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  GetData mode SingleWidthMulAddOpCode ->
  GetData mode Masking ->
  [Value mode] ->
  ctx [Value mode]
applySingleWidthMulAdd vconst config opcode mask values = do
  opcode <- extractData opcode
  applySingleWidthVVMulAdd
    vconst
    config
    (\a b c -> mrgReturn $ interpretSingleWidthMulAddOpCode opcode a b c)
    mask
    values

instance
  ( SemConstraint mode ctx,
    EvalModeConvertible opMode mode,
    MonadEvalMode opMode ctx
  ) =>
  OpSemantics MachineConfig (SingleWidthMulAdd opMode) (Value mode) ctx
  where
  applyOp vconst _ op values =
    case toSym op of
      SingleWidthMulAdd config opcode mask VectorRHS ->
        applySingleWidthMulAdd vconst config opcode mask values
      SingleWidthMulAdd config opcode mask ScalarRHS -> do
        newValues <- augmentMulAddScalarArgList vconst config 1 values
        applySingleWidthMulAdd vconst config opcode mask newValues
      SingleWidthMulAdd config opcode mask (ImmRHS imm) -> do
        newValues <- augmentMulAddImmArgList vconst config imm 1 values
        applySingleWidthMulAdd vconst config opcode mask newValues
      _ -> error "Not supported"

instance (SemConstraint mode ctx) => OpTyping (SingleWidthMulAdd mode) ctx where
  type OpTypeType (SingleWidthMulAdd mode) = ValueType
  typeOp (SingleWidthMulAdd config _ mask VectorRHS) =
    typeSingleWidthVVMulAdd config mask
  typeOp (SingleWidthMulAdd config _ mask ScalarRHS) =
    typeSingleWidthVXMulAdd config mask
  typeOp (SingleWidthMulAdd config _ mask (ImmRHS _)) =
    typeSingleWidthVIMulAdd config mask
  typeOp _ = error "Not supported"

instance (MonadAngelicContext ctx) => OpTyping SketchSingleWidthMulAdd ctx where
  type OpTypeType SketchSingleWidthMulAdd = ValueType
  typeOp = typeSketchOp @(SingleWidthMulAdd 'S)

instance OpSymmetryReduction (SingleWidthMulAdd mode) where
  opUnreorderable = error "Should not be called"
  opCommutativeArgPos _ = mrgReturn []

instance GenSym SketchSingleWidthMulAdd (SingleWidthMulAdd 'S)

instance GenSymSimple SketchSingleWidthMulAdd (SingleWidthMulAdd 'S) where
  simpleFresh (SketchSingleWidthMulAdd config opcode mask rhs) = do
    opcode <- chooseFresh opcode
    mask <- chooseFresh mask
    rhs <- simpleFresh rhs
    return $ SingleWidthMulAdd config opcode mask rhs

instance GenSym (SingleWidthMulAdd 'C) (SingleWidthMulAdd 'S)

instance GenSymSimple (SingleWidthMulAdd 'C) (SingleWidthMulAdd 'S) where
  simpleFresh = return . toSym

instance PPrint (SingleWidthMulAdd 'C) where
  pformat (SingleWidthMulAdd config opcode mask rhs) =
    "v"
      <> pformat opcode
      <> postFix
      <> case rhs of
        ImmRHS imm -> pformatArgList (config, mask, withName "rhs" imm)
        _ -> pformatArgList (config, mask)
    where
      postFix = case rhs of
        VectorRHS -> ".vv"
        ScalarRHS -> ".vx"
        ImmRHS _ -> ".vi"
        _ -> error "Not supported"

instance OpPPrint (SingleWidthMulAdd 'C) where
  describeArguments _ = return []

instance OpPPrint SketchSingleWidthMulAdd where
  describeArguments _ = return []
  prefixResults _ = return []

instance PPrint SketchSingleWidthMulAdd where
  pformat (SketchSingleWidthMulAdd config opcode mask rhs) =
    "v"
      <> pformatChoices opcode
      <> pformatArgList (config, choices mask, rhs)

instance OpParser (SingleWidthMulAdd 'C) where
  opParser = do
    (op, rhs) <- attrWithOp
    leftBracket
    config <- vectorConfigParser
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
    return $ SingleWidthMulAdd config op mask rhs
    where
      attrWithOp = do
        op <- singleWidthMulAddOpCodeParser ".v"
        rhs <-
          (string "i" >> return (ImmRHS undefined))
            <|> (string "v" >> return VectorRHS)
            <|> (string "x" >> return ScalarRHS)
        return (op, rhs)

instance OpReachableSymbols (SingleWidthMulAdd 'C) where
  opReachableSymbols _ = mempty

instance SplitChoice SketchSingleWidthMulAdd where
  splitChoice (SketchSingleWidthMulAdd config opcode mask rhs) = do
    opcode <- opcode
    mask <- mask
    rhs <- splitChoice rhs
    return $ SketchSingleWidthMulAdd config [opcode] [mask] rhs

instance
  (SemConstraint mode ctx, Num cost, Mergeable cost) =>
  OpCost CostModel (SingleWidthMulAdd mode) cost ctx
  where
  opCost CostModel {..} _ (SingleWidthMulAdd config _ _ _) =
    mrgReturn $ fromIntegral $ singleWidthCost Mul config

instance ExtractFeature (SingleWidthMulAdd 'C) FeatureSet where
  extractFeature (SingleWidthMulAdd config _ (Identity mask) _) =
    vectorConfigFeature config
      <> maskingFeature mask
      <> multiplicationFeature
      <> linearArithFeature

instance ToFastSketch (SingleWidthMulAdd 'C) SketchSingleWidthMulAdd where
  toFastSketch (SingleWidthMulAdd config opcode mask rhs) =
    SketchSingleWidthMulAdd
      config
      (toList opcode)
      (toList mask)
      (toFastSketch rhs)

instance ScaleLMul (SingleWidthMulAdd 'C) where
  scaleLMul ratio op = op & (vectorConfig %~ scaleLMul ratio)

instance ScaleLMul SketchSingleWidthMulAdd where
  scaleLMul ratio op = op & (vectorConfig %~ scaleLMul ratio)

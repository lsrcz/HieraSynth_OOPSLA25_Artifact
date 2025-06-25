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

module RVV.Synthesizer.Operator.NarrowingRightShift
  ( NarrowingRightShift (..),
    SketchNarrowingRightShift (..),
    narrowingRightShift,
    sketchNarrowingRightShift,
  )
where

import Control.Applicative (Alternative ((<|>)))
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
    GetData,
    UnifiedSymEq,
    extractData,
  )
import RVV.EvalMode (MonadEvalMode)
import RVV.Parser.ArgParser
  ( destinationParser,
    maskingParser,
    vectorConfigParser,
  )
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.VectorConfig (VectorConfig, widenVectorConfig)
import RVV.Synthesizer.CostModel.CostModel
  ( CostModel (CostModel, singleWidthCost),
  )
import RVV.Synthesizer.Feature.ExtractFeature (ExtractFeature (extractFeature))
import RVV.Synthesizer.Feature.FeatureSet
  ( FeatureSet,
    maskingFeature,
    narrowingVectorConfigFeature,
  )
import RVV.Synthesizer.Lens
  ( HasDestination (destination),
    HasMasking (masking),
    HasNarrowingRightShiftOpCode (narrowingRightShiftOpCode),
    HasRhs (rhs),
    HasVectorConfigNarrow (vectorConfigNarrow),
  )
import RVV.Synthesizer.OpSemantics.ElementWise
  ( applyNarrowingRightShiftWVElementWise,
    typeNarrowingRightShiftWIElementWise,
    typeNarrowingRightShiftWVElementWise,
    typeNarrowingRightShiftWXElementWise,
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
    augmentFullScalarVXArgList,
  )
import RVV.Synthesizer.Operator.Common.TypeSketchOp (typeSketchOp)
import RVV.Synthesizer.Parameter.Destination (Destination)
import RVV.Synthesizer.Parameter.Masking (Masking)
import RVV.Synthesizer.Parameter.NarrowingRightShiftOpCode
  ( NarrowingRightShiftOpCode,
    interpretNarrowingRightShiftOpCode,
    narrowingRightShiftNarrowingResult,
    narrowingRightShiftOpCodeParser,
    narrowingRightShiftWidenRhs,
  )
import RVV.Synthesizer.Parameter.SingleWidthIntBinaryOpCode
  ( SingleWidthIntBinaryOpCode (Srl),
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

data NarrowingRightShift mode = NarrowingRightShift
  { _vectorConfigNarrow :: VectorConfig,
    _narrowingRightShiftOpCode :: GetData mode NarrowingRightShiftOpCode,
    _destination :: GetData mode Destination,
    _masking :: GetData mode Masking,
    _rhs :: RHSSpec mode
  }
  deriving (Generic)

data SketchNarrowingRightShift = SketchNarrowingRightShift
  { _vectorConfigNarrow :: VectorConfig,
    _narrowingRightShiftOpCode :: [NarrowingRightShiftOpCode],
    _destination :: [Destination],
    _masking :: [Masking],
    _rhs :: SketchRHSSpec
  }
  deriving (Generic)

deriveFullExcept
  firstModeDeriveConfig
  [''NarrowingRightShift]
  [''UnifiedSymEq]
deriveNoSymEval [''SketchNarrowingRightShift]
makeFieldsNoPrefix ''SketchNarrowingRightShift
makeFieldsNoPrefix ''NarrowingRightShift

narrowingRightShift ::
  (NarrowingRightShift mode :<: op) =>
  VectorConfig ->
  GetData mode NarrowingRightShiftOpCode ->
  GetData mode Destination ->
  GetData mode Masking ->
  RHSSpec mode ->
  op
narrowingRightShift config opcode dest mask rhs =
  inj $ NarrowingRightShift config opcode dest mask rhs

sketchNarrowingRightShift ::
  (SketchNarrowingRightShift :<: op) =>
  VectorConfig ->
  [NarrowingRightShiftOpCode] ->
  [Destination] ->
  [Masking] ->
  SketchRHSSpec ->
  op
sketchNarrowingRightShift config opcode dest mask rhs =
  inj $ SketchNarrowingRightShift config opcode dest mask rhs

applyNarrowingRightShift ::
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  GetData mode NarrowingRightShiftOpCode ->
  GetData mode Destination ->
  GetData mode Masking ->
  [Value mode] ->
  ctx [Value mode]
applyNarrowingRightShift vconst config binOp dest mask values = do
  binOp <- extractData binOp
  applyNarrowingRightShiftWVElementWise
    vconst
    config
    (narrowingRightShiftWidenRhs binOp)
    (narrowingRightShiftNarrowingResult binOp)
    ( \a b -> mrgReturn $ interpretNarrowingRightShiftOpCode binOp a b
    )
    dest
    mask
    values

instance
  ( SemConstraint mode ctx,
    EvalModeConvertible opMode mode,
    MonadEvalMode opMode ctx
  ) =>
  OpSemantics MachineConfig (NarrowingRightShift opMode) (Value mode) ctx
  where
  applyOp vconst _ op values =
    case toSym op of
      NarrowingRightShift config binOp dest mask VectorRHS ->
        applyNarrowingRightShift vconst config binOp dest mask values
      NarrowingRightShift config binOp dest mask ScalarRHS -> do
        newValues <- augmentFullScalarVXArgList vconst config 2 values
        applyNarrowingRightShift vconst config binOp dest mask newValues
      NarrowingRightShift config binOp dest mask FullScalarRHS -> do
        newValues <- augmentFullScalarVXArgList vconst config 2 values
        applyNarrowingRightShift vconst config binOp dest mask newValues
      NarrowingRightShift config binOp dest mask (ImmRHS imm) -> do
        newValues <- augmentElementWiseVIArgList vconst config imm 2 values
        applyNarrowingRightShift vconst config binOp dest mask newValues

instance (SemConstraint mode ctx) => OpTyping (NarrowingRightShift mode) ctx where
  type OpTypeType (NarrowingRightShift mode) = ValueType
  typeOp (NarrowingRightShift config _ dest mask VectorRHS) =
    typeNarrowingRightShiftWVElementWise config dest mask
  typeOp (NarrowingRightShift config _ dest mask ScalarRHS) =
    typeNarrowingRightShiftWXElementWise config dest mask
  typeOp (NarrowingRightShift config _ dest mask FullScalarRHS) =
    typeNarrowingRightShiftWXElementWise config dest mask
  typeOp (NarrowingRightShift config _ dest mask (ImmRHS _)) =
    typeNarrowingRightShiftWIElementWise config dest mask

instance (MonadAngelicContext ctx) => OpTyping SketchNarrowingRightShift ctx where
  type OpTypeType SketchNarrowingRightShift = ValueType
  typeOp = typeSketchOp @(NarrowingRightShift 'S)

instance OpSymmetryReduction (NarrowingRightShift mode) where
  opUnreorderable = error "Should not be called"
  opCommutativeArgPos _ = mrgReturn []

instance GenSym SketchNarrowingRightShift (NarrowingRightShift 'S)

instance GenSymSimple SketchNarrowingRightShift (NarrowingRightShift 'S) where
  simpleFresh (SketchNarrowingRightShift config opcodes dests masks rhs) = do
    opcodes <- chooseFresh opcodes
    dests <- chooseFresh dests
    masks <- chooseFresh masks
    rhs <- simpleFresh rhs
    return $ NarrowingRightShift config opcodes dests masks rhs

instance GenSym (NarrowingRightShift 'C) (NarrowingRightShift 'S)

instance GenSymSimple (NarrowingRightShift 'C) (NarrowingRightShift 'S) where
  simpleFresh = return . toSym

instance PPrint (NarrowingRightShift 'C) where
  pformat (NarrowingRightShift config opcode dest mask rhs) =
    "v"
      <> pformat opcode
      <> "."
      <> postFix
      <> case rhs of
        ImmRHS imm ->
          pformatArgList
            ( withName "narrow" config,
              dest,
              mask,
              withName "rhs" imm
            )
        _ -> pformatArgList (withName "narrow" config, dest, mask)
    where
      postFix = case rhs of
        ImmRHS _ -> "wi"
        ScalarRHS -> "wx"
        FullScalarRHS -> "wx.full"
        VectorRHS -> "wv"

instance OpPPrint (NarrowingRightShift 'C) where
  describeArguments _ = return []

instance OpPPrint SketchNarrowingRightShift where
  describeArguments _ = return []
  prefixResults _ = return []

instance PPrint SketchNarrowingRightShift where
  pformat (SketchNarrowingRightShift config opcodes dests masks rhs) =
    "v"
      <> pformatChoices opcodes
      <> pformatArgList (config, choices dests, choices masks, rhs)

instance OpParser (NarrowingRightShift 'C) where
  opParser = do
    (op, rhs) <- attrWithOp
    leftBracket
    config <- named "narrow" vectorConfigParser
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
    return $ NarrowingRightShift config op dest mask rhs
    where
      attrWithOp = do
        op <- narrowingRightShiftOpCodeParser ".w"
        rhs <-
          (string "i" >> return (ImmRHS undefined))
            <|> (string "v" >> return VectorRHS)
            <|> (string "x" >> return ScalarRHS)
            <|> (string "x.full" >> return FullScalarRHS)
        return (op, rhs)

instance OpReachableSymbols (NarrowingRightShift 'C) where
  opReachableSymbols _ = mempty

instance SplitChoice SketchNarrowingRightShift where
  splitChoice (SketchNarrowingRightShift config opcodes dests masks rhs) = do
    opcode <- opcodes
    dest <- dests
    mask <- masks
    rhs' <- splitChoice rhs
    return $ SketchNarrowingRightShift config [opcode] [dest] [mask] rhs'

instance
  (SemConstraint mode ctx, Num cost, Mergeable cost) =>
  OpCost CostModel (NarrowingRightShift mode) cost ctx
  where
  opCost CostModel {..} _ (NarrowingRightShift config _ _ _ _) = do
    mrgReturn $ fromIntegral $ singleWidthCost Srl (widenVectorConfig config)

instance ExtractFeature (NarrowingRightShift 'C) FeatureSet where
  extractFeature (NarrowingRightShift config _ _ (Identity masking) _) =
    narrowingVectorConfigFeature config <> maskingFeature masking

instance ToFastSketch (NarrowingRightShift 'C) SketchNarrowingRightShift where
  toFastSketch (NarrowingRightShift config opcode dest mask rhs) =
    SketchNarrowingRightShift
      config
      (toList opcode)
      (toList dest)
      (toList mask)
      (toFastSketch rhs)

instance ScaleLMul (NarrowingRightShift 'C) where
  scaleLMul ratio op = op & (vectorConfigNarrow %~ scaleLMul ratio)

instance ScaleLMul SketchNarrowingRightShift where
  scaleLMul ratio op = op & (vectorConfigNarrow %~ scaleLMul ratio)

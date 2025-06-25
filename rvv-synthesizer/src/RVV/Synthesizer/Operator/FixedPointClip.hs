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

module RVV.Synthesizer.Operator.FixedPointClip
  ( FixedPointClip (..),
    SketchFixedPointClip (..),
    fixedPointClip,
    sketchFixedPointClip,
  )
where

import Control.Applicative ((<|>))
import Control.Lens (Identity (Identity), makeFieldsNoPrefix, (%~), (&))
import Data.Foldable (Foldable (toList))
import qualified Data.HashSet as HS
import Data.List (nub, sort)
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
  )
import RVV.EvalMode (MonadEvalMode)
import RVV.Parser.ArgParser
  ( destinationParser,
    maskingParser,
    vectorConfigParser,
  )
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.VectorConfig (VectorConfig)
import RVV.Synthesizer.CostModel.CostModel
  ( CostModel (CostModel, narrowingCost),
  )
import RVV.Synthesizer.Feature.ExtractFeature (ExtractFeature (extractFeature))
import RVV.Synthesizer.Feature.FeatureSet
  ( FeatureSet (opFeatures),
    maskingFeature,
    narrowingVectorConfigFeature,
  )
import qualified RVV.Synthesizer.Feature.FeatureSet as Feature
import RVV.Synthesizer.Lens
  ( HasDestination (destination),
    HasFixedPointRoundingMode (fixedPointRoundingMode),
    HasMasking (masking),
    HasRhs (rhs),
    HasSigned (signed),
    HasVectorConfigNarrow (vectorConfigNarrow),
  )
import RVV.Synthesizer.OpSemantics.ElementWise
  ( applyFixedPointClipWVElementWise,
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
import RVV.Synthesizer.Parameter.FixedPointRoundingMode
  ( FixedPointRoundingMode,
    fixedPointClipSignedRoundingModeParser,
    fixedPointRoundingModeFeature,
    interpretFixedPointClip,
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

data FixedPointClip mode = FixedPointClip
  { _vectorConfigNarrow :: VectorConfig,
    _signed :: GetBool mode,
    _fixedPointRoundingMode :: GetData mode FixedPointRoundingMode,
    _destination :: GetData mode Destination,
    _masking :: GetData mode Masking,
    _rhs :: RHSSpec mode
  }
  deriving (Generic)

data SketchFixedPointClip = SketchFixedPointClip
  { _vectorConfigNarrow :: VectorConfig,
    _signed :: [Bool],
    _fixedPointRoundingMode :: [FixedPointRoundingMode],
    _destination :: [Destination],
    _masking :: [Masking],
    _rhs :: SketchRHSSpec
  }
  deriving (Generic)

deriveFullExcept
  firstModeDeriveConfig
  [''FixedPointClip]
  [''UnifiedSymEq]
deriveNoSymEval [''SketchFixedPointClip]
makeFieldsNoPrefix ''SketchFixedPointClip
makeFieldsNoPrefix ''FixedPointClip

fixedPointClip ::
  (FixedPointClip mode :<: op) =>
  VectorConfig ->
  GetBool mode ->
  GetData mode FixedPointRoundingMode ->
  GetData mode Destination ->
  GetData mode Masking ->
  RHSSpec mode ->
  op
fixedPointClip config signed roundingMode dest mask rhs =
  inj $ FixedPointClip config signed roundingMode dest mask rhs

sketchFixedPointClip ::
  (SketchFixedPointClip :<: op) =>
  VectorConfig ->
  [Bool] ->
  [FixedPointRoundingMode] ->
  [Destination] ->
  [Masking] ->
  SketchRHSSpec ->
  op
sketchFixedPointClip config signed roundingMode dest mask rhs =
  inj $ SketchFixedPointClip config signed roundingMode dest mask rhs

applyFixedPointClip ::
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  GetBool mode ->
  GetData mode FixedPointRoundingMode ->
  GetData mode Destination ->
  GetData mode Masking ->
  [Value mode] ->
  ctx [Value mode]
applyFixedPointClip vconst config signed roundingMode dest mask values = do
  roundingMode <- extractData roundingMode
  applyFixedPointClipWVElementWise
    vconst
    config
    ( \a b -> mrgReturn $ interpretFixedPointClip roundingMode signed a b
    )
    dest
    mask
    values

instance
  ( SemConstraint mode ctx,
    EvalModeConvertible opMode mode,
    MonadEvalMode opMode ctx
  ) =>
  OpSemantics MachineConfig (FixedPointClip opMode) (Value mode) ctx
  where
  applyOp vconst _ op values =
    case toSym op of
      FixedPointClip config signed roundingMode dest mask VectorRHS ->
        applyFixedPointClip vconst config signed roundingMode dest mask values
      FixedPointClip config signed roundingMode dest mask ScalarRHS -> do
        newValues <- augmentFullScalarVXArgList vconst config 2 values
        applyFixedPointClip vconst config signed roundingMode dest mask newValues
      FixedPointClip config signed roundingMode dest mask FullScalarRHS -> do
        newValues <- augmentFullScalarVXArgList vconst config 2 values
        applyFixedPointClip vconst config signed roundingMode dest mask newValues
      FixedPointClip config signed roundingMode dest mask (ImmRHS imm) -> do
        newValues <- augmentElementWiseVIArgList vconst config imm 2 values
        applyFixedPointClip vconst config signed roundingMode dest mask newValues

instance (SemConstraint mode ctx) => OpTyping (FixedPointClip mode) ctx where
  type OpTypeType (FixedPointClip mode) = ValueType
  typeOp (FixedPointClip config _ _ dest mask VectorRHS) =
    typeNarrowingRightShiftWVElementWise config dest mask
  typeOp (FixedPointClip config _ _ dest mask ScalarRHS) =
    typeNarrowingRightShiftWXElementWise config dest mask
  typeOp (FixedPointClip config _ _ dest mask FullScalarRHS) =
    typeNarrowingRightShiftWXElementWise config dest mask
  typeOp (FixedPointClip config _ _ dest mask (ImmRHS _)) =
    typeNarrowingRightShiftWIElementWise config dest mask

instance (MonadAngelicContext ctx) => OpTyping SketchFixedPointClip ctx where
  type OpTypeType SketchFixedPointClip = ValueType
  typeOp = typeSketchOp @(FixedPointClip 'S)

instance OpSymmetryReduction (FixedPointClip mode) where
  opUnreorderable = error "Should not be called"
  opCommutativeArgPos _ = mrgReturn []

instance GenSym SketchFixedPointClip (FixedPointClip 'S)

instance GenSymSimple SketchFixedPointClip (FixedPointClip 'S) where
  simpleFresh (SketchFixedPointClip config signed roundingMode dest mask rhs) = do
    signed <- case nub $ sort signed of
      [c] -> return $ toSym c
      _ -> simpleFresh ()
    roundingMode <- chooseFresh roundingMode
    dest <- chooseFresh dest
    mask <- chooseFresh mask
    rhs <- simpleFresh rhs
    return $ FixedPointClip config signed roundingMode dest mask rhs

instance GenSym (FixedPointClip 'C) (FixedPointClip 'S)

instance GenSymSimple (FixedPointClip 'C) (FixedPointClip 'S) where
  simpleFresh = return . toSym

instance PPrint (FixedPointClip 'C) where
  pformat (FixedPointClip config signed roundingMode dest mask rhs) =
    "vnclip"
      <> (if signed then "" else "u")
      <> "."
      <> pformat roundingMode
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
        VectorRHS -> "wv"
        ScalarRHS -> "wx"
        FullScalarRHS -> "wx.full"
        ImmRHS _ -> "wi"

instance OpPPrint (FixedPointClip 'C) where
  describeArguments _ = return []

instance OpPPrint SketchFixedPointClip where
  describeArguments _ = return []
  prefixResults _ = return []

instance PPrint SketchFixedPointClip where
  pformat (SketchFixedPointClip config signed roundingMode dest mask rhs) =
    "vnclip"
      <> pformatChoices (map (\v -> if v then ("s" :: String) else "u") signed)
      <> "."
      <> pformatChoices roundingMode
      <> pformatArgList (config, choices dest, choices mask, rhs)

instance OpParser (FixedPointClip 'C) where
  opParser = do
    (signed, roundingMode, rhs) <- attrWithSignedMode
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
    return $ FixedPointClip config signed roundingMode dest mask rhs
    where
      attrWithSignedMode = do
        (signed, roundingMode) <- fixedPointClipSignedRoundingModeParser ".w"
        rhs <-
          (string "i" >> return (ImmRHS undefined))
            <|> (string "v" >> return VectorRHS)
            <|> (string "x" >> return ScalarRHS)
            <|> (string "x.full" >> return FullScalarRHS)
        return (signed, roundingMode, rhs)

instance OpReachableSymbols (FixedPointClip 'C) where
  opReachableSymbols _ = mempty

instance SplitChoice SketchFixedPointClip where
  splitChoice
    (SketchFixedPointClip config signed roundingMode dests masks rhs) = do
      signed <- signed
      roundingMode <- roundingMode
      dest <- dests
      mask <- masks
      rhs' <- splitChoice rhs
      return $
        SketchFixedPointClip config [signed] [roundingMode] [dest] [mask] rhs'

instance
  (SemConstraint mode ctx, Num cost, Mergeable cost) =>
  OpCost CostModel (FixedPointClip mode) cost ctx
  where
  opCost CostModel {..} _ (FixedPointClip config _ _ _ _ _) =
    mrgReturn $ fromIntegral $ narrowingCost config

instance ExtractFeature (FixedPointClip 'C) FeatureSet where
  extractFeature
    (FixedPointClip config _ (Identity roundingMode) _ (Identity mask) _) =
      narrowingVectorConfigFeature config
        <> maskingFeature mask
        <> fixedPointRoundingModeFeature roundingMode
        <> mempty {opFeatures = HS.singleton Feature.FixedPointClip}

instance ToFastSketch (FixedPointClip 'C) SketchFixedPointClip where
  toFastSketch (FixedPointClip config signed roundingMode dest mask rhs) =
    SketchFixedPointClip
      config
      [signed]
      (toList roundingMode)
      (toList dest)
      (toList mask)
      (toFastSketch rhs)

instance ScaleLMul (FixedPointClip 'C) where
  scaleLMul ratio op = op & (vectorConfigNarrow %~ scaleLMul ratio)

instance ScaleLMul SketchFixedPointClip where
  scaleLMul ratio op = op & (vectorConfigNarrow %~ scaleLMul ratio)

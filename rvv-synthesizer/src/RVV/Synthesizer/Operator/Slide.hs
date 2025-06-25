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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.Operator.Slide
  ( slide,
    sketchSlide,
    slide1,
    sketchSlide1,
    Slide (..),
    SketchSlide (..),
  )
where

import Control.Applicative ((<|>))
import Control.Lens (Identity (Identity), makeFieldsNoPrefix, (%~), (&))
import Data.Foldable (Foldable (toList))
import qualified Data.HashSet as HS
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
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
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
import RVV.Parser.ArgParser (destinationParser, maskingParser, vectorConfigParser)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.VectorConfig (VectorConfig)
import RVV.Synthesizer.CostModel.CostModel
  ( CostModel (CostModel, slide1Cost, slideCost),
  )
import RVV.Synthesizer.Feature.ExtractFeature (ExtractFeature (extractFeature))
import qualified RVV.Synthesizer.Feature.FeatureSet as Feature
import RVV.Synthesizer.Feature.FeatureSet
  ( FeatureSet (opFeatures),
    maskingFeature,
    vectorConfigFeature,
  )
import RVV.Synthesizer.Lens
  ( HasDestination (destination),
    HasMasking (masking),
    HasRhs (rhs),
    HasSlideDirection (slideDirection),
    HasVectorConfig (vectorConfig),
  )
import RVV.Synthesizer.OpSemantics.Slide
  ( applySlide,
    typeSlide1VI,
    typeSlide1VX,
    typeSlideVI,
    typeSlideVX,
  )
import RVV.Synthesizer.Operator.Common.PPrint
  ( PPrintArgList (pformatArgList),
    choices,
    pformatChoices,
    withName,
  )
import RVV.Synthesizer.Operator.Common.Parser (immParser)
import RVV.Synthesizer.Operator.Common.RHSSpec
  ( RHSSpec (ImmRHS, ScalarRHS),
    SketchRHSSpec,
    augmentSlideVIArgList,
  )
import RVV.Synthesizer.Operator.Common.TypeSketchOp (typeSketchOp)
import RVV.Synthesizer.Parameter.Common (oneOfOpCodeParser)
import RVV.Synthesizer.Parameter.Destination (Destination)
import RVV.Synthesizer.Parameter.Masking (Masking)
import RVV.Synthesizer.Parameter.SlideDirection
  ( SlideDirection,
    slide1DirectionParser,
    slideDirectionParser,
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

-- | Vector slide operations including vslide and vslide1
data Slide mode
  = Slide
      { _vectorConfig :: VectorConfig,
        _slideDirection :: GetData mode SlideDirection,
        _destination :: GetData mode Destination,
        _masking :: GetData mode Masking,
        _rhs :: RHSSpec mode
      }
  | Slide1
      { _vectorConfig :: VectorConfig,
        _slideDirection :: GetData mode SlideDirection,
        _destination :: GetData mode Destination,
        _masking :: GetData mode Masking,
        _rhs :: RHSSpec mode
      }
  deriving (Generic)

data SketchSlide
  = SketchSlide
      { _vectorConfig :: VectorConfig,
        _slideDirection :: [SlideDirection],
        _destination :: [Destination],
        _masking :: [Masking],
        _rhs :: SketchRHSSpec
      }
  | SketchSlide1
      { _vectorConfig :: VectorConfig,
        _slideDirection :: [SlideDirection],
        _destination :: [Destination],
        _masking :: [Masking],
        _rhs :: SketchRHSSpec
      }
  deriving (Generic)

deriveFullExcept
  firstModeDeriveConfig
  [''Slide]
  [''UnifiedSymEq]
deriveNoSymEval [''SketchSlide]
makeFieldsNoPrefix ''Slide
makeFieldsNoPrefix ''SketchSlide

slide ::
  (Slide mode :<: op) =>
  VectorConfig ->
  GetData mode SlideDirection ->
  GetData mode Destination ->
  GetData mode Masking ->
  RHSSpec mode ->
  op
slide config dir dest mask rhs =
  inj $ Slide config dir dest mask rhs

sketchSlide ::
  (SketchSlide :<: op) =>
  VectorConfig ->
  [SlideDirection] ->
  [Destination] ->
  [Masking] ->
  SketchRHSSpec ->
  op
sketchSlide config dir dest mask rhs =
  inj $ SketchSlide config dir dest mask rhs

slide1 ::
  (Slide mode :<: op) =>
  VectorConfig ->
  GetData mode SlideDirection ->
  GetData mode Destination ->
  GetData mode Masking ->
  RHSSpec mode ->
  op
slide1 config dir dest mask rhs =
  inj $ Slide1 config dir dest mask rhs

sketchSlide1 ::
  (SketchSlide :<: op) =>
  VectorConfig ->
  [SlideDirection] ->
  [Destination] ->
  [Masking] ->
  SketchRHSSpec ->
  op
sketchSlide1 config dir dest mask rhs =
  inj $ SketchSlide1 config dir dest mask rhs

instance
  ( SemConstraint mode ctx,
    EvalModeConvertible opMode mode,
    MonadEvalMode opMode ctx
  ) =>
  OpSemantics MachineConfig (Slide opMode) (Value mode) ctx
  where
  applyOp vconst _ op values =
    case toSym op of
      Slide vtype dir dest mask ScalarRHS -> do
        applySlide False vconst vtype dir dest mask values
      Slide vtype dir dest mask (ImmRHS imm) -> do
        newValues <- augmentSlideVIArgList False vconst vtype imm 2 values
        applySlide False vconst vtype dir dest mask newValues
      Slide {} ->
        mrgThrowError "Slide: rhs should be ScalarRHS or ImmRHS"
      Slide1 vtype dir dest mask ScalarRHS -> do
        applySlide True vconst vtype dir dest mask values
      Slide1 vtype dir dest mask (ImmRHS imm) -> do
        newValues <- augmentSlideVIArgList True vconst vtype imm 2 values
        applySlide True vconst vtype dir dest mask newValues
      Slide1 {} ->
        mrgThrowError "Slide1: rhs should be ScalarRHS or ImmRHS"

instance (SemConstraint mode ctx) => OpTyping (Slide mode) ctx where
  type OpTypeType (Slide mode) = ValueType
  typeOp (Slide vtype _ dest mask ScalarRHS) =
    typeSlideVX vtype dest mask
  typeOp (Slide vtype _ dest mask (ImmRHS _)) =
    typeSlideVI vtype dest mask
  typeOp (Slide {}) =
    mrgThrowError "Slide: rhs should be ScalarRHS or ImmRHS"
  typeOp (Slide1 vtype _ dest mask ScalarRHS) =
    typeSlide1VX vtype dest mask
  typeOp (Slide1 vtype _ dest mask (ImmRHS _)) =
    typeSlide1VI vtype dest mask
  typeOp (Slide1 {}) =
    mrgThrowError "Slide1: rhs should be ScalarRHS or ImmRHS"

instance (MonadAngelicContext ctx) => OpTyping SketchSlide ctx where
  type OpTypeType SketchSlide = ValueType
  typeOp = typeSketchOp @(Slide 'S)

instance OpSymmetryReduction (Slide mode) where
  opUnreorderable = error "Should not be called"
  opCommutativeArgPos _ = mrgReturn []

instance GenSym SketchSlide (Slide 'S)

instance GenSymSimple SketchSlide (Slide 'S) where
  simpleFresh (SketchSlide config dir dest mask rhs) = do
    dir <- chooseFresh dir
    dest <- chooseFresh dest
    mask <- chooseFresh mask
    rhs <- simpleFresh rhs
    return $ Slide config dir dest mask rhs
  simpleFresh (SketchSlide1 config dir dest mask rhs) = do
    dir <- chooseFresh dir
    dest <- chooseFresh dest
    mask <- chooseFresh mask
    rhs <- simpleFresh rhs
    return $ Slide1 config dir dest mask rhs

instance GenSym (Slide 'C) (Slide 'S)

instance GenSymSimple (Slide 'C) (Slide 'S) where
  simpleFresh = return . toSym

instance PPrint (Slide 'C) where
  pformat (Slide vtype dir dest mask rhs) =
    "vslide"
      <> pformat dir
      <> "."
      <> postFix
      <> case rhs of
        ImmRHS imm ->
          pformatArgList
            ( vtype,
              dest,
              mask,
              withName "offset" imm
            )
        _ -> pformatArgList (vtype, dest, mask)
    where
      postFix = case rhs of
        ImmRHS _ -> "vi"
        ScalarRHS -> "vx"
        _ -> error "Slide: rhs should be ScalarRHS or ImmRHS"
  pformat (Slide1 vtype dir dest mask rhs) =
    "vslide1"
      <> pformat dir
      <> "."
      <> postFix
      <> case rhs of
        ImmRHS imm ->
          pformatArgList
            ( vtype,
              dest,
              mask,
              withName "rhs" imm
            )
        _ -> pformatArgList (vtype, dest, mask)
    where
      postFix = case rhs of
        ImmRHS _ -> "vi"
        ScalarRHS -> "vx"
        _ -> error "Slide1: rhs should be ScalarRHS or ImmRHS"

instance OpPPrint (Slide 'C) where
  describeArguments _ = return []

instance OpPPrint SketchSlide where
  describeArguments _ = return []
  prefixResults _ = return []

instance PPrint SketchSlide where
  pformat (SketchSlide config dir dest mask rhs) =
    "vslide"
      <> pformatChoices dir
      <> pformatArgList (config, choices dest, choices mask, rhs)
  pformat (SketchSlide1 config dir dest mask rhs) =
    "vslide1"
      <> pformatChoices dir
      <> pformatArgList (config, choices dest, choices mask, rhs)

instance OpParser (Slide 'C) where
  opParser = do
    (isSlide1, dir, rhs) <- attrWithIsSlide1
    leftBracket
    vtype <- vectorConfigParser
    comma
    dest <- destinationParser
    comma
    mask <- maskingParser
    rhs <- case rhs of
      ImmRHS _ -> do
        comma
        imm <- named (if isSlide1 then "offset" else "rhs") immParser
        return $ ImmRHS imm
      _ -> return rhs
    rightBracket
    let op = if isSlide1 then slide1 else slide
    return $ op vtype dir dest mask rhs
    where
      attrWithIsSlide1 = do
        (dir, isSlide1) <-
          oneOfOpCodeParser
            [ (slideDirectionParser ".v", False),
              (slide1DirectionParser ".v", True)
            ]
        rhs <-
          (string "i" >> return (ImmRHS undefined))
            <|> (string "x" >> return ScalarRHS)
        return (isSlide1, dir, rhs)

instance OpReachableSymbols (Slide 'C) where
  opReachableSymbols = mempty

instance SplitChoice SketchSlide where
  splitChoice (SketchSlide config dir dest mask rhs) = do
    dir <- dir
    dest <- dest
    mask <- mask
    rhs <- splitChoice rhs
    return $ SketchSlide config [dir] [dest] [mask] rhs
  splitChoice (SketchSlide1 config dir dest mask rhs) = do
    dir <- dir
    dest <- dest
    mask <- mask
    rhs <- splitChoice rhs
    return $ SketchSlide1 config [dir] [dest] [mask] rhs

instance
  (SemConstraint mode ctx, Num cost, Mergeable cost) =>
  OpCost CostModel (Slide mode) cost ctx
  where
  opCost CostModel {..} _ (Slide {..}) = do
    dir <- extractData _slideDirection
    mrgReturn $ fromIntegral $ slideCost dir _vectorConfig
  opCost CostModel {..} _ (Slide1 {..}) = do
    dir <- extractData _slideDirection
    mrgReturn $ fromIntegral $ slide1Cost dir _vectorConfig

instance ExtractFeature (Slide 'C) FeatureSet where
  extractFeature (Slide config _ _ (Identity mask) _) =
    vectorConfigFeature config
      <> maskingFeature mask
      <> mempty {opFeatures = HS.singleton Feature.Slide}
  extractFeature (Slide1 config _ _ (Identity mask) _) =
    vectorConfigFeature config
      <> maskingFeature mask
      <> mempty {opFeatures = HS.singleton Feature.Slide1}

instance ToFastSketch (Slide 'C) SketchSlide where
  toFastSketch (Slide config dir dest mask rhs) =
    SketchSlide
      config
      (toList dir)
      (toList dest)
      (toList mask)
      (toFastSketch rhs)
  toFastSketch (Slide1 config dir dest mask rhs) =
    SketchSlide1
      config
      (toList dir)
      (toList dest)
      (toList mask)
      (toFastSketch rhs)

instance ScaleLMul (Slide 'C) where
  scaleLMul ratio op = op & (vectorConfig %~ scaleLMul ratio)

instance ScaleLMul SketchSlide where
  scaleLMul ratio op = op & (vectorConfig %~ scaleLMul ratio)

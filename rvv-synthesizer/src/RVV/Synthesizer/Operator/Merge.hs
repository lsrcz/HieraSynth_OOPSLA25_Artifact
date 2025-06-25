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

module RVV.Synthesizer.Operator.Merge
  ( merge,
    sketchMerge,
    Merge (..),
    SketchMerge (..),
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Lens (makeFieldsNoPrefix, (%~), (&))
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
  )
import RVV.EvalMode (EvalMode, MonadEvalMode)
import RVV.Parser.ArgParser (destinationParser, vectorConfigParser)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Policy (muPolicy)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.VectorConfig (VectorConfig)
import RVV.Synthesizer.CostModel.CostModel
  ( CostModel (CostModel, mergeCost),
  )
import RVV.Synthesizer.Feature.ExtractFeature (ExtractFeature (extractFeature))
import RVV.Synthesizer.Feature.FeatureSet
  ( Feature (Masking),
    FeatureSet (opFeatures, policyFeatures),
    vectorConfigFeature,
  )
import RVV.Synthesizer.Lens
  ( HasDestination (destination),
    HasRhs (rhs),
    HasVectorConfig (vectorConfig),
  )
import RVV.Synthesizer.OpSemantics.Merge
  ( applyMerge,
    typeMergeVIM,
    typeMergeVVM,
    typeMergeVXM,
  )
import RVV.Synthesizer.Operator.Common.PPrint
  ( PPrintArgList (pformatArgList),
    choices,
    withName,
  )
import RVV.Synthesizer.Operator.Common.Parser (immParser)
import RVV.Synthesizer.Operator.Common.RHSSpec
  ( RHSSpec (ImmRHS, ScalarRHS, VectorRHS),
    SketchRHSSpec (SketchImmRHS, SketchScalarRHS, SketchVectorRHS),
    augmentElementWiseVIArgList,
    augmentElementWiseVXArgList,
  )
import RVV.Synthesizer.Operator.Common.TypeSketchOp (typeSketchOp)
import RVV.Synthesizer.Parameter.Destination (Destination)
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

data Merge mode
  = Merge
  { _vectorConfig :: VectorConfig,
    _destination :: GetData mode Destination,
    _rhs :: RHSSpec mode
  }

merge ::
  (Merge mode :<: op) =>
  VectorConfig ->
  GetData mode Destination ->
  RHSSpec mode ->
  op
merge vtype dest rhs =
  inj $ Merge vtype dest rhs

data SketchMerge
  = SketchMerge
  { _vectorConfig :: VectorConfig,
    _destination :: [Destination],
    _rhs :: SketchRHSSpec
  }

sketchMerge ::
  (SketchMerge :<: op) =>
  VectorConfig ->
  [Destination] ->
  SketchRHSSpec ->
  op
sketchMerge config dest rhs =
  inj $ SketchMerge config dest rhs

deriveFullExcept
  firstModeDeriveConfig
  [''Merge]
  [''UnifiedSymEq]
deriveNoSymEval [''SketchMerge]
makeFieldsNoPrefix ''Merge
makeFieldsNoPrefix ''SketchMerge

instance
  ( SemConstraint mode ctx,
    EvalModeConvertible opMode mode,
    MonadEvalMode opMode ctx
  ) =>
  OpSemantics MachineConfig (Merge opMode) (Value mode) ctx
  where
  applyOp vconst _ op inputs = do
    case toSym op of
      Merge vtype dest VectorRHS -> do
        applyMerge vconst vtype dest inputs
      Merge vtype dest ScalarRHS -> do
        newValues <- augmentElementWiseVXArgList vconst vtype 2 inputs
        applyMerge vconst vtype dest newValues
      Merge vtype dest (ImmRHS imm) -> do
        newValues <- augmentElementWiseVIArgList vconst vtype imm 2 inputs
        applyMerge vconst vtype dest newValues
      Merge {} ->
        mrgThrowError "Merge: rhs should be VectorRHS, ScalarRHS or ImmRHS"

instance
  (SemConstraint mode ctx) =>
  OpTyping (Merge mode) ctx
  where
  type OpTypeType (Merge mode) = ValueType
  typeOp (Merge vtype dest VectorRHS) =
    typeMergeVVM vtype dest
  typeOp (Merge vtype dest ScalarRHS) =
    typeMergeVXM vtype dest
  typeOp (Merge vtype dest (ImmRHS _)) =
    typeMergeVIM vtype dest
  typeOp (Merge {}) =
    mrgThrowError "Merge: rhs should be VectorRHS, ScalarRHS or ImmRHS"

instance
  (MonadAngelicContext ctx) =>
  OpTyping SketchMerge ctx
  where
  type OpTypeType SketchMerge = ValueType
  typeOp = typeSketchOp @(Merge 'S)

instance
  (MonadEvalMode mode Union) =>
  OpSymmetryReduction (Merge mode)
  where
  opUnreorderable = error "Should not be called"
  opCommutativeArgPos _ = mrgReturn []

instance GenSym SketchMerge (Merge 'S)

instance GenSymSimple SketchMerge (Merge 'S) where
  simpleFresh (SketchMerge vtype dest rhs) = do
    dest <- chooseFresh dest
    rhs <- simpleFresh rhs
    return $ Merge vtype dest rhs

instance GenSym (Merge 'C) (Merge 'S)

instance
  GenSymSimple
    (Merge 'C)
    (Merge 'S)
  where
  simpleFresh = return . toSym

instance OpPPrint (Merge 'C) where
  describeArguments _ = return []

instance PPrint (Merge 'C) where
  pformat (Merge vtype dest rhs) =
    "vmerge"
      <> case rhs of
        VectorRHS -> ".vvm"
        ScalarRHS -> ".vxm"
        ImmRHS _ -> ".vim"
        _ -> error "Merge: rhs should be VectorRHS, ScalarRHS or ImmRHS"
      <> case rhs of
        ImmRHS imm -> pformatArgList (vtype, dest, withName "rhs" imm)
        _ -> pformatArgList (vtype, dest)

instance OpPPrint SketchMerge where
  describeArguments _ = return []
  prefixResults _ = return []

instance PPrint SketchMerge where
  pformat (SketchMerge vtype dest rhs) =
    "vmerge"
      <> case rhs of
        SketchVectorRHS -> ".vvm"
        SketchScalarRHS -> ".vxm"
        SketchImmRHS _ -> ".vim"
      <> case rhs of
        SketchImmRHS imm -> pformatArgList (vtype, choices dest, withName "rhs" imm)
        _ -> pformatArgList (vtype, choices dest)

instance OpParser (Merge 'C) where
  opParser = do
    string "vmerge"
    rhs <-
      (string ".vvm" >> return VectorRHS)
        <|> (string ".vxm" >> return ScalarRHS)
        <|> (string ".vim" >> return (ImmRHS undefined))
    leftBracket
    vtype <- vectorConfigParser
    comma
    dest <- destinationParser
    rhs <- case rhs of
      ImmRHS _ -> do
        comma
        imm <- named "rhs" immParser
        return $ ImmRHS imm
      _ -> return rhs
    rightBracket
    return $ Merge vtype dest rhs

instance OpReachableSymbols (Merge 'C) where
  opReachableSymbols = mempty

instance SplitChoice SketchMerge where
  splitChoice (SketchMerge vtype dest rhs) = do
    dest <- dest
    rhs <- splitChoice rhs
    return $ SketchMerge vtype [dest] rhs

instance
  (SemConstraint mode ctx, Num cost, Mergeable cost) =>
  OpCost CostModel (Merge mode) cost ctx
  where
  opCost CostModel {..} _ (Merge {..}) = do
    mrgReturn $ fromIntegral $ mergeCost _vectorConfig

instance ExtractFeature (Merge 'C) FeatureSet where
  extractFeature (Merge config _ _) =
    vectorConfigFeature config
      <> mempty
        { opFeatures = HS.singleton Masking,
          policyFeatures = HS.singleton muPolicy
        }

instance
  ToFastSketch
    (Merge 'C)
    SketchMerge
  where
  toFastSketch (Merge vtype dest rhs) =
    SketchMerge vtype (toList dest) (toFastSketch rhs)

instance (EvalMode mode) => ScaleLMul (Merge mode) where
  scaleLMul ratio op = op & (vectorConfig %~ scaleLMul ratio)

instance ScaleLMul SketchMerge where
  scaleLMul ratio op = op & (vectorConfig %~ scaleLMul ratio)

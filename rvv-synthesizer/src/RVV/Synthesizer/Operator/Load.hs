{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.Operator.Load
  ( Load (..),
    SketchLoad (..),
    load,
    sketchLoad,
  )
where

import Control.Lens (makeFieldsNoPrefix, (%~), (&))
import Data.Foldable (Foldable (toList))
import GHC.Generics (Generic)
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
import HieraSynth.Util.Parser (bracketCommaSep4, named)
import Grisette.Unified
  ( EvalModeConvertible,
    EvalModeTag (C, S),
    GetData,
    UnifiedSymEq,
  )
import RVV.EvalMode (MonadEvalMode)
import RVV.Parser.ArgParser
  ( destinationParser,
    maskingParser,
    vectorConfigParser,
  )
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Memory (BlockId)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.VectorConfig (VectorConfig)
import RVV.Synthesizer.CostModel.CostModel (CostModel)
import RVV.Synthesizer.Feature.ExtractFeature (ExtractFeature (extractFeature))
import RVV.Synthesizer.Feature.FeatureSet (FeatureSet)
import RVV.Synthesizer.Lens
  ( HasBlockId (blockId),
    HasDestination (destination),
    HasMasking (masking),
    HasVectorConfig (vectorConfig),
  )
import RVV.Synthesizer.OpSemantics.UnitStrideLoadStore (applyVLE, typeVLE)
import RVV.Synthesizer.Operator.Common.PPrint
  ( PPrintArgList (pformatArgList),
    choices,
    withName,
  )
import RVV.Synthesizer.Operator.Common.TypeSketchOp (typeSketchOp)
import RVV.Synthesizer.Parameter.Destination (Destination)
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
import Text.Megaparsec.Char.Lexer (decimal)

data Load mode = Load
  { _vectorConfig :: VectorConfig,
    _blockId :: BlockId,
    _destination :: GetData mode Destination,
    _masking :: GetData mode Masking
  }
  deriving (Generic)

data SketchLoad = SketchLoad
  { _vectorConfig :: VectorConfig,
    _blockId :: BlockId,
    _destination :: [Destination],
    _masking :: [Masking]
  }
  deriving (Generic)

deriveFullExcept firstModeDeriveConfig [''Load] [''UnifiedSymEq]
deriveNoSymEval [''SketchLoad]
makeFieldsNoPrefix ''Load
makeFieldsNoPrefix ''SketchLoad

load ::
  (Load mode :<: op) =>
  VectorConfig ->
  BlockId ->
  GetData mode Destination ->
  GetData mode Masking ->
  op
load vtype blockId dest mask = inj $ Load vtype blockId dest mask

sketchLoad ::
  (SketchLoad :<: op) =>
  VectorConfig ->
  BlockId ->
  [Destination] ->
  [Masking] ->
  op
sketchLoad vtype blockId dest mask = inj $ SketchLoad vtype blockId dest mask

instance
  ( SemConstraint mode ctx,
    EvalModeConvertible opMode mode,
    MonadEvalMode opMode ctx
  ) =>
  OpSemantics MachineConfig (Load opMode) (Value mode) ctx
  where
  applyOp machine _ op inputs = do
    case toSym op of
      Load vtype blockId dest mask ->
        applyVLE dest mask machine vtype blockId inputs

instance (SemConstraint mode ctx) => OpTyping (Load mode) ctx where
  type OpTypeType (Load mode) = ValueType
  typeOp (Load vtype blockId dest mask) = typeVLE vtype blockId dest mask

instance (MonadAngelicContext ctx) => OpTyping SketchLoad ctx where
  type OpTypeType SketchLoad = ValueType
  typeOp = typeSketchOp @(Load 'S)

instance (MonadEvalMode mode Union) => OpSymmetryReduction (Load mode) where
  opUnreorderable = error "Should not be called"
  opCommutativeArgPos _ = mrgReturn []

instance GenSym SketchLoad (Load 'S)

instance GenSymSimple SketchLoad (Load 'S) where
  simpleFresh (SketchLoad vtype blockId dest mask) = do
    dest <- chooseFresh dest
    mask <- chooseFresh mask
    return $ Load vtype blockId dest mask

instance GenSym (Load 'C) (Load 'S)

instance GenSymSimple (Load 'C) (Load 'S) where
  simpleFresh = return . toSym

instance OpPPrint (Load 'C) where
  describeArguments _ = return []

instance PPrint (Load 'C) where
  pformat (Load vtype blockId dest mask) =
    "vle" <> pformatArgList (vtype, withName "block" blockId, dest, mask)

instance OpPPrint SketchLoad where
  describeArguments _ = return []
  prefixResults _ = return []

instance PPrint SketchLoad where
  pformat (SketchLoad vtype blockId dest mask) =
    "vle"
      <> pformatArgList
        ( vtype,
          withName "block" blockId,
          choices dest,
          choices mask
        )

instance OpParser (Load 'C) where
  opParser = do
    string "vle"
    (vtype, blockId, dest, mask) <-
      bracketCommaSep4
        vectorConfigParser
        (named "block" decimal)
        destinationParser
        maskingParser
    return $ Load vtype blockId dest mask

instance OpReachableSymbols (Load 'C) where
  opReachableSymbols = mempty

instance SplitChoice SketchLoad where
  splitChoice (SketchLoad vtype blockId dest mask) = do
    dest <- dest
    mask <- mask
    return $ SketchLoad vtype blockId [dest] [mask]

instance
  (SemConstraint mode ctx, Num cost, Mergeable cost) =>
  OpCost CostModel (Load mode) cost ctx
  where
  opCost _ _ _ = error "Unsupported"

instance ExtractFeature (Load 'C) FeatureSet where
  extractFeature _ = error "Unsupported"

instance ToFastSketch (Load 'C) SketchLoad where
  toFastSketch (Load vtype blockId dest mask) =
    SketchLoad vtype blockId (toList dest) (toList mask)

instance ScaleLMul (Load 'C) where
  scaleLMul ratio op = op & (vectorConfig %~ scaleLMul ratio)

instance ScaleLMul SketchLoad where
  scaleLMul ratio op = op & (vectorConfig %~ scaleLMul ratio)

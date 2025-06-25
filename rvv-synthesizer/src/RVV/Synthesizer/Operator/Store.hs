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

module RVV.Synthesizer.Operator.Store
  ( Store (..),
    SketchStore (..),
    store,
    sketchStore,
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
import HieraSynth.Util.Parser (bracketCommaSep3, named)
import Grisette.Unified
  ( EvalModeConvertible,
    EvalModeTag (C, S),
    GetData,
    UnifiedSymEq,
  )
import RVV.EvalMode (MonadEvalMode)
import RVV.Parser.ArgParser (maskingParser, vectorConfigParser)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Memory (BlockId)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.VectorConfig (VectorConfig)
import RVV.Synthesizer.CostModel.CostModel (CostModel)
import RVV.Synthesizer.Feature.ExtractFeature (ExtractFeature (extractFeature))
import RVV.Synthesizer.Feature.FeatureSet (FeatureSet)
import RVV.Synthesizer.Lens
  ( HasBlockId (blockId),
    HasMasking (masking),
    HasVectorConfig (vectorConfig),
  )
import RVV.Synthesizer.OpSemantics.UnitStrideLoadStore (applyVSE, typeVSE)
import RVV.Synthesizer.Operator.Common.PPrint
  ( PPrintArgList (pformatArgList),
    choices,
    withName,
  )
import RVV.Synthesizer.Operator.Common.TypeSketchOp (typeSketchOp)
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

data Store mode = Store
  { _vectorConfig :: VectorConfig,
    _blockId :: BlockId,
    _masking :: GetData mode Masking
  }
  deriving (Generic)

data SketchStore = SketchStore
  { _vectorConfig :: VectorConfig,
    _blockId :: BlockId,
    _masking :: [Masking]
  }
  deriving (Generic)

deriveFullExcept firstModeDeriveConfig [''Store] [''UnifiedSymEq]
deriveNoSymEval [''SketchStore]
makeFieldsNoPrefix ''Store
makeFieldsNoPrefix ''SketchStore

store ::
  (Store mode :<: op) =>
  VectorConfig ->
  BlockId ->
  GetData mode Masking ->
  op
store vtype blockId mask = inj $ Store vtype blockId mask

sketchStore :: (SketchStore :<: op) => VectorConfig -> BlockId -> [Masking] -> op
sketchStore vtype blockId mask = inj $ SketchStore vtype blockId mask

instance
  ( SemConstraint mode ctx,
    EvalModeConvertible opMode mode,
    MonadEvalMode opMode ctx
  ) =>
  OpSemantics MachineConfig (Store opMode) (Value mode) ctx
  where
  applyOp machine _ op inputs = do
    case toSym op of
      Store vtype blockId mask ->
        applyVSE mask machine vtype blockId inputs

instance (SemConstraint mode ctx) => OpTyping (Store mode) ctx where
  type OpTypeType (Store mode) = ValueType
  typeOp (Store vtype dest mask) = typeVSE vtype dest mask

instance (MonadAngelicContext ctx) => OpTyping SketchStore ctx where
  type OpTypeType SketchStore = ValueType
  typeOp = typeSketchOp @(Store 'S)

instance (MonadEvalMode mode Union) => OpSymmetryReduction (Store mode) where
  opUnreorderable = error "Should not be called"
  opCommutativeArgPos _ = mrgReturn []

instance GenSym SketchStore (Store 'S)

instance GenSymSimple SketchStore (Store 'S) where
  simpleFresh (SketchStore vtype blockId mask) = do
    mask <- chooseFresh mask
    return $ Store vtype blockId mask

instance GenSym (Store 'C) (Store 'S)

instance GenSymSimple (Store 'C) (Store 'S) where
  simpleFresh = return . toSym

instance OpPPrint (Store 'C) where
  describeArguments _ = return []

instance PPrint (Store 'C) where
  pformat (Store vtype blockId mask) =
    "vse" <> pformatArgList (vtype, withName "block" blockId, mask)

instance OpPPrint SketchStore where
  describeArguments _ = return []
  prefixResults _ = return []

instance PPrint SketchStore where
  pformat (SketchStore vtype blockId mask) =
    "vse" <> pformatArgList (vtype, withName "block" blockId, choices mask)

instance OpParser (Store 'C) where
  opParser = do
    string "vse"
    (vtype, blockId, mask) <-
      bracketCommaSep3
        vectorConfigParser
        (named "block" decimal)
        maskingParser
    return $ Store vtype blockId mask

instance OpReachableSymbols (Store 'C) where
  opReachableSymbols = mempty

instance SplitChoice SketchStore where
  splitChoice (SketchStore vtype blockId mask) = do
    mask <- mask
    return $ SketchStore vtype blockId [mask]

instance
  (SemConstraint mode ctx, Num cost, Mergeable cost) =>
  OpCost CostModel (Store mode) cost ctx
  where
  opCost _ _ _ = error "Unsupported"

instance ExtractFeature (Store 'C) FeatureSet where
  extractFeature _ = error "Unsupported"

instance ToFastSketch (Store 'C) SketchStore where
  toFastSketch (Store vtype blockId mask) =
    SketchStore vtype blockId (toList mask)

instance ScaleLMul (Store 'C) where
  scaleLMul ratio op = op & (vectorConfig %~ scaleLMul ratio)

instance ScaleLMul SketchStore where
  scaleLMul ratio op = op & (vectorConfig %~ scaleLMul ratio)

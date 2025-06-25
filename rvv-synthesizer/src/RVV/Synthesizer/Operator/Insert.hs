{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}

module RVV.Synthesizer.Operator.Insert
  ( Insert (..),
    SketchInsert (..),
    VectorExtend (..),
    SketchVectorExtend (..),
    insertVector,
    sketchInsertVector,
    insertMask,
    sketchInsertMask,
    vectorExtend,
    sketchVectorExtend,
  )
where

import Control.Applicative ((<|>))
import Control.Lens (makeFieldsNoPrefix)
import Grisette
  ( GenSym (fresh),
    GenSymSimple (simpleFresh),
    Mergeable,
    PPrint (pformat),
    ToSym (toSym),
    chooseFresh,
    mrgReturn, mrgFmap,
  )
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
import HieraSynth.Util.Parser (bracketCommaSep2, bracketCommaSep4, named)
import Grisette.Unified (EvalModeConvertible, EvalModeTag (C, S), GetData, UnifiedSymEq)
import RVV.EvalMode (MonadEvalMode)
import RVV.Parser.ArgParser (destinationParser, maskMulParser, vectorConfigParser)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Multiplier (MaskMul)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.VectorConfig (VectorConfig (lengthMul))
import RVV.Synthesizer.CostModel.CostModel (CostModel (broadcastCost, maskOpCost, CostModel))
import RVV.Synthesizer.Feature.ExtractFeature (ExtractFeature (extractFeature))
import RVV.Synthesizer.Feature.FeatureSet
  ( FeatureSet,
    maskMulFeature,
    vectorConfigFeature,
  )
import RVV.Synthesizer.Lens
  ( HasDestination (destination),
    HasIndex (index),
    HasMaskMulDest (maskMulDest),
    HasMaskMulPart (maskMulPart),
    HasVectorConfigDest (vectorConfigDest),
    HasVectorConfigPart (vectorConfigPart),
  )
import RVV.Synthesizer.OpSemantics.VSet
  ( applyInsertMask,
    applyLMulExtend,
    applyVSet,
    typeInsertMask,
    typeLMulExtend,
    typeVSet,
  )
import RVV.Synthesizer.Operator.Common.PPrint
  ( PPrintArgList (pformatArgList),
    choices,
    withName,
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
    noModeDeriveConfig,
  )
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Foldable (Foldable(toList))

data Insert mode
  = InsertVector -- corresponds to VSet
      { _vectorConfigPart :: VectorConfig,
        _vectorConfigDest :: VectorConfig,
        _destination :: GetData mode Destination,
        _index :: Int
      }
  | InsertMask -- corresponds to InsertMask
      { _maskMulPart :: MaskMul,
        _maskMulDest :: MaskMul,
        _destination :: GetData mode Destination,
        _index :: Int
      }

data VectorExtend
  = VectorExtend -- corresponds to VLMulExtend
  { _vectorConfigPart :: VectorConfig,
    _vectorConfigDest :: VectorConfig
  }

data SketchInsert
  = SketchInsertVector
      { _vectorConfigPart :: VectorConfig,
        _vectorConfigDest :: VectorConfig,
        _destination :: [Destination],
        _index :: [Int]
      }
  | SketchInsertMask
      { _maskMulPart :: MaskMul,
        _maskMulDest :: MaskMul,
        _destination :: [Destination],
        _index :: [Int]
      }

data SketchVectorExtend
  = SketchVectorExtend
  { _vectorConfigPart :: VectorConfig,
    _vectorConfigDest :: VectorConfig
  }

deriveFullExcept
  firstModeDeriveConfig
  [''Insert]
  [''UnifiedSymEq]
deriveFullExcept
  noModeDeriveConfig
  [''VectorExtend]
  [''UnifiedSymEq]
deriveNoSymEval [''SketchInsert, ''SketchVectorExtend]
makeFieldsNoPrefix ''Insert
makeFieldsNoPrefix ''SketchInsert
makeFieldsNoPrefix ''VectorExtend
makeFieldsNoPrefix ''SketchVectorExtend

insertVector ::
  forall mode op.
  (Insert mode :<: op) =>
  VectorConfig ->
  VectorConfig ->
  GetData mode Destination ->
  Int ->
  op
insertVector part dest dst idx = inj $ InsertVector part dest dst idx

sketchInsertVector ::
  (SketchInsert :<: op) =>
  VectorConfig ->
  VectorConfig ->
  [Destination] ->
  [Int] ->
  op
sketchInsertVector part dest dst idx =
  inj $ SketchInsertVector part dest dst idx

insertMask ::
  forall mode op.
  (Insert mode :<: op) =>
  MaskMul ->
  MaskMul ->
  GetData mode Destination ->
  Int ->
  op
insertMask part dest dst idx = inj $ InsertMask part dest dst idx

sketchInsertMask ::
  (SketchInsert :<: op) =>
  MaskMul ->
  MaskMul ->
  [Destination] ->
  [Int] ->
  op
sketchInsertMask part dest dst idx = inj $ SketchInsertMask part dest dst idx

vectorExtend ::
  (VectorExtend :<: op) =>
  VectorConfig ->
  VectorConfig ->
  op
vectorExtend part dest = inj $ VectorExtend part dest

sketchVectorExtend ::
  (SketchVectorExtend :<: op) =>
  VectorConfig ->
  VectorConfig ->
  op
sketchVectorExtend part dest = inj $ SketchVectorExtend part dest

instance
  ( SemConstraint mode ctx,
    EvalModeConvertible opMode mode,
    MonadEvalMode opMode ctx
  ) =>
  OpSemantics MachineConfig (Insert opMode) (Value mode) ctx
  where
  applyOp machine _ op inputs = case toSym op :: Insert mode of
    InsertVector part dest dst idx -> applyVSet machine part dest dst idx inputs
    InsertMask part dest dst idx -> applyInsertMask machine part dest dst idx inputs

instance
  ( SemConstraint mode ctx,
    MonadEvalMode mode ctx
  ) =>
  OpSemantics MachineConfig VectorExtend (Value mode) ctx
  where
  applyOp machine _ (VectorExtend part dest) = applyLMulExtend machine part dest

instance (SemConstraint mode ctx) => OpTyping (Insert mode) ctx where
  type OpTypeType (Insert mode) = ValueType
  typeOp op = case op of
    InsertVector part dest dst _ -> typeVSet part dest dst
    InsertMask part dest dst _ -> typeInsertMask part dest dst

instance (MonadContext ctx) => OpTyping VectorExtend ctx where
  type OpTypeType VectorExtend = ValueType
  typeOp (VectorExtend part dest) = typeLMulExtend part dest

instance (MonadAngelicContext ctx) => OpTyping SketchInsert ctx where
  type OpTypeType SketchInsert = ValueType
  typeOp = typeSketchOp @(Insert 'S)

instance (MonadAngelicContext ctx) => OpTyping SketchVectorExtend ctx where
  type OpTypeType SketchVectorExtend = ValueType
  typeOp = typeSketchOp @VectorExtend

instance OpSymmetryReduction (Insert mode) where
  opUnreorderable = error "Should not be called"
  opCommutativeArgPos _ = mrgReturn []

instance OpSymmetryReduction VectorExtend where
  opUnreorderable = error "Should not be called"
  opCommutativeArgPos _ = mrgReturn []

instance GenSym SketchInsert (Insert 'S) where
  fresh (SketchInsertVector part dest dst idx) = do
    dst <- chooseFresh dst
    idx <- chooseFresh idx
    return $ mrgFmap (InsertVector part dest dst) idx
  fresh (SketchInsertMask part dest dst idx) = do
    dst <- chooseFresh dst
    idx <- chooseFresh idx
    return $ mrgFmap (InsertMask part dest dst) idx

instance GenSym SketchVectorExtend VectorExtend where
  fresh (SketchVectorExtend part dest) = return $ mrgReturn $ VectorExtend part dest

instance GenSym (Insert 'C) (Insert 'S)

instance GenSymSimple (Insert 'C) (Insert 'S) where
  simpleFresh = return . toSym

instance GenSym VectorExtend VectorExtend

instance GenSymSimple VectorExtend VectorExtend where
  simpleFresh = return . toSym

instance OpPPrint (Insert 'C) where
  describeArguments _ = return []
  prefixResults _ = return []

instance OpPPrint VectorExtend where
  describeArguments _ = return []
  prefixResults _ = return []

instance PPrint (Insert 'C) where
  pformat op = case op of
    InsertVector part dest dst idx ->
      "vset" <> pformatArgList (withName "part" part, withName "dest" dest, dst, withName "idx" idx)
    InsertMask part dest dst idx ->
      "insert_mask" <> pformatArgList (withName "part" part, withName "dest" dest, dst, withName "idx" idx)

instance PPrint VectorExtend where
  pformat (VectorExtend part dest) =
    "vlmul_extend" <> pformatArgList (withName "part" part, withName "dest" dest)

instance OpPPrint SketchInsert where
  describeArguments _ = return []
  prefixResults _ = return []

instance OpPPrint SketchVectorExtend where
  describeArguments _ = return []
  prefixResults _ = return []

instance PPrint SketchInsert where
  pformat op = case op of
    SketchInsertVector part dest dst idx ->
      "vset" <> pformatArgList (withName "part" part, withName "dest" dest, choices dst, choices idx)
    SketchInsertMask part dest dst idx ->
      "insert_mask" <> pformatArgList (withName "part" part, withName "dest" dest, choices dst, choices idx)

instance PPrint SketchVectorExtend where
  pformat (SketchVectorExtend part dest) =
    "vlmul_extend" <> pformatArgList (withName "part" part, withName "dest" dest)

instance OpParser (Insert 'C) where
  opParser = insertVectorParser <|> insertMaskParser
    where
      insertVectorParser = do
        string "vset"
        (part, dest, dst, idx) <-
          bracketCommaSep4
            (named "part" vectorConfigParser)
            (named "dest" vectorConfigParser)
            destinationParser
            (named "idx" decimal)
        return $ InsertVector part dest dst idx
      insertMaskParser = do
        string "insert_mask"
        (part, dest, dst, idx) <-
          bracketCommaSep4
            (named "part" maskMulParser)
            (named "dest" maskMulParser)
            destinationParser
            (named "idx" decimal)
        return $ InsertMask part dest dst idx

instance OpParser VectorExtend where
  opParser = do
    string "vlmul_extend"
    (part, dest) <-
      bracketCommaSep2
        (named "part" vectorConfigParser)
        (named "dest" vectorConfigParser)
    return $ VectorExtend part dest

instance OpReachableSymbols (Insert 'C) where
  opReachableSymbols = mempty

instance OpReachableSymbols VectorExtend where
  opReachableSymbols = mempty

instance SplitChoice SketchInsert where
  splitChoice (SketchInsertVector part dest dst idx) = do
    dst <- dst
    idx <- idx
    return $ SketchInsertVector part dest [dst] [idx]
  splitChoice (SketchInsertMask part dest dst idx) = do
    dst <- dst
    idx <- idx
    return $ SketchInsertMask part dest [dst] [idx]

instance SplitChoice SketchVectorExtend where
  splitChoice = return

instance
  (MonadContext ctx, Num cost, Mergeable cost) =>
  OpCost CostModel (Insert mode) cost ctx
  where
  opCost CostModel {..} _ (InsertVector part dest _ index) =
    if lengthMul part >= 1 || index == 0
      then mrgReturn $ fromIntegral $ broadcastCost part
      else mrgReturn $ fromIntegral $ broadcastCost dest
  opCost CostModel {..}  _ (InsertMask part _ _ _) =
    mrgReturn $ fromIntegral $ maskOpCost part

instance
  (MonadContext ctx, Num cost, Mergeable cost) =>
  OpCost CostModel VectorExtend cost ctx
  where
  opCost CostModel {..} _ (VectorExtend part _) =
    mrgReturn $ fromIntegral $ broadcastCost part

instance ExtractFeature (Insert 'C) FeatureSet where
  extractFeature op = case op of
    InsertVector part dest _ _ -> vectorConfigFeature part <> vectorConfigFeature dest
    InsertMask part dest _ _ -> maskMulFeature part <> maskMulFeature dest

instance ExtractFeature VectorExtend FeatureSet where
  extractFeature (VectorExtend part dest) = vectorConfigFeature part <> vectorConfigFeature dest

instance ToFastSketch (Insert 'C) SketchInsert where
  toFastSketch op = case op of
    InsertVector part dest dst idx -> SketchInsertVector part dest (toList dst) [idx]
    InsertMask part dest dst idx -> SketchInsertMask part dest (toList dst) [idx]

instance ToFastSketch VectorExtend SketchVectorExtend where
  toFastSketch (VectorExtend part dest) = SketchVectorExtend part dest

instance ScaleLMul (Insert mode) where
  scaleLMul ratio op = case op of
    InsertVector part dest dst idx -> InsertVector (scaleLMul ratio part) (scaleLMul ratio dest) dst idx
    InsertMask part dest dst idx -> InsertMask (scaleLMul ratio part) (scaleLMul ratio dest) dst idx

instance ScaleLMul VectorExtend where
  scaleLMul ratio (VectorExtend part dest) = VectorExtend (scaleLMul ratio part) (scaleLMul ratio dest)

instance ScaleLMul SketchInsert where
  scaleLMul ratio op = case op of
    SketchInsertVector part dest dst idx -> SketchInsertVector (scaleLMul ratio part) (scaleLMul ratio dest) dst idx
    SketchInsertMask part dest dst idx -> SketchInsertMask (scaleLMul ratio part) (scaleLMul ratio dest) dst idx

instance ScaleLMul SketchVectorExtend where
  scaleLMul ratio (SketchVectorExtend part dest) = SketchVectorExtend (scaleLMul ratio part) (scaleLMul ratio dest)

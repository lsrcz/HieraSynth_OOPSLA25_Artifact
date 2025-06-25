{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.Operator.Move
  ( Move (..),
    MoveFirstElementToScalar (..),
    SketchMove (..),
    SketchMoveFirstElementToScalar (..),
    moveToVector,
    sketchMoveToVector,
    moveFirstElementToScalar,
    sketchMoveFirstElementToScalar,
    moveScalarToFirstElement,
    sketchMoveScalarToFirstElement,
    moveToMask,
    sketchMoveToMask,
  )
where

import Control.Applicative ((<|>))
import Control.Lens (makeFieldsNoPrefix, (%~), (&))
import Data.Foldable (Foldable (toList))
import qualified Data.HashSet as HS
import Grisette
  ( GenSym (fresh),
    GenSymSimple (simpleFresh),
    Mergeable,
    PPrint (pformat),
    ToSym (toSym),
    chooseFresh,
    mrgReturn,
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
import HieraSynth.Util.Parser (betweenBrackets, bracketCommaSep2, bracketCommaSep3, named)
import Grisette.Unified (EvalModeConvertible, EvalModeTag (C, S), GetData, UnifiedSymEq)
import RVV.EvalMode (MonadEvalMode)
import RVV.Parser.ArgParser (destinationParser, maskMulParser, vectorConfigParser, widthMulParser)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Multiplier (MaskMul, WidthMul)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.VectorConfig (VectorConfig, getDelegatedVectorConfig)
import RVV.Synthesizer.CostModel.CostModel
  ( CostModel,
    broadcastCost,
    modelMachineConfig,
  )
import RVV.Synthesizer.Feature.ExtractFeature (ExtractFeature (extractFeature))
import RVV.Synthesizer.Feature.FeatureSet
  ( Feature (Broadcast),
    FeatureSet (opFeatures),
    maskMulFeature,
    vectorConfigFeature,
  )
import RVV.Synthesizer.Lens
  ( HasDestination (destination),
    HasMaskMul (maskMul),
    HasRhs (rhs),
    HasVectorConfig (vectorConfig),
    HasWidthMulDelegated (widthMulDelegated),
  )
import RVV.Synthesizer.OpSemantics.Move
  ( applyVMVMI,
    applyVMVMX,
    applyVMVSX,
    applyVMVVI,
    applyVMVVV,
    applyVMVVX,
    applyVMVXS,
    typeVMVMI,
    typeVMVMX,
    typeVMVVI,
    typeVMVVV,
    typeVMVVXSX,
    typeVMVXS,
  )
import RVV.Synthesizer.Operator.Common.PPrint
  ( PPrintArgList (pformatArgList),
    withName,
  )
import RVV.Synthesizer.Operator.Common.Parser (immParser)
import RVV.Synthesizer.Operator.Common.RHSSpec
  ( RHSSpec (ImmRHS, ScalarRHS, VectorRHS),
    SketchRHSSpec (SketchImmRHS, SketchScalarRHS, SketchVectorRHS),
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

-- | Operators for moving data between registers that require a mode
data Move mode
  = MoveToVector
      { _vectorConfig :: VectorConfig,
        _destination :: GetData mode Destination,
        _rhs :: RHSSpec mode
      }
  | MoveToMask
      { _widthMulDelegated :: WidthMul,
        _maskMul :: MaskMul,
        _rhs :: RHSSpec mode
      }
  | MoveScalarToFirstElement
      { _vectorConfig :: VectorConfig,
        _destination :: GetData mode Destination
      }

-- | Operators for moving data from first element to scalar
newtype MoveFirstElementToScalar
  = MoveFirstElementToScalar
  { _vectorConfig :: VectorConfig
  }

-- | Sketch version of Move operators
data SketchMove
  = SketchMoveToVector
      { _vectorConfig :: VectorConfig,
        _destination :: [Destination],
        _rhs :: SketchRHSSpec
      }
  | SketchMoveToMask
      { _widthMulDelegated :: WidthMul,
        _maskMul :: MaskMul,
        _rhs :: SketchRHSSpec
      }
  | SketchMoveScalarToFirstElement
      { _vectorConfig :: VectorConfig,
        _destination :: [Destination]
      }

-- | Sketch version of MoveFirstElementToScalar
newtype SketchMoveFirstElementToScalar
  = SketchMoveFirstElementToScalar {_vectorConfig :: VectorConfig}

deriveFullExcept
  noModeDeriveConfig
  [''MoveFirstElementToScalar]
  [''UnifiedSymEq]
deriveFullExcept
  firstModeDeriveConfig
  [''Move]
  [''UnifiedSymEq]
deriveNoSymEval [''SketchMove, ''SketchMoveFirstElementToScalar]
makeFieldsNoPrefix ''Move
makeFieldsNoPrefix ''MoveFirstElementToScalar
makeFieldsNoPrefix ''SketchMove
makeFieldsNoPrefix ''SketchMoveFirstElementToScalar

-- Smart constructors
moveToVector ::
  (Move mode :<: op) =>
  VectorConfig ->
  GetData mode Destination ->
  RHSSpec mode ->
  op
moveToVector vtype dest rhs = inj $ MoveToVector vtype dest rhs

sketchMoveToVector ::
  (SketchMove :<: op) =>
  VectorConfig ->
  [Destination] ->
  SketchRHSSpec ->
  op
sketchMoveToVector vtype dest rhs = inj $ SketchMoveToVector vtype dest rhs

moveToMask ::
  (Move mode :<: op) =>
  WidthMul ->
  MaskMul ->
  RHSSpec mode ->
  op
moveToMask xmul mmul rhs = inj $ MoveToMask xmul mmul rhs

sketchMoveToMask ::
  (SketchMove :<: op) =>
  WidthMul ->
  MaskMul ->
  SketchRHSSpec ->
  op
sketchMoveToMask xmul mmul rhs = inj $ SketchMoveToMask xmul mmul rhs

moveScalarToFirstElement ::
  (Move mode :<: op) =>
  VectorConfig ->
  GetData mode Destination ->
  op
moveScalarToFirstElement vtype dest = inj $ MoveScalarToFirstElement vtype dest

sketchMoveScalarToFirstElement ::
  (SketchMove :<: op) =>
  VectorConfig ->
  [Destination] ->
  op
sketchMoveScalarToFirstElement vtype dest = inj $ SketchMoveScalarToFirstElement vtype dest

moveFirstElementToScalar ::
  (MoveFirstElementToScalar :<: op) =>
  VectorConfig ->
  op
moveFirstElementToScalar vtype = inj $ MoveFirstElementToScalar vtype

sketchMoveFirstElementToScalar ::
  (SketchMoveFirstElementToScalar :<: op) =>
  VectorConfig ->
  op
sketchMoveFirstElementToScalar vtype = inj $ SketchMoveFirstElementToScalar vtype

-- Type class instances
instance
  ( SemConstraint mode ctx,
    EvalModeConvertible opMode mode,
    MonadEvalMode opMode ctx
  ) =>
  OpSemantics MachineConfig (Move opMode) (Value mode) ctx
  where
  applyOp machine _ op inputs = case toSym op of
    MoveToVector vtype dest rhsSpec -> case rhsSpec of
      VectorRHS -> applyVMVVV machine vtype dest inputs
      ScalarRHS -> applyVMVVX machine vtype dest inputs
      ImmRHS i -> applyVMVVI machine vtype dest i inputs
      _ -> error "Unsupported RHS spec for MoveToVector"
    MoveToMask xmul mmul rhsSpec -> case rhsSpec of
      ScalarRHS -> applyVMVMX machine xmul mmul inputs
      ImmRHS i -> applyVMVMI machine xmul mmul i inputs
      _ -> error "Unsupported RHS spec for MoveToMask"
    MoveScalarToFirstElement vtype dest -> applyVMVSX machine vtype dest inputs

instance
  (SemConstraint mode ctx, MonadEvalMode mode ctx) =>
  OpSemantics MachineConfig MoveFirstElementToScalar (Value mode) ctx
  where
  applyOp machine _ op inputs = case op of
    MoveFirstElementToScalar vtype -> applyVMVXS machine vtype inputs

instance (SemConstraint mode ctx) => OpTyping (Move mode) ctx where
  type OpTypeType (Move mode) = ValueType
  typeOp op = case op of
    MoveToVector vtype dest rhsSpec -> case rhsSpec of
      VectorRHS -> typeVMVVV vtype dest
      ScalarRHS -> typeVMVVXSX vtype dest
      ImmRHS _ -> typeVMVVI vtype dest
      _ -> error "Unsupported RHS spec for MoveToVector"
    MoveToMask xmul mmul rhsSpec -> case rhsSpec of
      ScalarRHS -> typeVMVMX xmul mmul
      ImmRHS _ -> typeVMVMI mmul
      _ -> error "Unsupported RHS spec for MoveToMask"
    MoveScalarToFirstElement vtype dest -> typeVMVVXSX vtype dest

instance (MonadContext ctx) => OpTyping MoveFirstElementToScalar ctx where
  type OpTypeType MoveFirstElementToScalar = ValueType
  typeOp op = case op of
    MoveFirstElementToScalar vtype -> typeVMVXS vtype

instance (MonadAngelicContext ctx) => OpTyping SketchMove ctx where
  type OpTypeType SketchMove = ValueType
  typeOp = typeSketchOp @(Move 'S)

instance (MonadAngelicContext ctx) => OpTyping SketchMoveFirstElementToScalar ctx where
  type OpTypeType SketchMoveFirstElementToScalar = ValueType
  typeOp = typeSketchOp @MoveFirstElementToScalar

instance OpSymmetryReduction (Move mode) where
  opUnreorderable = error "Should not be called"
  opCommutativeArgPos _ = mrgReturn []

instance OpSymmetryReduction MoveFirstElementToScalar where
  opUnreorderable = error "Should not be called"
  opCommutativeArgPos _ = mrgReturn []

instance GenSym SketchMove (Move 'S) where
  fresh (SketchMoveToVector vtype dest rhsSpec) = do
    dest <- chooseFresh dest
    rhsSpec <- simpleFresh rhsSpec
    return $ mrgReturn $ MoveToVector vtype dest rhsSpec
  fresh (SketchMoveToMask xmul mmul rhsSpec) = do
    rhsSpec <- simpleFresh rhsSpec
    return $ mrgReturn $ MoveToMask xmul mmul rhsSpec
  fresh (SketchMoveScalarToFirstElement vtype dest) = do
    dest <- chooseFresh dest
    return $ mrgReturn $ MoveScalarToFirstElement vtype dest

instance GenSym SketchMoveFirstElementToScalar MoveFirstElementToScalar where
  fresh (SketchMoveFirstElementToScalar vtype) =
    return $ mrgReturn $ MoveFirstElementToScalar vtype

instance GenSym (Move 'C) (Move 'S)

instance GenSym MoveFirstElementToScalar MoveFirstElementToScalar

instance GenSymSimple (Move 'C) (Move 'S) where
  simpleFresh = return . toSym

instance GenSymSimple MoveFirstElementToScalar MoveFirstElementToScalar where
  simpleFresh = return . toSym

instance OpPPrint (Move 'C) where
  describeArguments _ = return []

instance OpPPrint MoveFirstElementToScalar where
  describeArguments _ = return []

instance PPrint (Move 'C) where
  pformat op = case op of
    MoveToVector vtype dest rhsSpec ->
      case rhsSpec of
        VectorRHS {} -> "vec_to_vec" <> pformatArgList (vtype, dest)
        ScalarRHS {} -> "scalar_to_vec" <> pformatArgList (vtype, dest)
        ImmRHS imm -> "imm_to_vec" <> pformatArgList (vtype, dest, withName "imm" imm)
        _ -> error "Unsupported RHS spec for MoveToVector"
    MoveToMask xmul mmul rhsSpec ->
      case rhsSpec of
        ScalarRHS {} -> "scalar_to_mask" <> pformatArgList (withName "xmul" xmul, withName "mmul" mmul)
        ImmRHS imm -> "imm_to_mask" <> pformatArgList (withName "xmul" xmul, withName "mmul" mmul, withName "imm" imm)
        _ -> error "Unsupported RHS spec for MoveToMask"
    MoveScalarToFirstElement vtype dest ->
      "scalar_to_first_element" <> pformatArgList (vtype, dest)

instance PPrint MoveFirstElementToScalar where
  pformat op = case op of
    MoveFirstElementToScalar vtype ->
      "first_element_to_scalar" <> pformatArgList vtype

instance OpPPrint SketchMove where
  describeArguments _ = return []
  prefixResults _ = return []

instance OpPPrint SketchMoveFirstElementToScalar where
  describeArguments _ = return []
  prefixResults _ = return []

instance PPrint SketchMove where
  pformat op = case op of
    SketchMoveToVector vtype dest rhsSpec ->
      case rhsSpec of
        SketchVectorRHS -> "vec_to_vec" <> pformatArgList (vtype, dest)
        SketchScalarRHS -> "scalar_to_vec" <> pformatArgList (vtype, dest)
        SketchImmRHS imm -> "imm_to_vec" <> pformatArgList (vtype, dest, withName "imm" imm)
    SketchMoveToMask xmul mmul rhsSpec ->
      case rhsSpec of
        SketchScalarRHS -> "scalar_to_mask" <> pformatArgList (withName "xmul" xmul, withName "mmul" mmul)
        SketchImmRHS imm -> "imm_to_mask" <> pformatArgList (withName "xmul" xmul, withName "mmul" mmul, withName "imm" imm)
        _ -> error "Unsupported RHS spec for SketchMoveToMask"
    SketchMoveScalarToFirstElement vtype dest ->
      "scalar_to_first_element" <> pformatArgList (vtype, dest)

instance PPrint SketchMoveFirstElementToScalar where
  pformat op = case op of
    SketchMoveFirstElementToScalar vtype ->
      "first_element_to_scalar" <> pformatArgList vtype

instance OpParser (Move 'C) where
  opParser =
    moveVectorToVectorParser
      <|> moveScalarToVectorParser
      <|> moveImmToVectorParser
      <|> moveScalarToMaskParser
      <|> moveImmToMaskParser
      <|> moveScalarToFirstElementParser
    where
      moveVectorToVectorParser = do
        string "vec_to_vec"
        (vtype, dest) <-
          bracketCommaSep2
            vectorConfigParser
            destinationParser
        return $ MoveToVector vtype dest VectorRHS

      moveScalarToVectorParser = do
        string "scalar_to_vec"
        (vtype, dest) <-
          bracketCommaSep2
            vectorConfigParser
            destinationParser
        return $ MoveToVector vtype dest ScalarRHS

      moveImmToVectorParser = do
        string "imm_to_vec"
        (vtype, dest, imm) <-
          bracketCommaSep3
            vectorConfigParser
            destinationParser
            (named "imm" immParser)
        return $ MoveToVector vtype dest (ImmRHS imm)

      moveScalarToMaskParser = do
        string "scalar_to_mask"
        (xmul, mmul) <-
          bracketCommaSep2
            (named "xmul" widthMulParser)
            (named "mmul" maskMulParser)
        return $ MoveToMask xmul mmul ScalarRHS

      moveImmToMaskParser = do
        string "imm_to_mask"
        (xmul, mmul, imm) <-
          bracketCommaSep3
            (named "xmul" widthMulParser)
            (named "mmul" maskMulParser)
            (named "imm" immParser)
        return $ MoveToMask xmul mmul (ImmRHS imm)

      moveScalarToFirstElementParser = do
        string "scalar_to_first_element"
        (vtype, dest) <-
          bracketCommaSep2
            vectorConfigParser
            destinationParser
        return $ MoveScalarToFirstElement vtype dest

instance OpParser MoveFirstElementToScalar where
  opParser = moveFirstElementToScalarParser
    where
      moveFirstElementToScalarParser = do
        string "first_element_to_scalar"
        vtype <- betweenBrackets vectorConfigParser
        return $ MoveFirstElementToScalar vtype

instance OpReachableSymbols (Move mode) where
  opReachableSymbols = mempty

instance OpReachableSymbols MoveFirstElementToScalar where
  opReachableSymbols = mempty

instance SplitChoice SketchMove where
  splitChoice (SketchMoveToVector vtype dest rhs) = do
    dest <- dest
    rhs <- splitChoice rhs
    return $ SketchMoveToVector vtype [dest] rhs
  splitChoice (SketchMoveToMask xmul mmul rhs) = do
    rhs <- splitChoice rhs
    return $ SketchMoveToMask xmul mmul rhs
  splitChoice (SketchMoveScalarToFirstElement vtype dest) = do
    dest <- dest
    return $ SketchMoveScalarToFirstElement vtype [dest]

instance SplitChoice SketchMoveFirstElementToScalar where
  splitChoice = return

instance
  (MonadContext ctx, Num cost, Mergeable cost) =>
  OpCost CostModel (Move mode) cost ctx
  where
  opCost costModel _ op = case op of
    MoveToVector vtype _ _ -> mrgReturn $ fromIntegral $ broadcastCost costModel vtype
    MoveToMask xmul mmul _ -> do
      vtype <- getDelegatedVectorConfig (modelMachineConfig costModel) xmul mmul
      mrgReturn $ fromIntegral $ broadcastCost costModel vtype
    MoveScalarToFirstElement vtype _ -> mrgReturn $ fromIntegral $ broadcastCost costModel vtype

instance
  (MonadContext ctx, Num cost, Mergeable cost) =>
  OpCost CostModel MoveFirstElementToScalar cost ctx
  where
  opCost costModel _ op = case op of
    MoveFirstElementToScalar vtype -> mrgReturn $ fromIntegral $ broadcastCost costModel vtype

instance ExtractFeature (Move mode) FeatureSet where
  extractFeature op = case op of
    MoveToVector vtype _ _ ->
      vectorConfigFeature vtype
        <> mempty {opFeatures = HS.singleton Broadcast}
    MoveToMask _ mmul _ ->
      maskMulFeature mmul
        <> mempty {opFeatures = HS.singleton Broadcast}
    MoveScalarToFirstElement vtype _ ->
      vectorConfigFeature vtype
        <> mempty {opFeatures = HS.singleton Broadcast}

instance ExtractFeature MoveFirstElementToScalar FeatureSet where
  extractFeature op = case op of
    MoveFirstElementToScalar vtype ->
      vectorConfigFeature vtype
        <> mempty {opFeatures = HS.singleton Broadcast}

instance ToFastSketch (Move 'C) SketchMove where
  toFastSketch op = case op of
    MoveToVector vtype dest rhs -> SketchMoveToVector vtype (toList dest) (toFastSketch rhs)
    MoveToMask xmul mmul rhs -> SketchMoveToMask xmul mmul (toFastSketch rhs)
    MoveScalarToFirstElement vtype dest -> SketchMoveScalarToFirstElement vtype (toList dest)

instance ToFastSketch MoveFirstElementToScalar SketchMoveFirstElementToScalar where
  toFastSketch op = case op of
    MoveFirstElementToScalar vtype -> SketchMoveFirstElementToScalar vtype

instance ScaleLMul (Move mode) where
  scaleLMul ratio op = op & maskMul %~ scaleLMul ratio & vectorConfig %~ scaleLMul ratio

instance ScaleLMul MoveFirstElementToScalar where
  scaleLMul ratio op = op & vectorConfig %~ scaleLMul ratio

instance ScaleLMul SketchMove where
  scaleLMul ratio op = op & maskMul %~ scaleLMul ratio & vectorConfig %~ scaleLMul ratio

instance ScaleLMul SketchMoveFirstElementToScalar where
  scaleLMul ratio op = op & vectorConfig %~ scaleLMul ratio

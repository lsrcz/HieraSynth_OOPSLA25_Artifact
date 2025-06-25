{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.Operator.DelegatedVectorBinaryOnMask
  ( DelegatedVectorBinaryOnMask (..),
    SketchDelegatedVectorBinaryOnMask (..),
    delegatedVectorBinaryOnMask,
    sketchDelegatedVectorBinaryOnMask,
  )
where

import Control.Lens (Identity (Identity), makeFieldsNoPrefix, (%~), (&))
import Control.Monad.Error.Class (MonadError (throwError))
import Data.Foldable (toList)
import qualified Data.HashSet as HS
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
import Grisette.Internal.Unified.UnifiedData (extractData)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
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
import HieraSynth.Util.Parser
  ( bracketCommaSep3,
    named,
  )
import HieraSynth.Util.Show (showAsText)
import Grisette.Unified
  ( EvalModeConvertible,
    EvalModeTag (C, S),
    GetData,
    UnifiedSymEq,
  )
import RVV.EvalMode (MonadEvalMode)
import RVV.Parser.ArgParser
  ( maskMulParser,
    widthMulParser,
  )
import RVV.Semantics.Imm (Imm)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Multiplier (MaskMul, WidthMul)
import RVV.Semantics.PrimOp.Broadcast (fullBroadcast)
import RVV.Semantics.PrimOp.ImmToReg (fullImmToVector)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.Value (Mask (Mask), Vector (Vector))
import RVV.Semantics.VectorConfig (getDelegatedVectorConfig)
import RVV.Synthesizer.CostModel.CostModel
  ( CostModel (CostModel, maskOpCost, modelMachineConfig, singleWidthCost),
  )
import RVV.Synthesizer.Feature.ExtractFeature (ExtractFeature (extractFeature))
import RVV.Synthesizer.Feature.FeatureSet
  ( Feature (Masking),
    FeatureSet (opFeatures),
    maskMulFeature,
  )
import RVV.Synthesizer.Lens
  ( HasMaskMul (maskMul),
    HasRhs (rhs),
    HasSingleWidthIntBinaryOpCode (singleWidthIntBinaryOpCode),
    HasWidthMulDelegated (widthMulDelegated),
  )
import RVV.Synthesizer.OpSemantics.ElementWise
  ( applyElementWiseMMElementWise,
    typeElementWiseMI,
    typeElementWiseMM,
    typeElementWiseMX,
  )
import RVV.Synthesizer.Operator.Common.OpSymmetryReduction
  ( checkOpCommutativeArgPos,
  )
import RVV.Synthesizer.Operator.Common.PPrint
  ( PPrintArgList (pformatArgList),
    pformatChoices,
    withName,
  )
import RVV.Synthesizer.Operator.Common.RHSSpec
  ( RHSSpec (ImmRHS, ScalarRHS, VectorRHS),
    SketchRHSSpec,
    rhsParser,
  )
import RVV.Synthesizer.Operator.Common.TypeSketchOp (typeSketchOp)
import RVV.Synthesizer.Operator.SingleWidthIntBinary
  ( singleWidthIntBinaryImmRHSSupportedOpCode,
    singleWidthIntBinaryScalarRHSSupportedOpCode,
    singleWidthIntBinaryVectorRHSSupportedOpCode,
  )
import RVV.Synthesizer.Parameter.Common (checkUnsupportedOpCode)
import RVV.Synthesizer.Parameter.SingleWidthIntBinaryOpCode
  ( SingleWidthIntBinaryOpCode,
    commutativeSingleWidthIntBinaryOp,
    interpretSingleWidthBinaryOpCode,
    singleWidthIntBinaryOpCodeFeature,
    singleWidthIntBinaryOpCodeParser,
  )
import RVV.Synthesizer.SketchGen.ToFastSketch (ToFastSketch (toFastSketch))
import RVV.Synthesizer.Specification.ScaleLMul
  ( ScaleLMul (scaleLMul),
  )
import RVV.Synthesizer.Type (ValueType)
import RVV.Synthesizer.Value (Value (MaskValue), extractScalarValue)
import RVV.Util.Context (assert)
import RVV.Util.Derive
  ( deriveFullExcept,
    deriveNoSymEval,
    firstModeDeriveConfig,
  )

data DelegatedVectorBinaryOnMask mode
  = DelegatedVectorBinaryOnMask
  { _widthMulDelegated :: WidthMul,
    _maskMul :: MaskMul,
    _singleWidthIntBinaryOpCode ::
      GetData mode SingleWidthIntBinaryOpCode,
    _rhs :: RHSSpec mode
  }
  deriving (Generic)

data SketchDelegatedVectorBinaryOnMask
  = SketchDelegatedVectorBinaryOnMask
  { _widthMulDelegated :: WidthMul,
    _maskMul :: MaskMul,
    _singleWidthIntBinaryOpCode :: [SingleWidthIntBinaryOpCode],
    _rhs :: SketchRHSSpec
  }
  deriving (Generic)

deriveFullExcept
  firstModeDeriveConfig
  [''DelegatedVectorBinaryOnMask]
  [''UnifiedSymEq]
deriveNoSymEval [''SketchDelegatedVectorBinaryOnMask]
makeFieldsNoPrefix ''DelegatedVectorBinaryOnMask
makeFieldsNoPrefix ''SketchDelegatedVectorBinaryOnMask

delegatedVectorBinaryOnMask ::
  (DelegatedVectorBinaryOnMask mode :<: op) =>
  WidthMul ->
  MaskMul ->
  GetData mode SingleWidthIntBinaryOpCode ->
  RHSSpec mode ->
  op
delegatedVectorBinaryOnMask xmul mmul binOp rhs =
  inj $ DelegatedVectorBinaryOnMask xmul mmul binOp rhs

sketchDelegatedVectorBinaryOnMask ::
  (SketchDelegatedVectorBinaryOnMask :<: op) =>
  WidthMul ->
  MaskMul ->
  [SingleWidthIntBinaryOpCode] ->
  SketchRHSSpec ->
  op
sketchDelegatedVectorBinaryOnMask xmul mmul binOp rhs =
  inj $ SketchDelegatedVectorBinaryOnMask xmul mmul binOp rhs

applyDelegatedVectorBinaryOnMask ::
  (SemConstraint mode ctx) =>
  MachineConfig ->
  WidthMul ->
  MaskMul ->
  GetData mode SingleWidthIntBinaryOpCode ->
  [Value mode] ->
  ctx [Value mode]
applyDelegatedVectorBinaryOnMask vconst xmul maskMul binOp inputs = do
  binOp <- extractData binOp
  applyElementWiseMMElementWise
    vconst
    xmul
    maskMul
    ( \a b -> do
        mrgReturn $ interpretSingleWidthBinaryOpCode binOp a b
    )
    inputs

augmentElementWiseMXArgList ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  WidthMul ->
  MaskMul ->
  [Value mode] ->
  ctx [Value mode]
augmentElementWiseMXArgList vconst xmul maskMul list = do
  config <- getDelegatedVectorConfig vconst xmul maskMul
  assert "augmentElementWiseMXArgList: Bad number of arguments" $
    length list == 2
  rhsScalar <- extractScalarValue $ last list
  Vector _ rhsRegs <- fullBroadcast vconst config rhsScalar
  case rhsRegs of
    [rhsReg] -> mrgReturn [head list, MaskValue $ Mask maskMul rhsReg]
    _ ->
      mrgThrowError $
        "augmentElementWiseMXArgList: "
          <> "vtype should only occupy a single register"

augmentElementWiseMIArgList ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  WidthMul ->
  MaskMul ->
  Imm mode ->
  [Value mode] ->
  ctx [Value mode]
augmentElementWiseMIArgList vconst xmul maskMul imm list = do
  config <- getDelegatedVectorConfig vconst xmul maskMul
  Vector _ rhsRegs <- fullImmToVector vconst config True imm
  case rhsRegs of
    [rhsReg] -> mrgReturn $ list ++ [MaskValue (Mask maskMul rhsReg)]
    _ ->
      mrgThrowError $
        "augmentElementWiseMXArgList: "
          <> "vtype should only occupy a single register"

instance
  ( SemConstraint mode ctx,
    EvalModeConvertible opMode mode,
    MonadEvalMode opMode ctx
  ) =>
  OpSemantics MachineConfig (DelegatedVectorBinaryOnMask opMode) (Value mode) ctx
  where
  applyOp vconst _ op inputs = do
    case toSym op of
      DelegatedVectorBinaryOnMask xmul maskMul opCode VectorRHS -> do
        checkUnsupportedOpCode
          "DelegatedVectorBinaryOnMask (VectorRHS)"
          opCode
          singleWidthIntBinaryVectorRHSSupportedOpCode
        applyDelegatedVectorBinaryOnMask vconst xmul maskMul opCode inputs
      DelegatedVectorBinaryOnMask xmul maskMul opCode ScalarRHS -> do
        checkUnsupportedOpCode
          "DelegatedVectorBinaryOnMask (ScalarRHS)"
          opCode
          singleWidthIntBinaryScalarRHSSupportedOpCode
        newInputs <- augmentElementWiseMXArgList vconst xmul maskMul inputs
        applyDelegatedVectorBinaryOnMask vconst xmul maskMul opCode newInputs
      DelegatedVectorBinaryOnMask xmul maskMul opCode (ImmRHS imm) -> do
        checkUnsupportedOpCode
          "DelegatedVectorBinaryOnMask (ImmRHS)"
          opCode
          singleWidthIntBinaryImmRHSSupportedOpCode
        newInputs <- augmentElementWiseMIArgList vconst xmul maskMul imm inputs
        applyDelegatedVectorBinaryOnMask vconst xmul maskMul opCode newInputs
      DelegatedVectorBinaryOnMask _ _ _ rhs -> do
        throwError $ "Unsupported RHS: " <> showAsText rhs

instance (SemConstraint mode ctx) => OpTyping (DelegatedVectorBinaryOnMask mode) ctx where
  type OpTypeType (DelegatedVectorBinaryOnMask mode) = ValueType
  typeOp (DelegatedVectorBinaryOnMask _ maskMul _ VectorRHS) =
    typeElementWiseMM maskMul
  typeOp (DelegatedVectorBinaryOnMask xmul maskMul _ ScalarRHS) =
    typeElementWiseMX xmul maskMul
  typeOp (DelegatedVectorBinaryOnMask _ maskMul _ (ImmRHS _)) =
    typeElementWiseMI maskMul
  typeOp (DelegatedVectorBinaryOnMask _ _ _ rhs) =
    throwError $ "Unsupported RHS: " <> showAsText rhs

instance
  (MonadAngelicContext ctx) =>
  OpTyping SketchDelegatedVectorBinaryOnMask ctx
  where
  type OpTypeType SketchDelegatedVectorBinaryOnMask = ValueType
  typeOp = typeSketchOp @(DelegatedVectorBinaryOnMask 'S)

instance
  (MonadEvalMode mode Union) =>
  OpSymmetryReduction (DelegatedVectorBinaryOnMask mode)
  where
  opUnreorderable = error "Should not be called"
  opCommutativeArgPos (DelegatedVectorBinaryOnMask _ _ opCode VectorRHS) =
    checkOpCommutativeArgPos
      opCode
      commutativeSingleWidthIntBinaryOp
      [[0, 1]]
  opCommutativeArgPos _ = mrgReturn []

instance
  GenSym
    SketchDelegatedVectorBinaryOnMask
    (DelegatedVectorBinaryOnMask 'S)

instance
  GenSymSimple
    SketchDelegatedVectorBinaryOnMask
    (DelegatedVectorBinaryOnMask 'S)
  where
  simpleFresh (SketchDelegatedVectorBinaryOnMask xmul mmul binOp rhs) = do
    binOp <- chooseFresh binOp
    rhs <- simpleFresh rhs
    return $ DelegatedVectorBinaryOnMask xmul mmul binOp rhs

instance
  GenSym
    (DelegatedVectorBinaryOnMask 'C)
    (DelegatedVectorBinaryOnMask 'S)

instance
  GenSymSimple
    (DelegatedVectorBinaryOnMask 'C)
    (DelegatedVectorBinaryOnMask 'S)
  where
  simpleFresh = return . toSym

instance OpPPrint (DelegatedVectorBinaryOnMask 'C) where
  describeArguments _ = return []

instance PPrint (DelegatedVectorBinaryOnMask 'C) where
  pformat (DelegatedVectorBinaryOnMask xmul maskMul opCode rhs) =
    "v"
      <> pformat opCode
      <> ".delegated"
      <> pformatArgList
        ( withName "xmul" xmul,
          withName "mmul" maskMul,
          rhs
        )

instance PPrint SketchDelegatedVectorBinaryOnMask where
  pformat (SketchDelegatedVectorBinaryOnMask xmul mmul binOp rhs) =
    "v"
      <> pformatChoices binOp
      <> ".delegated"
      <> pformatArgList (withName "xmul" xmul, withName "mmul" mmul, rhs)

instance OpPPrint SketchDelegatedVectorBinaryOnMask where
  describeArguments _ = return []
  prefixResults _ = return []

instance OpParser (DelegatedVectorBinaryOnMask 'C) where
  opParser = do
    op <- singleWidthIntBinaryOpCodeParser "v" ".delegated"
    (xmul, maskMul, rhs) <-
      bracketCommaSep3
        (named "xmul" widthMulParser)
        (named "mmul" maskMulParser)
        rhsParser
    return $ DelegatedVectorBinaryOnMask xmul maskMul op rhs

instance OpReachableSymbols (DelegatedVectorBinaryOnMask 'C) where
  opReachableSymbols = mempty

instance SplitChoice SketchDelegatedVectorBinaryOnMask where
  splitChoice (SketchDelegatedVectorBinaryOnMask xmul mmul binOp rhs) = do
    binOp <- binOp
    rhs <- splitChoice rhs
    return $ SketchDelegatedVectorBinaryOnMask xmul mmul [binOp] rhs

instance
  (SemConstraint mode ctx, Num cost, Mergeable cost) =>
  OpCost CostModel (DelegatedVectorBinaryOnMask mode) cost ctx
  where
  opCost CostModel {..} _ (DelegatedVectorBinaryOnMask xmul mmul opCode _) = do
    config <- getDelegatedVectorConfig modelMachineConfig xmul mmul
    op <- extractData opCode
    mrgReturn $ fromIntegral $ singleWidthCost op config

instance ExtractFeature (DelegatedVectorBinaryOnMask 'C) FeatureSet where
  extractFeature (DelegatedVectorBinaryOnMask _ mmul (Identity op) _) =
    maskMulFeature mmul
      <> mempty {opFeatures = HS.singleton Masking}
      <> singleWidthIntBinaryOpCodeFeature op

instance
  ToFastSketch
    (DelegatedVectorBinaryOnMask 'C)
    SketchDelegatedVectorBinaryOnMask
  where
  toFastSketch (DelegatedVectorBinaryOnMask xmul mmul opCode rhs) =
    SketchDelegatedVectorBinaryOnMask xmul mmul (toList opCode) (toFastSketch rhs)

instance ScaleLMul (DelegatedVectorBinaryOnMask 'C) where
  scaleLMul ratio op = op & (maskMul %~ scaleLMul ratio)

instance ScaleLMul SketchDelegatedVectorBinaryOnMask where
  scaleLMul ratio op = op & (maskMul %~ scaleLMul ratio)

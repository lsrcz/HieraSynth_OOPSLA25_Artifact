{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.Operator.Common.RHSSpec
  ( RHSSpec (..),
    SketchRHSSpec (..),
    rhsParser,
    augmentElementWiseVXArgList,
    augmentElementWiseVIArgList,
    augmentFullScalarVXArgList,
    augmentMulAddScalarArgList,
    augmentMulAddImmArgList,
    augmentSlideVIArgList,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Lens (makeFieldsNoPrefix)
import Data.Bits (FiniteBits (finiteBitSize))
import GHC.Stack (HasCallStack)
import Grisette
  ( BV (bvSelect),
    GenSym,
    GenSymSimple (simpleFresh),
    PPrint (pformat),
    mrgReturn,
  )
import HieraSynth.Program.Choice.Counting (SplitChoice (splitChoice))
import HieraSynth.Util.Parser (CharParser, symbol)
import Grisette.Unified (EvalModeTag (C, S), UnifiedSymEq)
import RVV.Semantics.Imm (Imm)
import RVV.Semantics.MachineConfig
  ( MachineBaseConfig (machineScalarLength),
    MachineConfig (baseConfig),
  )
import RVV.Semantics.PrimOp.Broadcast (fullBroadcast)
import RVV.Semantics.PrimOp.ImmToReg (fullImmToVector, immToScalarForVInst)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.SizeConstraint (validScalar)
import RVV.Semantics.Value (Scalar (Scalar))
import RVV.Semantics.VectorConfig (VectorConfig (elementWidthMul), vectorElementBitWidth)
import RVV.Synthesizer.Lens
  ( HasImm (imm),
  )
import RVV.Synthesizer.Operator.Common.ImmSpec (ImmSpec)
import RVV.Synthesizer.Operator.Common.Parser (immParser)
import RVV.Synthesizer.SketchGen.ToFastSketch (ToFastSketch (toFastSketch))
import RVV.Synthesizer.Value (Value (ScalarValue, VectorValue), extractScalarValue)
import RVV.Util.Context (assert)
import RVV.Util.Derive
  ( deriveFullExcept,
    deriveNoSymEval,
    firstModeDeriveConfig,
  )

data RHSSpec mode
  = VectorRHS
  | FullScalarRHS
  | ScalarRHS
  | ImmRHS {_imm :: Imm mode}

data SketchRHSSpec
  = SketchVectorRHS
  | SketchScalarRHS
  | SketchImmRHS {_imm :: ImmSpec}

deriveFullExcept
  firstModeDeriveConfig
  [''RHSSpec]
  [''UnifiedSymEq]
deriveNoSymEval [''SketchRHSSpec]
makeFieldsNoPrefix ''RHSSpec
makeFieldsNoPrefix ''SketchRHSSpec

instance PPrint (RHSSpec 'C) where
  pformat VectorRHS = "vector_rhs"
  pformat ScalarRHS = "scalar_rhs"
  pformat FullScalarRHS = "full_scalar_rhs"
  pformat (ImmRHS imm) = "imm_rhs=" <> pformat imm

instance PPrint SketchRHSSpec where
  pformat SketchVectorRHS = "vector_rhs"
  pformat SketchScalarRHS = "scalar_rhs"
  pformat (SketchImmRHS imm) = "imm_rhs=" <> pformat imm

rhsParser :: (CharParser e s m) => m (RHSSpec 'C)
rhsParser =
  (symbol "vector_rhs" >> return VectorRHS)
    <|> (symbol "scalar_rhs" >> return ScalarRHS)
    <|> (symbol "full_scalar_rhs" >> return FullScalarRHS)
    <|> ( do
            symbol "imm_rhs"
            symbol "="
            ImmRHS <$> immParser
        )

instance GenSymSimple SketchRHSSpec (RHSSpec 'S) where
  simpleFresh SketchVectorRHS = return VectorRHS
  simpleFresh SketchScalarRHS = return ScalarRHS
  simpleFresh (SketchImmRHS imm) = do
    imm <- simpleFresh imm
    return $ ImmRHS imm

instance GenSym SketchRHSSpec (RHSSpec 'S)

instance SplitChoice SketchRHSSpec where
  splitChoice SketchVectorRHS = [SketchVectorRHS]
  splitChoice SketchScalarRHS = [SketchScalarRHS]
  splitChoice (SketchImmRHS imm) = [SketchImmRHS imm]

instance ToFastSketch (RHSSpec 'C) SketchRHSSpec where
  toFastSketch VectorRHS = SketchVectorRHS
  toFastSketch ScalarRHS = SketchScalarRHS
  toFastSketch FullScalarRHS = SketchScalarRHS
  toFastSketch (ImmRHS imm) = SketchImmRHS (toFastSketch imm)

augmentElementWiseVXArgList ::
  forall mode ctx.
  (HasCallStack, SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  Int ->
  [Value mode] ->
  ctx [Value mode]
augmentElementWiseVXArgList vconst config pos list = do
  assert "No enough arguments" (length list > pos)
  rhsScalar <- extractScalarValue $ list !! pos
  rhsVector <- fullBroadcast vconst config rhsScalar
  mrgReturn $
    take pos list
      ++ [VectorValue rhsVector]
      ++ drop (pos + 1) list

-- | TODO: signedness for the extension need to be fixed for shifting operations
augmentElementWiseVIArgList ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  Imm mode ->
  Int ->
  [Value mode] ->
  ctx [Value mode]
augmentElementWiseVIArgList vconst config imm pos list = do
  assert "No enough arguments" (length list >= pos)
  rhsVector <- fullImmToVector vconst config True imm
  mrgReturn $
    take pos list
      ++ [VectorValue rhsVector]
      ++ drop pos list

augmentFullScalarVXArgList ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  Int ->
  [Value mode] ->
  ctx [Value mode]
augmentFullScalarVXArgList vconst config pos list = do
  assert "No enough arguments" (length list > pos)
  scalar@(Scalar rhsScalar uninit) <-
    extractScalarValue $ list !! pos
  validScalar vconst 1 scalar
  assert "Scalar must have xlen bits" $
    finiteBitSize rhsScalar == machineScalarLength (baseConfig vconst)
  rhsVector <-
    fullBroadcast vconst config $
      Scalar (bvSelect 0 (vectorElementBitWidth vconst config) rhsScalar) uninit
  mrgReturn $
    take pos list
      ++ [VectorValue rhsVector]
      ++ drop (pos + 1) list

augmentMulAddScalarArgList ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  Int ->
  [Value mode] ->
  ctx [Value mode]
augmentMulAddScalarArgList vconst config pos list = do
  assert
    "Multiply Add Scalar function should have enough arguments"
    (length list > pos)
  rhsScalar <- extractScalarValue $ list !! pos
  rhsVector <- fullBroadcast vconst config rhsScalar
  mrgReturn $
    take pos list
      ++ [VectorValue rhsVector]
      ++ drop (pos + 1) list

augmentMulAddImmArgList ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  Imm mode ->
  Int ->
  [Value mode] ->
  ctx [Value mode]
augmentMulAddImmArgList vconst config imm pos list = do
  assert
    "Multiply Add Scalar function should have enough arguments"
    (length list >= pos)
  rhsVector <- fullImmToVector vconst config True imm
  mrgReturn $
    take pos list
      ++ [VectorValue rhsVector]
      ++ drop pos list

augmentSlideVIArgList ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  Bool ->
  MachineConfig ->
  VectorConfig ->
  Imm mode ->
  Int ->
  [Value mode] ->
  ctx [Value mode]
augmentSlideVIArgList isSlide1 vconst config imm pos list = do
  assert
    "slide operation should have enough arguments"
    (length list > pos)
  rhsScalar <-
    immToScalarForVInst
      vconst
      isSlide1
      (if isSlide1 then elementWidthMul config else 1)
      imm
  mrgReturn $ take pos list ++ [ScalarValue rhsScalar] ++ drop pos list

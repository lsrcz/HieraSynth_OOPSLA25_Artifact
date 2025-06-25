{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.OpSemantics.Reinterpret
  ( applyVectorToVector,
    applyMaskToVector,
    applyVectorToMask,
    typeVectorToVector,
    typeMaskToVector,
    typeVectorToMask,
    applyMaskCast,
    typeMaskCast,
  )
where

import Grisette (mrgReturn)
import HieraSynth.Context (MonadContext)
import HieraSynth.TypeSignature (TypeSignature (TypeSignature))
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Multiplier (MaskMul)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.SizeConstraint
  ( validMask,
    validMaskMul,
    validVector,
    validVectorConfig,
  )
import RVV.Semantics.Value (Mask (Mask), Vector (Vector))
import RVV.Semantics.VectorConfig (VectorConfig, vectorNumRegisters)
import RVV.Synthesizer.Type (ValueType (MaskType, VectorType))
import RVV.Synthesizer.Value
  ( Value (MaskValue, VectorValue),
    extractMaskValue,
    extractVectorValue,
  )
import RVV.Util.Context (assert)

applyVectorToVector ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  VectorConfig ->
  [Value mode] ->
  ctx [Value mode]
applyVectorToVector vconst srcConfig destConfig values = do
  validVectorConfig vconst srcConfig
  validVectorConfig vconst destConfig
  assert "vreg_to_vreg: should have 1 arguments" $ length values == 1
  assert
    ( "vreg_to_vreg: srcVType and destVType should have the same number of "
        <> "registers"
    )
    $ vectorNumRegisters srcConfig == vectorNumRegisters destConfig
  assert "vreg_to_vreg: srcVType and destVType should not be the same" $
    srcConfig /= destConfig
  vec@(Vector _ regs) <- extractVectorValue (head values)
  validVector vconst srcConfig vec
  mrgReturn [VectorValue $ Vector destConfig regs]

applyMaskToVector ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  MaskMul ->
  VectorConfig ->
  [Value mode] ->
  ctx [Value mode]
applyMaskToVector vconst srcMaskMul destConfig values = do
  validMaskMul vconst srcMaskMul
  validVectorConfig vconst destConfig
  assert "vmask_to_vreg: should have 1 arguments" $ length values == 1
  assert "vmask_to_vreg: destVType must have a single reg" $
    vectorNumRegisters destConfig == 1
  mask@(Mask _ reg) <- extractMaskValue (head values)
  validMask vconst srcMaskMul mask
  mrgReturn [VectorValue $ Vector destConfig [reg]]

applyVectorToMask ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  MaskMul ->
  [Value mode] ->
  ctx [Value mode]
applyVectorToMask vconst srcConfig destMaskMul values = do
  validVectorConfig vconst srcConfig
  validMaskMul vconst destMaskMul
  assert "vreg_to_vmask: should have 1 arguments" $ length values == 1
  assert "vreg_to_vmask: srcVType must have a single reg" $
    vectorNumRegisters srcConfig == 1
  vec@(Vector _ regs) <- extractVectorValue (head values)
  validVector vconst srcConfig vec
  mrgReturn [MaskValue $ Mask destMaskMul $ head regs]

applyMaskCast ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  MaskMul ->
  MaskMul ->
  [Value mode] ->
  ctx [Value mode]
applyMaskCast vconst srcMaskMul destMaskMul values = do
  validMaskMul vconst srcMaskMul
  validMaskMul vconst destMaskMul
  assert "mask_cast: should have 1 arguments" $ length values == 1
  mask@(Mask _ vreg) <- extractMaskValue (head values)
  validMask vconst srcMaskMul mask
  return [MaskValue $ Mask destMaskMul vreg]

typeVectorToVector ::
  (MonadContext ctx) =>
  VectorConfig ->
  VectorConfig ->
  ctx (TypeSignature ValueType)
typeVectorToVector srcConfig destConfig =
  mrgReturn $ TypeSignature [VectorType srcConfig] [VectorType destConfig]

typeMaskToVector ::
  (MonadContext ctx) =>
  MaskMul ->
  VectorConfig ->
  ctx (TypeSignature ValueType)
typeMaskToVector srcMaskMul destConfig =
  mrgReturn $ TypeSignature [MaskType srcMaskMul] [VectorType destConfig]

typeVectorToMask ::
  (MonadContext ctx) =>
  VectorConfig ->
  MaskMul ->
  ctx (TypeSignature ValueType)
typeVectorToMask srcConfig destMaskMul = do
  assert "vreg_to_vmask: srcVType must have a single reg" $
    vectorNumRegisters srcConfig == 1
  mrgReturn $ TypeSignature [VectorType srcConfig] [MaskType destMaskMul]

typeMaskCast ::
  (MonadContext ctx) =>
  MaskMul ->
  MaskMul ->
  ctx (TypeSignature ValueType)
typeMaskCast srcMaskMul destMaskMul =
  mrgReturn $ TypeSignature [MaskType srcMaskMul] [MaskType destMaskMul]

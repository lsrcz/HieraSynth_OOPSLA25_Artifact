{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.OpSemantics.VSet
  ( applyVSet,
    applyLMulExtend,
    typeVSet,
    typeLMulExtend,
    applyInsertMask,
    typeInsertMask,
  )
where

import Grisette (mrgReturn)
import HieraSynth.Context (MonadContext)
import HieraSynth.TypeSignature (TypeSignature (TypeSignature))
import Grisette.Unified (GetData, extractData)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Multiplier (MaskMul)
import RVV.Semantics.PrimOp.Undefined (undefinedMask, undefinedVector)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.PrimOp.VSet (insertMask, lmulExtend, vset)
import RVV.Semantics.SizeConstraint (validMaskMul)
import RVV.Semantics.VectorConfig (VectorConfig)
import RVV.Synthesizer.Parameter.Destination (Destination (UseProvidedDest, UseUndefinedDest))
import RVV.Synthesizer.Type
  ( ValueType (MaskType, VectorType),
  )
import RVV.Synthesizer.Value
  ( Value (MaskValue, VectorValue),
    extractMaskValue,
    extractVectorValue,
  )
import RVV.Util.Context (assert)

applyInsertMask ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  MaskMul ->
  MaskMul ->
  GetData mode Destination ->
  Int ->
  [Value mode] ->
  ctx [Value mode]
applyInsertMask vconst partMaskMul destMaskMul destination idx values = do
  validMaskMul vconst destMaskMul
  validMaskMul vconst partMaskMul
  assert "insert_mask: should have at least 1 arguments" $ not (null values)
  src <- extractMaskValue (head values)
  dest <- extractData destination
  dest <- case dest of
    UseProvidedDest -> do
      assert "insert_mask: should have 2 arguments" $ length values == 2
      extractMaskValue $ values !! 1
    UseUndefinedDest -> do
      assert "insert_mask: should have 1 arguments" $ length values == 1
      mrgReturn $ undefinedMask vconst destMaskMul
  res <- insertMask vconst partMaskMul destMaskMul idx dest src
  mrgReturn [MaskValue res]

applyVSet ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  VectorConfig ->
  GetData mode Destination ->
  Int ->
  [Value mode] ->
  ctx [Value mode]
applyVSet vconst partConfig destConfig destination idx values = do
  dest <- extractData destination
  assert "vset: should have at least 1 arguments" $ not (null values)
  part <- extractVectorValue (head values)
  dest <- case dest of
    UseProvidedDest -> do
      assert "vset: should have 2 arguments" $ length values == 2
      extractVectorValue (values !! 1)
    UseUndefinedDest -> do
      assert "vset: should have 1 arguments" $ length values == 1
      mrgReturn $ undefinedVector vconst destConfig
  res <- vset vconst partConfig destConfig idx dest part
  mrgReturn [VectorValue res]

applyLMulExtend ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  VectorConfig ->
  [Value mode] ->
  ctx [Value mode]
applyLMulExtend vconst partConfig destConfig values = do
  assert "lmul_extend: should have 1 arguments" $ length values == 1
  part <- extractVectorValue (head values)
  res <- lmulExtend vconst partConfig destConfig part
  mrgReturn [VectorValue res]

typeInsertMask ::
  (SemConstraint mode ctx) =>
  MaskMul ->
  MaskMul ->
  GetData mode Destination ->
  ctx (TypeSignature ValueType)
typeInsertMask partMaskMul destMaskMul destination = do
  dest <- extractData destination
  case dest of
    UseProvidedDest -> do
      let argTypes = [MaskType partMaskMul, MaskType destMaskMul]
      let resTypes = [MaskType destMaskMul]
      pure $ TypeSignature argTypes resTypes
    UseUndefinedDest -> do
      let argTypes = [MaskType partMaskMul]
      let resTypes = [MaskType destMaskMul]
      pure $ TypeSignature argTypes resTypes

typeVSet ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  VectorConfig ->
  GetData mode Destination ->
  ctx (TypeSignature ValueType)
typeVSet partConfig destConfig destination = do
  dest <- extractData destination
  case dest of
    UseProvidedDest -> do
      let argTypes =
            [VectorType partConfig, VectorType destConfig]
      let resTypes = [VectorType destConfig]
      pure $ TypeSignature argTypes resTypes
    UseUndefinedDest -> do
      let argTypes = [VectorType partConfig]
      let resTypes = [VectorType destConfig]
      pure $ TypeSignature argTypes resTypes

typeLMulExtend ::
  (MonadContext ctx) =>
  VectorConfig ->
  VectorConfig ->
  ctx (TypeSignature ValueType)
typeLMulExtend partConfig destConfig = do
  let argTypes = [VectorType partConfig]
  let resTypes = [VectorType destConfig]
  pure $ TypeSignature argTypes resTypes

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.OpSemantics.VGet
  ( applyVGet,
    applyLMulTruncate,
    typeVGet,
    typeLMulTruncate,
    applyExtractMask,
    typeExtractMask,
  )
where

import Grisette (mrgReturn)
import HieraSynth.Context (MonadContext)
import HieraSynth.TypeSignature (TypeSignature (TypeSignature))
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Multiplier (MaskMul)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.PrimOp.VGet (extractMask, lmulTruncate, vget)
import RVV.Semantics.SizeConstraint (validMaskMul)
import RVV.Semantics.VectorConfig (VectorConfig)
import RVV.Synthesizer.Type (ValueType (MaskType, VectorType))
import RVV.Synthesizer.Value
  ( Value (MaskValue, VectorValue),
    extractMaskValue,
    extractVectorValue,
  )
import RVV.Util.Context (assert)

applyExtractMask ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  MaskMul ->
  MaskMul ->
  Int ->
  [Value mode] ->
  ctx [Value mode]
applyExtractMask vconst partMaskMul srcMaskMul idx values = do
  validMaskMul vconst srcMaskMul
  validMaskMul vconst partMaskMul
  assert "extract_mask: should have 1 arguments" $ length values == 1
  mask <- extractMaskValue (head values)
  res <- extractMask vconst partMaskMul srcMaskMul idx mask
  mrgReturn [MaskValue res]

applyVGet ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  VectorConfig ->
  Int ->
  [Value mode] ->
  ctx [Value mode]
applyVGet vconst partConfig srcConfig idx values = do
  assert "vget: should have 1 arguments" $ length values == 1
  src <- extractVectorValue (head values)
  res <- vget vconst partConfig srcConfig idx src
  mrgReturn [VectorValue res]

applyLMulTruncate ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  VectorConfig ->
  [Value mode] ->
  ctx [Value mode]
applyLMulTruncate vconst partConfig srcConfig values = do
  assert "lmul_truncate: should have 1 arguments" $ length values == 1
  src <- extractVectorValue (head values)
  res <- lmulTruncate vconst partConfig srcConfig src
  mrgReturn [VectorValue res]

typeExtractMask ::
  (MonadContext ctx) =>
  MaskMul ->
  MaskMul ->
  ctx (TypeSignature ValueType)
typeExtractMask partMaskMul srcMaskMul = do
  let argTypes = [MaskType srcMaskMul]
  let resTypes = [MaskType partMaskMul]
  pure $ TypeSignature argTypes resTypes

typeVGet ::
  (MonadContext ctx) =>
  VectorConfig ->
  VectorConfig ->
  ctx (TypeSignature ValueType)
typeVGet partConfig srcConfig = do
  let argTypes = [VectorType srcConfig]
  let resTypes = [VectorType partConfig]
  pure $ TypeSignature argTypes resTypes

typeLMulTruncate ::
  (MonadContext ctx) =>
  VectorConfig ->
  VectorConfig ->
  ctx (TypeSignature ValueType)
typeLMulTruncate partConfig srcConfig = do
  let argTypes = [VectorType srcConfig]
  let resTypes = [VectorType partConfig]
  pure $ TypeSignature argTypes resTypes

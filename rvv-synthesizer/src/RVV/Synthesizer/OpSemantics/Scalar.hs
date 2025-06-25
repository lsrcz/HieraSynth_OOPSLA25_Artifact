{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.OpSemantics.Scalar
  ( applyScalarBin,
    applyScalarBinVL,
    applyScalarZero,
    applyScalarShort,
    applyScalarLong,
    applyScalarTrunc,
    typeScalarBin,
    typeScalarBinVL,
    typeScalarBinImm,
    typeScalarBinVLImm,
    typeScalarZero,
    typeScalarShort,
    typeScalarLong,
    typeScalarTrunc,
    applyScalarUnary,
    typeScalarUnary,
  )
where

import Data.Bits (FiniteBits (finiteBitSize))
import Data.Ratio (denominator, numerator)
import Grisette (BV (bv, bvSelect, bvSext), LogicalOp (false, (.||)), mrgReturn)
import HieraSynth.Context (MonadContext)
import HieraSynth.TypeSignature (TypeSignature (TypeSignature))
import HieraSynth.Util.Show (showAsText)
import Grisette.Unified (symFromBits)
import RVV.Semantics.Element (VectorElement (VectorElement), vlValueToVLNum)
import RVV.Semantics.Imm (Imm, getImm)
import RVV.Semantics.MachineConfig
  ( MachineBaseConfig (MachineBaseConfig, machineScalarLength),
    MachineConfig (MachineConfig, baseConfig),
  )
import RVV.Semantics.Multiplier (MaskMul, WidthMul (getWidthMul))
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.SizeConstraint (validScalar, validVL, validWidthMul)
import RVV.Semantics.Value (Scalar (Scalar), VL (VL), VLValue (VLNum))
import RVV.Semantics.VectorConfig (scalarBitWidth)
import RVV.Synthesizer.Type (ValueType (ScalarType, VLType))
import RVV.Synthesizer.Value
  ( Value (ScalarValue),
    extractScalarValue,
    extractVLValue,
  )
import RVV.Util.Context (assert)

applyScalarTrunc ::
  (SemConstraint mode ctx) =>
  MachineConfig ->
  WidthMul ->
  WidthMul ->
  [Value mode] ->
  ctx [Value mode]
applyScalarTrunc vconst srcXMul destXMul values = do
  validWidthMul vconst srcXMul
  validWidthMul vconst destXMul
  assert "applyScalarTrunc: not truncating" $ srcXMul > destXMul
  assert "applyScalarTrunc: invalid argument number" $ length values == 1
  s@(Scalar bv lu) <- extractScalarValue (head values)
  validScalar vconst srcXMul s
  mrgReturn
    [ScalarValue $ Scalar (bvSelect 0 (scalarBitWidth vconst destXMul) bv) lu]

applyScalarUnary ::
  (SemConstraint mode ctx) =>
  MachineConfig ->
  WidthMul ->
  (VectorElement mode -> VectorElement mode) ->
  [Value mode] ->
  ctx [Value mode]
applyScalarUnary vconst@(MachineConfig {baseConfig = MachineBaseConfig {..}}) xmul f values = do
  assert
    ( "scalarUnary: invalid argument number, expected 1, got "
        <> showAsText (length values)
    )
    (length values == 1)
  s@(Scalar bv lu) <- extractScalarValue (head values)
  validScalar vconst xmul s
  let VectorElement v _ =
        f (VectorElement bv (symFromBits $ replicate (finiteBitSize bv) lu))
  mrgReturn [ScalarValue $ Scalar v lu]

applyScalarBin ::
  (SemConstraint mode ctx) =>
  MachineConfig ->
  WidthMul ->
  (VectorElement mode -> VectorElement mode -> VectorElement mode) ->
  [Value mode] ->
  ctx [Value mode]
applyScalarBin vconst@(MachineConfig {baseConfig = MachineBaseConfig {..}}) xmul f values = do
  assert
    ( "scalarBin: invalid argument number, expected 2, got "
        <> showAsText (length values)
    )
    (length values == 2)
  l@(Scalar lbv lu) <- extractScalarValue (head values)
  r@(Scalar rbv ru) <- extractScalarValue (values !! 1)
  validScalar vconst xmul l
  validScalar vconst xmul r
  let VectorElement v _ =
        f
          (VectorElement lbv (symFromBits $ replicate (finiteBitSize lbv) lu))
          (VectorElement rbv (symFromBits $ replicate (finiteBitSize rbv) ru))
  mrgReturn [ScalarValue $ Scalar v (lu .|| ru)]

applyScalarBinVL ::
  (SemConstraint mode ctx) =>
  MachineConfig ->
  MaskMul ->
  (VectorElement mode -> VectorElement mode -> VectorElement mode) ->
  [Value mode] ->
  ctx [Value mode]
applyScalarBinVL vconst maskMul f values = do
  assert
    ( "scalarBinVL: invalid argument number, expected 2, got "
        <> showAsText (length values)
    )
    (length values == 2)
  vl@(VL _ vlValue _ vlu) <- extractVLValue (head values)
  validVL vconst maskMul vl
  let result = vlValueToVLNum vconst vlValue
  case result of
    VLNum vlNum -> do
      applyScalarBin vconst 1 f $
        ScalarValue (Scalar vlNum vlu) : tail values
    _ -> error "Should not happen"

applyScalarZero ::
  (SemConstraint mode ctx) =>
  MachineConfig ->
  WidthMul ->
  [Value mode] ->
  ctx [Value mode]
applyScalarZero MachineConfig {baseConfig = MachineBaseConfig {..}} xmul values = do
  assert
    ( "scalarZero: invalid argument number, expected 0, got "
        <> showAsText (length values)
    )
    (null values)

  let targetBitWidthRatio = fromIntegral machineScalarLength * getWidthMul xmul
  assert "Target bit width must not be fractional" $
    denominator targetBitWidthRatio == 1
  let targetBitWidth = numerator targetBitWidthRatio
  mrgReturn [ScalarValue $ Scalar (bv targetBitWidth 0) false]

applyScalarShort ::
  (SemConstraint mode ctx) =>
  MachineConfig ->
  WidthMul ->
  Imm mode ->
  [Value mode] ->
  ctx [Value mode]
applyScalarShort MachineConfig {baseConfig = MachineBaseConfig {..}} xmul imm values = do
  assert
    ( "scalarShort: invalid argument number, expected 0, got "
        <> showAsText (length values)
    )
    (null values)
  let targetBitWidthRatio = fromIntegral machineScalarLength * getWidthMul xmul
  let targetBitWidth = numerator targetBitWidthRatio
  assert "Target bit width must not be fractional" $
    denominator targetBitWidthRatio == 1
  bv <- getImm (machineScalarLength `div` 2) imm
  assert "scalarShort: imm bit width should be half of XLEN" $
    machineScalarLength == 2 * finiteBitSize bv

  if targetBitWidth > finiteBitSize bv
    then mrgReturn [ScalarValue $ Scalar (bvSext targetBitWidth bv) false]
    else
      if targetBitWidth < finiteBitSize bv
        then
          mrgReturn [ScalarValue $ Scalar (bvSelect 0 targetBitWidth bv) false]
        else mrgReturn [ScalarValue $ Scalar bv false]

applyScalarLong ::
  (SemConstraint mode ctx) =>
  MachineConfig ->
  WidthMul ->
  Imm mode ->
  [Value mode] ->
  ctx [Value mode]
applyScalarLong MachineConfig {baseConfig = MachineBaseConfig {..}} xmul imm values = do
  assert
    ( "scalarLong: invalid argument number, expected 0, got "
        <> showAsText (length values)
    )
    (null values)
  let targetBitWidthRatio = fromIntegral machineScalarLength * getWidthMul xmul
  assert "Target bit width must not be fractional" $
    denominator targetBitWidthRatio == 1
  let targetBitWidth = numerator targetBitWidthRatio
  bv <- getImm machineScalarLength imm
  assert "scalarLong: imm bit width should be XLEN" $
    machineScalarLength == finiteBitSize bv
  if targetBitWidth > finiteBitSize bv
    then mrgReturn [ScalarValue $ Scalar (bvSext targetBitWidth bv) false]
    else
      if targetBitWidth < finiteBitSize bv
        then
          mrgReturn [ScalarValue $ Scalar (bvSelect 0 targetBitWidth bv) false]
        else mrgReturn [ScalarValue $ Scalar bv false]

typeScalarTrunc ::
  (MonadContext ctx) =>
  WidthMul ->
  WidthMul ->
  ctx (TypeSignature ValueType)
typeScalarTrunc srcXMul destXMul = do
  mrgReturn $ TypeSignature [ScalarType srcXMul] [ScalarType destXMul]

typeScalarUnary ::
  (MonadContext ctx) =>
  WidthMul ->
  ctx (TypeSignature ValueType)
typeScalarUnary xmul =
  mrgReturn $
    TypeSignature
      [ScalarType xmul]
      [ScalarType xmul]

typeScalarBin ::
  (MonadContext ctx) =>
  WidthMul ->
  ctx (TypeSignature ValueType)
typeScalarBin xmul =
  mrgReturn $
    TypeSignature
      [ScalarType xmul, ScalarType xmul]
      [ScalarType xmul]

typeScalarBinImm ::
  (MonadContext ctx) =>
  WidthMul ->
  ctx (TypeSignature ValueType)
typeScalarBinImm xmul =
  mrgReturn $
    TypeSignature [ScalarType xmul] [ScalarType xmul]

typeScalarBinVL ::
  (MonadContext ctx) =>
  MaskMul ->
  ctx (TypeSignature ValueType)
typeScalarBinVL maskMul =
  mrgReturn $
    TypeSignature
      [VLType maskMul, ScalarType 1]
      [ScalarType 1]

typeScalarBinVLImm ::
  (MonadContext ctx) =>
  MaskMul ->
  ctx (TypeSignature ValueType)
typeScalarBinVLImm maskMul =
  mrgReturn $
    TypeSignature
      [VLType maskMul]
      [ScalarType 1]

typeScalarZero ::
  (MonadContext ctx) =>
  WidthMul ->
  ctx (TypeSignature ValueType)
typeScalarZero xmul = mrgReturn $ TypeSignature [] [ScalarType xmul]

typeScalarShort ::
  (MonadContext ctx) =>
  WidthMul ->
  ctx (TypeSignature ValueType)
typeScalarShort xmul = mrgReturn $ TypeSignature [] [ScalarType xmul]

typeScalarLong ::
  (MonadContext ctx) =>
  WidthMul ->
  ctx (TypeSignature ValueType)
typeScalarLong xmul = mrgReturn $ TypeSignature [] [ScalarType xmul]

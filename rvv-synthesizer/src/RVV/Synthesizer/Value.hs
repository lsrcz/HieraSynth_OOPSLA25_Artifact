{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.Value
  ( Value (..),
    extractVLValue,
    extractVectorValue,
    extractMaskValue,
    extractScalarValue,
    extractPtrValue,
    extractMemValue,
    freshValidValue,
    freshValidValue',
    maxAvailableBits,
  )
where

import Control.Monad (replicateM)
import Data.Ratio (numerator)
import GHC.Stack (HasCallStack)
import Grisette
  ( BV (bv, bvConcat),
    GenSymSimple (simpleFresh),
    LogicalOp (false),
    PPrint,
    SimpleListSpec (SimpleListSpec),
    deriveWith,
    mrgFmap,
    mrgReturn,
  )
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import HieraSynth.Context (MonadAngelicContext, MonadContext)
import HieraSynth.Program.ComponentSketch
  ( GenIntermediate (genIntermediate),
  )
import Grisette.Unified
  ( EvalModeTag (S),
  )
import RVV.EvalMode (MonadEvalMode)
import RVV.Semantics.MachineConfig
  ( AllowPartialVL (AllowPartialVL, DisallowPartialVL),
    MachineBaseConfig (MachineBaseConfig, machineScalarLength),
    MachineConfig (MachineConfig, baseConfig, useVLMask, allowPartialVL),
  )
import RVV.Semantics.Memory (Memory, Ptr (Ptr), freshValidMemory)
import RVV.Semantics.Multiplier (WidthMul (getWidthMul))
import RVV.Semantics.SizeConstraint (validVL)
import RVV.Semantics.Value
  ( Mask (Mask),
    Scalar (Scalar),
    VL (VL),
    VLValue (VLMask, VLNum),
    Vector (Vector),
    freshValidVectorReg,
    freshValidVectorReg',
    freshValidVectorReg'',
  )
import RVV.Semantics.VectorConfig
  ( maskNumValidElements,
    vectorNumRegisters,
    vectorNumValidBits,
  )
import RVV.Synthesizer.Type
  ( ValueType
      ( MaskType,
        MemType,
        PtrType,
        ScalarType,
        VLType,
        VectorType
      ),
  )
import RVV.Util.Context (assert)
import RVV.Util.Derive (deriveFull, firstModeDeriveConfig)

data Value mode
  = VLValue (VL mode)
  | VectorValue (Vector mode)
  | MaskValue (Mask mode)
  | ScalarValue (Scalar mode)
  | PtrValue (Ptr mode)
  | MemValue (Memory mode)

deriveFull firstModeDeriveConfig [''Value]
deriveWith firstModeDeriveConfig [''Value] [''PPrint]

extractVLValue ::
  (MonadContext ctx, MonadEvalMode mode ctx) =>
  Value mode ->
  ctx (VL mode)
extractVLValue (VLValue vl) = mrgReturn vl
extractVLValue _ = mrgThrowError "Not a VLValue"

extractVectorValue ::
  (MonadContext ctx, MonadEvalMode mode ctx) =>
  Value mode ->
  ctx (Vector mode)
extractVectorValue (VectorValue v) = mrgReturn v
extractVectorValue _ = mrgThrowError "Not a VectorValue"

extractMaskValue ::
  (MonadContext ctx, MonadEvalMode mode ctx) =>
  Value mode ->
  ctx (Mask mode)
extractMaskValue (MaskValue v) = mrgReturn v
extractMaskValue _ = mrgThrowError "Not a MaskValue"

extractScalarValue ::
  (MonadContext ctx, MonadEvalMode mode ctx, HasCallStack) =>
  Value mode ->
  ctx (Scalar mode)
extractScalarValue (ScalarValue x) = mrgReturn x
extractScalarValue _ = assert "Not a ScalarValue" False >> mrgThrowError "Not a ScalarValue"

extractPtrValue ::
  (MonadContext ctx, MonadEvalMode mode ctx) =>
  Value mode ->
  ctx (Ptr mode)
extractPtrValue (PtrValue x) = mrgReturn x
extractPtrValue _ = mrgThrowError "Not a PtrValue"

extractMemValue ::
  (MonadContext ctx, MonadEvalMode mode ctx) =>
  Value mode ->
  ctx (Memory mode)
extractMemValue (MemValue x) = mrgReturn x
extractMemValue _ = mrgThrowError "Not a MemValue"

maxAvailableBits :: MachineConfig -> ValueType -> Int
maxAvailableBits MachineConfig {baseConfig = MachineBaseConfig {..}} (VLType _) = machineScalarLength
maxAvailableBits vconst (MaskType mmul) = maskNumValidElements vconst mmul
maxAvailableBits vconst (VectorType vtype) = vectorNumValidBits vconst vtype
maxAvailableBits MachineConfig {baseConfig = MachineBaseConfig {..}} (ScalarType xmul) =
  numerator $ fromIntegral machineScalarLength * getWidthMul xmul
maxAvailableBits _ _ = 0

freshValidValue' ::
  (MonadAngelicContext ctx) =>
  Int ->
  MachineConfig ->
  ValueType ->
  ctx (Value 'S)
freshValidValue' _ vconst vltype@VLType {..} =
  freshValidValue vconst vltype
freshValidValue' nbit vconst (MaskType ratio) =
  MaskValue . Mask ratio <$> freshValidVectorReg' nbit vconst
freshValidValue' nbit vconst (VectorType vtype) =
  VectorValue . Vector vtype
    <$> replicateM (vectorNumRegisters vtype) (freshValidVectorReg'' nbit vconst vtype)
freshValidValue'
  nbit
  vconst@MachineConfig {baseConfig = MachineBaseConfig {..}}
  scalarType@(ScalarType xmul)
    | fromIntegral nbit >= fromIntegral machineScalarLength * xmul =
        freshValidValue vconst scalarType
    | otherwise =
        let wholeBit = numerator $ fromIntegral machineScalarLength * getWidthMul xmul
         in fmap ScalarValue $
              (Scalar . bvConcat (bv (wholeBit - nbit) 0) <$> simpleFresh nbit)
                <*> return false
freshValidValue' _ vconst ty = freshValidValue vconst ty

freshValidValue ::
  (MonadAngelicContext ctx) => MachineConfig -> ValueType -> ctx (Value 'S)
freshValidValue machine@MachineConfig {baseConfig = MachineBaseConfig {..}, useVLMask, allowPartialVL} (VLType ratio) = do
  let numValidBits = maskNumValidElements machine ratio
  vlValue <-
    if useVLMask
      then do
        mask <-
          ( case allowPartialVL of
              AllowPartialVL -> simpleFresh numValidBits
              DisallowPartialVL -> return (bv numValidBits (-1))
            )
        mrgReturn $ VLMask mask
      else
        VLNum
          <$> ( case allowPartialVL of
                  AllowPartialVL -> simpleFresh machineScalarLength
                  DisallowPartialVL -> return (bv machineScalarLength numValidBits)
              )
  policy <- simpleFresh ()
  let r = VL ratio vlValue policy false
  validVL machine ratio r
  mrgReturn $ VLValue r
freshValidValue vconst (MaskType ratio) = MaskValue . Mask ratio <$> freshValidVectorReg vconst
freshValidValue vconst (VectorType vtype) =
  VectorValue . Vector vtype <$> replicateM (vectorNumRegisters vtype) (freshValidVectorReg vconst)
freshValidValue MachineConfig {baseConfig = MachineBaseConfig {..}} (ScalarType xmul) =
  fmap ScalarValue $
    Scalar
      <$> simpleFresh (numerator $ fromIntegral machineScalarLength * getWidthMul xmul)
      <*> return false
freshValidValue
  MachineConfig {baseConfig = MachineBaseConfig {..}}
  (PtrType width blockId) =
    mrgFmap
      PtrValue
      (Ptr width blockId <$> simpleFresh machineScalarLength <*> return false)
freshValidValue vconst MemType = MemValue <$> freshValidMemory vconst

freshValue ::
  (MonadAngelicContext ctx) => MachineConfig -> ValueType -> ctx (Value 'S)
freshValue machine@MachineConfig {baseConfig = MachineBaseConfig {..}, useVLMask, allowPartialVL} (VLType ratio) = do
  let numValidBits = maskNumValidElements machine ratio
  vlValue <-
    if useVLMask
      then do
        mask <-
          ( case allowPartialVL of
              AllowPartialVL -> simpleFresh numValidBits
              DisallowPartialVL -> return (bv numValidBits (-1))
            )
        mrgReturn $ VLMask mask
      else
        VLNum
          <$> ( case allowPartialVL of
                  AllowPartialVL -> simpleFresh machineScalarLength
                  DisallowPartialVL -> return (bv machineScalarLength numValidBits)
              )
  policy <- simpleFresh ()
  let r = VL ratio vlValue policy false
  validVL machine ratio r
  mrgReturn $ VLValue r
freshValue vconst (MaskType ratio) =
  MaskValue . Mask ratio <$> simpleFresh vconst
freshValue vconst (VectorType vtype) =
  VectorValue . Vector vtype <$> simpleFresh (SimpleListSpec (vectorNumRegisters vtype) vconst)
freshValue MachineConfig {baseConfig = MachineBaseConfig {..}} (ScalarType xmul) =
  fmap ScalarValue $
    flip Scalar false
      <$> simpleFresh (numerator $ fromIntegral machineScalarLength * getWidthMul xmul)
freshValue
  MachineConfig {baseConfig = MachineBaseConfig {..}}
  (PtrType width blockId) =
    mrgFmap
      PtrValue
      (Ptr width blockId <$> simpleFresh machineScalarLength <*> simpleFresh ())
freshValue vconst MemType = MemValue <$> simpleFresh vconst

instance GenIntermediate MachineConfig ValueType (Value 'S) where
  genIntermediate = freshValue

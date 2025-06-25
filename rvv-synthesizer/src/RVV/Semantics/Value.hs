{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file -ddump-file-prefix=val #-}

module RVV.Semantics.Value
  ( VLValue (..),
    VL (..),
    Scalar (..),
    VectorReg (..),
    Mask (..),
    Vector (..),
    freshValidVectorReg,
    freshValidVectorReg',
    freshValidVectorReg'',
    concatData,
    concatUninitialized,
    fromConcatenated,
  )
where

import Control.Monad (replicateM)
import Data.Bits (FiniteBits (finiteBitSize))
import Grisette
  ( BV (bv, bvConcat, bvSelect),
    GenSym,
    GenSymSimple (simpleFresh),
    MonadFresh,
    PPrint,
    deriveWith,
  )
import Grisette.Unified
  ( EvalModeTag (S),
    GetBool,
    GetSomeWordN,
  )
import RVV.EvalMode (EvalMode)
import RVV.Semantics.MachineConfig
  ( MachineConfig (MachineConfig, scalableConfig),
    MachineScalableConfig (MachineScalableConfig, machineVectorLength),
  )
import RVV.Semantics.Multiplier (MaskMul)
import RVV.Semantics.Policy
  ( Policy,
  )
import RVV.Semantics.VectorConfig
  ( VectorConfig,
    vectorElementBitWidth,
  )
import RVV.Util.Derive (deriveFull, firstModeDeriveConfig)

data VLValue mode
  = VLNum (GetSomeWordN mode)
  | VLMask (GetSomeWordN mode)

data VL mode = VL
  { vlMaskMul :: MaskMul,
    vlValue :: VLValue mode,
    vlPolicy :: Policy mode,
    vlUninitialized :: GetBool mode
  }

deriveFull firstModeDeriveConfig [''VL, ''VLValue]
deriveWith firstModeDeriveConfig [''VL, ''VLValue] [''PPrint]

data Scalar mode = Scalar
  { scalarData :: GetSomeWordN mode,
    scalarUninitialized :: GetBool mode
  }

deriveFull firstModeDeriveConfig [''Scalar]
deriveWith firstModeDeriveConfig [''Scalar] [''PPrint]

data VectorReg mode = VectorReg
  { vectorRegData :: GetSomeWordN mode,
    vectorRegUninitialized :: GetSomeWordN mode
  }

deriveFull firstModeDeriveConfig [''VectorReg]
deriveWith firstModeDeriveConfig [''VectorReg] [''PPrint]

instance GenSym MachineConfig (VectorReg 'S)

instance GenSymSimple MachineConfig (VectorReg 'S) where
  simpleFresh MachineConfig {scalableConfig = MachineScalableConfig {..}} =
    VectorReg <$> simpleFresh machineVectorLength <*> simpleFresh machineVectorLength

freshValidVectorReg' :: (MonadFresh m) => Int -> MachineConfig -> m (VectorReg 'S)
freshValidVectorReg' nbit vconst@(MachineConfig {scalableConfig = MachineScalableConfig {..}})
  | nbit >= machineVectorLength = freshValidVectorReg vconst
  | otherwise =
      (VectorReg . bvConcat (bv (machineVectorLength - nbit) 0) <$> simpleFresh nbit)
        <*> return (bv machineVectorLength 0)

freshValidVectorReg :: (MonadFresh m) => MachineConfig -> m (VectorReg 'S)
freshValidVectorReg MachineConfig {scalableConfig = MachineScalableConfig {..}} =
  VectorReg <$> simpleFresh machineVectorLength <*> return (bv machineVectorLength 0)

data Mask mode = Mask
  { maskMultiplier :: MaskMul,
    maskVectorReg :: VectorReg mode
  }

deriveFull firstModeDeriveConfig [''Mask]
deriveWith firstModeDeriveConfig [''Mask] [''PPrint]

data Vector mode = Vector
  { vectorConfig :: VectorConfig,
    vectorVectorRegs :: [VectorReg mode]
  }

deriveFull firstModeDeriveConfig [''Vector]
deriveWith firstModeDeriveConfig [''Vector] [''PPrint]

freshValidVectorReg'' ::
  (MonadFresh m) =>
  Int -> -- number of bits per element
  MachineConfig ->
  VectorConfig -> -- to determine element width
  m (VectorReg 'S)
freshValidVectorReg'' nbitPerElem vconst@(MachineConfig {scalableConfig = MachineScalableConfig {..}}) vectorConfig = do
  let elemWidth = vectorElementBitWidth vconst vectorConfig
      numElems = machineVectorLength `div` elemWidth
      validBitsPerElem = min nbitPerElem elemWidth
      paddingBitsPerElem = elemWidth - validBitsPerElem

  -- Generate valid bits for each element
  elems <- replicateM numElems $ do
    validBits <- simpleFresh validBitsPerElem
    if paddingBitsPerElem > 0
      then return $ bvConcat (bv paddingBitsPerElem 0) validBits
      else return validBits

  -- Concatenate all elements
  let fullReg = foldr1 bvConcat elems
  return $ VectorReg fullReg (bv machineVectorLength 0)

concatData :: (EvalMode mode) => Vector mode -> GetSomeWordN mode
concatData Vector {..} = foldr1 bvConcat $ reverse $ map vectorRegData vectorVectorRegs

concatUninitialized :: (EvalMode mode) => Vector mode -> GetSomeWordN mode
concatUninitialized Vector {..} =
  foldr1 bvConcat $ reverse $ map vectorRegUninitialized vectorVectorRegs

fromConcatenated ::
  (EvalMode mode) =>
  MachineConfig ->
  VectorConfig ->
  Bool ->
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  Vector mode
fromConcatenated machine config padUninitialized d u = do
  let vlen = machineVectorLength $ scalableConfig machine
  let pad n v = if finiteBitSize v < vlen then bvConcat (bv (vlen - finiteBitSize v) n) v else v
  let split p
        | finiteBitSize p <= vlen = [p]
        | otherwise =
            bvSelect 0 vlen p : split (bvSelect vlen (finiteBitSize p - vlen) p)
  let ds = split $ pad 0 d
  let us = if padUninitialized then split $ pad (-1) u else split $ pad 0 u
  Vector config $ zipWith VectorReg ds us

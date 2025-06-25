{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Semantics.VectorConfig
  ( VectorConfig (..),
    scalarBitWidth,
    vectorElementBitWidth,
    vectorMaskMul,
    delegatedVectorMaskMul,
    vectorNumRegisters,
    vectorNumValidElements,
    vectorNumAllElements,
    vectorElementIndices,
    getDelegatedVectorConfig,
    maskNumValidElements,
    maskLengthMul,
    maskValidElementIndices,
    vectorNumValidBits,
    widenVectorConfig,
    narrowVectorConfig,
    halfVectorConfig,
  )
where

import Data.Ratio (denominator, numerator, (%))
import GHC.Stack (HasCallStack)
import Grisette
  ( BV (bv),
    PPrint (pformat),
    mrgReturn,
  )
import HieraSynth.Context (MonadContext)
import RVV.Semantics.MachineConfig
  ( MachineBaseConfig (MachineBaseConfig, machineScalarLength),
    MachineConfig (MachineConfig, baseConfig, scalableConfig),
    MachineScalableConfig (MachineScalableConfig, machineVectorLength),
  )
import RVV.Semantics.Multiplier
  ( LengthMul (LengthMul, getLengthMul),
    MaskMul (MaskMul),
    WidthMul (WidthMul, getWidthMul),
  )
import RVV.Synthesizer.Specification.ScaleLMul (ScaleLMul (scaleLMul))
import RVV.Util.Context (assert)
import RVV.Util.Derive (deriveFull, noModeDeriveConfig)

data VectorConfig = VectorConfig
  { elementWidthMul :: WidthMul,
    lengthMul :: LengthMul
  }

deriveFull noModeDeriveConfig [''VectorConfig]

scalarBitWidth :: (HasCallStack) => MachineConfig -> WidthMul -> Int
scalarBitWidth
  MachineConfig
    { scalableConfig = MachineScalableConfig {..},
      baseConfig = MachineBaseConfig {..}
    }
  (WidthMul mul) =
    if denominator (fromIntegral machineScalarLength * mul) == 1
      then numerator (fromIntegral machineScalarLength * mul)
      else error "scalarBitWidth: xlen * multiplier must be an integer"

vectorElementBitWidth :: (HasCallStack) => MachineConfig -> VectorConfig -> Int
vectorElementBitWidth
  MachineConfig {baseConfig = MachineBaseConfig {..}}
  VectorConfig {..} =
    numerator $ getWidthMul $ fromIntegral machineScalarLength * elementWidthMul

instance PPrint VectorConfig where
  pformat (VectorConfig ewmul emul) =
    "e" <> pformat ewmul <> "m" <> pformat emul

vectorMaskMul :: VectorConfig -> MaskMul
vectorMaskMul VectorConfig {..} =
  MaskMul $ getLengthMul lengthMul / getWidthMul elementWidthMul

delegatedVectorMaskMul ::
  (MonadContext ctx) => MachineConfig -> VectorConfig -> ctx MaskMul
delegatedVectorMaskMul
  MachineConfig {baseConfig = MachineBaseConfig {..}}
  VectorConfig {..} = do
    assert "delegatedVectorMaskMul: must be 1 or fractional EMUL" $
      elementWidthMul <= 1
    mrgReturn $
      MaskMul $
        fromIntegral machineScalarLength * getLengthMul lengthMul

vectorNumRegisters :: VectorConfig -> Int
vectorNumRegisters VectorConfig {..} =
  if lengthMul >= 1
    then
      numerator $ getLengthMul lengthMul
    else 1

getDelegatedVectorConfig ::
  (MonadContext ctx) =>
  MachineConfig ->
  WidthMul ->
  MaskMul ->
  ctx VectorConfig
getDelegatedVectorConfig vconst xmul maskMul = do
  let lmul =
        max
          (maskLengthMul vconst maskMul)
          ( LengthMul $
              getWidthMul xmul
                * ( machineScalarLength
                      (baseConfig vconst)
                      % machineVectorLength (scalableConfig vconst)
                  )
          )
  assert "Vector length multiplier must be 1 or fractional" $ lmul <= 1
  mrgReturn $ VectorConfig xmul lmul

maskNumValidElements :: MachineConfig -> MaskMul -> Int
maskNumValidElements vconst (MaskMul maskMul) =
  numerator $
    (fromIntegral (machineVectorLength (scalableConfig vconst)) * maskMul)
      / fromIntegral (machineScalarLength (baseConfig vconst))

maskLengthMul :: MachineConfig -> MaskMul -> LengthMul
maskLengthMul vconst maskMul =
  let num = maskNumValidElements vconst maskMul
   in LengthMul $ num % machineVectorLength (scalableConfig vconst)

maskValidElementIndices :: (BV r) => MachineConfig -> MaskMul -> [r]
maskValidElementIndices vconst maskMul =
  let num = maskNumValidElements vconst maskMul
   in [bv (machineScalarLength (baseConfig vconst)) i | i <- [0 .. num - 1]]

vectorNumValidElements :: MachineConfig -> VectorConfig -> Int
vectorNumValidElements
  vconst@MachineConfig
    { scalableConfig = MachineScalableConfig {..},
      baseConfig = MachineBaseConfig {..}
    }
  vtype =
    numerator $
      ( fromIntegral machineVectorLength
          * getLengthMul (lengthMul vtype)
      )
        / fromIntegral (vectorElementBitWidth vconst vtype)

vectorNumValidBits :: MachineConfig -> VectorConfig -> Int
vectorNumValidBits vconst vectorConfig =
  vectorNumValidElements vconst vectorConfig
    * vectorElementBitWidth vconst vectorConfig

vectorNumAllElements :: MachineConfig -> VectorConfig -> Int
vectorNumAllElements vconst vectorConfig =
  vectorNumRegisters vectorConfig
    * ( machineVectorLength (scalableConfig vconst)
          `div` vectorElementBitWidth vconst vectorConfig
      )

vectorElementIndices :: (BV r) => MachineConfig -> VectorConfig -> [r]
vectorElementIndices
  vconst@MachineConfig {baseConfig = MachineBaseConfig {..}}
  vectorConfig =
    let num = vectorNumAllElements vconst vectorConfig
     in [bv machineScalarLength i | i <- [0 .. num - 1]]

widenVectorConfig :: VectorConfig -> VectorConfig
widenVectorConfig VectorConfig {..} =
  VectorConfig (elementWidthMul * 2) (lengthMul * 2)

narrowVectorConfig :: VectorConfig -> VectorConfig
narrowVectorConfig VectorConfig {..} =
  VectorConfig (elementWidthMul / 2) (lengthMul / 2)

halfVectorConfig :: VectorConfig -> VectorConfig
halfVectorConfig VectorConfig {..} =
  VectorConfig elementWidthMul (lengthMul / 2)

instance ScaleLMul VectorConfig where
  scaleLMul ratio (VectorConfig xmul lmul) =
    VectorConfig xmul $ scaleLMul ratio lmul

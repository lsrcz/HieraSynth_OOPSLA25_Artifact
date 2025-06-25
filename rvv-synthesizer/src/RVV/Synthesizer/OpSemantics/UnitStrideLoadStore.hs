{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.OpSemantics.UnitStrideLoadStore
  ( applyVLE,
    typeVLE,
    applyVSE,
    typeVSE,
  )
where

import Grisette (mrgReturn)
import HieraSynth.TypeSignature
  ( TypeSignature (TypeSignature, argTypes, resTypes),
  )
import Grisette.Unified (GetData)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Memory (BlockId)
import RVV.Semantics.PrimOp.UnitStrideLoadStore
  ( unitStrideLoad,
    unitStrideStore,
  )
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.VectorConfig
  ( VectorConfig (elementWidthMul),
    vectorMaskMul,
  )
import RVV.Synthesizer.OpSemantics.Util
  ( getVectorDestMaskAndValidateTotalNum,
    typeVectorOpWithDestMask,
  )
import RVV.Synthesizer.Parameter.Destination (Destination, ud)
import RVV.Synthesizer.Parameter.Masking (Masking)
import RVV.Synthesizer.Type
  ( ValueType (MemType, PtrType, VLType, VectorType),
  )
import RVV.Synthesizer.Value
  ( Value (MemValue, VectorValue),
    extractMemValue,
    extractPtrValue,
    extractVLValue,
    extractVectorValue,
  )

applyVLE ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  GetData mode Destination ->
  GetData mode Masking ->
  MachineConfig ->
  VectorConfig ->
  BlockId ->
  [Value mode] ->
  ctx [Value mode]
applyVLE
  wrappedDestination
  wrappedMasking
  vconst
  config
  blockId
  values = do
    (remainingValues, vd, vm) <-
      getVectorDestMaskAndValidateTotalNum
        "applyVLE"
        wrappedDestination
        wrappedMasking
        vconst
        config
        3
        values
    vl <- extractVLValue (head remainingValues)
    mem <- extractMemValue (remainingValues !! 1)
    ptr <- extractPtrValue (remainingValues !! 2)
    res <- unitStrideLoad vconst config vd vm mem blockId ptr vl
    mrgReturn [VectorValue res]

typeVLE ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  Int ->
  GetData mode Destination ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeVLE config blockId =
  typeVectorOpWithDestMask
    config
    ( TypeSignature
        { argTypes =
            [ VLType $ vectorMaskMul config,
              MemType,
              PtrType (elementWidthMul config) blockId
            ],
          resTypes = [VectorType config]
        }
    )

applyVSE ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  GetData mode Masking ->
  MachineConfig ->
  VectorConfig ->
  Int ->
  [Value mode] ->
  ctx [Value mode]
applyVSE
  wrappedMasking
  vconst
  config
  blockId
  values = do
    (remainingValues, _, vm) <-
      getVectorDestMaskAndValidateTotalNum
        "applyVSE"
        (ud @mode)
        wrappedMasking
        vconst
        config
        4
        values
    vl <- extractVLValue (head remainingValues)
    mem <- extractMemValue (remainingValues !! 1)
    ptr <- extractPtrValue (remainingValues !! 2)
    reg <- extractVectorValue (remainingValues !! 3)
    res <- unitStrideStore vconst config reg vm mem blockId ptr vl
    mrgReturn [MemValue res]

typeVSE ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  VectorConfig ->
  Int ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeVSE config blockId =
  typeVectorOpWithDestMask
    config
    ( TypeSignature
        { argTypes =
            [ VLType $ vectorMaskMul config,
              MemType,
              PtrType (elementWidthMul config) blockId,
              VectorType config
            ],
          resTypes = [MemType]
        }
    )
    (ud @mode)

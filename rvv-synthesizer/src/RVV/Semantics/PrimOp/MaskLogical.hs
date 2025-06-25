{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Semantics.PrimOp.MaskLogical (maskLogical, maskLogical') where

import Data.Bits (Bits ((.&.), (.|.)), FiniteBits (finiteBitSize))
import Grisette (BV (bv, bvConcat, bvSelect), mrgReturn)
import Grisette.Internal.Unified.UnifiedBV (GetSomeWordN)
import RVV.Semantics.Element
  ( MaskElement,
    decomposeMask,
    vlToMaskMask,
  )
import RVV.Semantics.MachineConfig
  ( MachineConfig (MachineConfig),
  )
import RVV.Semantics.Multiplier (MaskMul)
import RVV.Semantics.PrimOp.FullMask (fullMask)
import RVV.Semantics.PrimOp.Undefined (undefinedMask)
import RVV.Semantics.PrimOp.Util
  ( SemConstraint,
    handleMaskTailMasks,
    -- maskVLDataMask,
    -- maskVLUninitializedMask,
  )
import RVV.Semantics.SizeConstraint (validMask)
import RVV.Semantics.Value (Mask (Mask), VL (VL), VectorReg (VectorReg))
import RVV.Semantics.VectorConfig (maskNumValidElements)

maskLogical ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  (MaskElement mode -> MaskElement mode -> MaskElement mode) ->
  MachineConfig ->
  MaskMul ->
  Mask mode ->
  Mask mode ->
  VL mode ->
  ctx (Mask mode)
maskLogical elemFunc vconst@MachineConfig {..} maskMul lhs rhs vlreg@VL {..} = do
  lhsElements <- decomposeMask vconst lhs
  rhsElements <- decomposeMask vconst rhs
  let resElements :: [ctx (MaskElement mode)] =
        zipWith
          (\lhsElement rhsElement -> mrgReturn $ elemFunc lhsElement rhsElement)
          lhsElements
          rhsElements
  handleMaskTailMasks
    vconst
    vlreg
    (fullMask vconst maskMul)
    (undefinedMask vconst maskMul)
    resElements

maskLogical' ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  (GetSomeWordN mode -> GetSomeWordN mode -> GetSomeWordN mode) ->
  ( GetSomeWordN mode ->
    GetSomeWordN mode ->
    GetSomeWordN mode ->
    GetSomeWordN mode ->
    GetSomeWordN mode
  ) ->
  MachineConfig ->
  MaskMul ->
  Mask mode ->
  Mask mode ->
  VL mode ->
  ctx (Mask mode)
maskLogical'
  dataFunc
  uninitFunc
  vconst
  maskMul
  lhs@(Mask _ (VectorReg ld lu))
  rhs@(Mask _ (VectorReg rd ru))
  vlreg@VL {..} = do
    validMask vconst maskMul lhs
    validMask vconst maskMul rhs
    let num = maskNumValidElements vconst maskMul
    let get = bvSelect 0 num
    let recover n v =
          if num == finiteBitSize ld
            then v
            else bvConcat (bv (finiteBitSize ld - num) n) v
    let ld' = get ld
    let ru' = get ru
    let rd' = get rd
    let lu' = get lu
    let resd = dataFunc ld' rd'
    let resu = uninitFunc ld' ru' rd' lu'
    -- dataMask <- maskVLDataMask vconst maskMul vlreg
    -- uninitMask <- maskVLUninitializedMask vconst maskMul vlreg
    Mask _ (VectorReg dataMask uninitMask) <- vlToMaskMask vconst vlreg
    mrgReturn $
      Mask maskMul $
        VectorReg
          (recover 0 $ resd .&. dataMask)
          (recover (-1) $ resu .|. uninitMask)

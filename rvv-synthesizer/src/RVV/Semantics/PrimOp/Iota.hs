{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Semantics.PrimOp.Iota (viota, vid) where

import Grisette (BV (bv), LogicalOp ((.&&)), mrgReturn)
import Grisette.Unified (GetBool, symIte)
import RVV.Semantics.Element
  ( MaskElement (maskElementData),
    VectorElement (VectorElement, vectorElementData),
    decomposeMask,
    maskElementIsInitialized,
    vectorElementIsInitialized,
  )
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.PrimOp.Util (SemConstraint, handleVectorTailMasks)
import RVV.Semantics.SizeConstraint (validMask, validVL, validVector)
import RVV.Semantics.Value
  ( Mask,
    VL,
    Vector,
  )
import RVV.Semantics.VectorConfig
  ( VectorConfig,
    vectorElementBitWidth,
    vectorMaskMul,
  )

viota ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  Vector mode ->
  Mask mode ->
  Mask mode ->
  VL mode ->
  ctx (Vector mode)
viota vconst vectorConfig dest mask srcMask vl = do
  maskElements <- decomposeMask vconst mask
  srcMaskElements <- decomposeMask vconst srcMask
  let eew = vectorElementBitWidth vconst vectorConfig
  let initialElement = VectorElement (bv eew 0) (bv eew 0)
  let elems =
        scanl
          ( \prevElement (maskElement, srcMaskElement) -> do
              let operandsAreInitialized =
                    maskElementIsInitialized maskElement
                      .&& maskElementIsInitialized srcMaskElement
                      .&& vectorElementIsInitialized prevElement ::
                      GetBool mode
                  vectorElementValue =
                    symIte
                      operandsAreInitialized
                      ( vectorElementData prevElement
                          + symIte
                            (maskElementData maskElement .&& maskElementData srcMaskElement)
                            (bv eew 1)
                            (bv eew 0)
                      )
                      (bv eew 0)
                  vectorElementUnintialized =
                    symIte
                      operandsAreInitialized
                      (bv eew 0)
                      (bv eew (-1))
               in VectorElement vectorElementValue vectorElementUnintialized
          )
          (initialElement :: VectorElement mode)
          (zip maskElements srcMaskElements)
  handleVectorTailMasks vconst vl mask dest $ mrgReturn <$> init elems

vid ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  MachineConfig ->
  VectorConfig ->
  Vector mode ->
  Mask mode ->
  VL mode ->
  ctx (Vector mode)
vid vconst vectorConfig dest mask vl = do
  validVector vconst vectorConfig dest
  validMask vconst (vectorMaskMul vectorConfig) mask
  validVL vconst (vectorMaskMul vectorConfig) vl
  let eew = vectorElementBitWidth vconst vectorConfig
  let elems =
        (\v -> VectorElement v (bv eew 0))
          <$> iterate (+ bv eew 1) (bv eew 0)
  handleVectorTailMasks vconst vl mask dest $ mrgReturn <$> init elems

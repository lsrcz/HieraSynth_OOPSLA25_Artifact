{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Semantics.PrimOp.Merge (mergeVector) where

import GHC.Stack (HasCallStack)
import Grisette (BV (bv), mrgReturn)
import Grisette.Unified (mrgIf)
import RVV.Semantics.Element
  ( MaskElement (maskElementData),
    VectorElement (VectorElement),
    decomposeMask,
    decomposeVector,
    maskElementIsInitialized,
  )
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.PrimOp.FullMask (fullMask)
import RVV.Semantics.PrimOp.Util (SemConstraint, handleVectorTailMasks)
import RVV.Semantics.SizeConstraint (validMask)
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

mergeVector ::
  forall mode ctx.
  (SemConstraint mode ctx, HasCallStack) =>
  MachineConfig ->
  VectorConfig ->
  Vector mode ->
  Mask mode ->
  Vector mode ->
  Vector mode ->
  VL mode ->
  ctx (Vector mode)
mergeVector vconst vectorConfig dest mask false true vlreg = do
  validMask vconst (vectorMaskMul vectorConfig) mask
  maskElements :: [MaskElement mode] <- decomposeMask vconst mask
  trueElements <- decomposeVector vconst true
  falseElements <- decomposeVector vconst false
  let resElements :: [ctx (VectorElement mode)] =
        zipWith3
          ( \maskElement trueElement falseElement -> do
              mrgIf @mode
                (maskElementIsInitialized maskElement)
                ( mrgIf @mode
                    (maskElementData maskElement)
                    (mrgReturn trueElement)
                    (mrgReturn falseElement)
                )
                ( mrgReturn $
                    VectorElement
                      (bv (vectorElementBitWidth vconst vectorConfig) 0)
                      (bv (vectorElementBitWidth vconst vectorConfig) (-1))
                )
          )
          maskElements
          trueElements
          falseElements
  handleVectorTailMasks
    vconst
    vlreg
    (fullMask vconst (vectorMaskMul vectorConfig))
    dest
    resElements

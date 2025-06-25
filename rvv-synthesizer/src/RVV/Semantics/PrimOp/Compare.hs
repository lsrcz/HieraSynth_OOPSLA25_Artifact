{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Semantics.PrimOp.Compare (compareToMask) where

import Grisette (LogicalOp (symNot, (.&&)), ToSym (toSym), mrgReturn)
import Grisette.Unified (GetBool, GetSomeWordN, symIte)
import RVV.Semantics.Element
  ( MaskElement (MaskElement),
    VectorElement (vectorElementData),
    decomposeVector,
    vectorElementIsInitialized,
  )
import RVV.Semantics.MachineConfig (MachineConfig (MachineConfig))
import RVV.Semantics.PrimOp.Util (SemConstraint, handleMaskTailMasks)
import RVV.Semantics.SizeConstraint (validVector)
import RVV.Semantics.Value
  ( Mask,
    VL (VL),
    Vector,
  )
import RVV.Semantics.VectorConfig (VectorConfig)

compareToMask ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  (GetSomeWordN mode -> GetSomeWordN mode -> GetBool mode) ->
  MachineConfig ->
  VectorConfig ->
  Mask mode ->
  Mask mode ->
  Vector mode ->
  Vector mode ->
  VL mode ->
  ctx (Mask mode)
compareToMask elemFunc vconst@MachineConfig {..} vectorConfig dest mask lhs rhs vlreg@VL {..} = do
  validVector vconst vectorConfig lhs
  validVector vconst vectorConfig rhs
  lhsElements <- decomposeVector vconst lhs
  rhsElements <- decomposeVector vconst rhs
  let resElements :: [ctx (MaskElement mode)] =
        zipWith
          ( \lhsElement rhsElement -> do
              let operandsAreInitialized =
                    vectorElementIsInitialized lhsElement
                      .&& vectorElementIsInitialized rhsElement ::
                      GetBool mode
              let resultValue =
                    symIte
                      operandsAreInitialized
                      ( elemFunc
                          (vectorElementData lhsElement)
                          (vectorElementData rhsElement)
                      )
                      (toSym False)
              let resultUninitialized = symNot operandsAreInitialized
              mrgReturn $ MaskElement resultValue resultUninitialized
          )
          lhsElements
          rhsElements
  handleMaskTailMasks vconst vlreg mask dest resElements

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Semantics.PrimOp.ElementWise
  ( elementWise,
  )
where

import GHC.Stack (HasCallStack)
import Grisette (mrgReturn)
import RVV.Semantics.Element
  ( VectorElement,
    decomposeVector,
  )
import RVV.Semantics.MachineConfig
  ( MachineConfig (MachineConfig),
  )
import RVV.Semantics.PrimOp.Util (SemConstraint, handleVectorTailMasks)
import RVV.Semantics.SizeConstraint (validMask, validVL, validVector)
import RVV.Semantics.Value
  ( Mask,
    VL (VL),
    Vector,
  )
import RVV.Semantics.VectorConfig
  ( VectorConfig,
    vectorMaskMul,
  )
import RVV.Util.Context (assert)

elementWise ::
  forall mode ctx.
  (SemConstraint mode ctx, HasCallStack) =>
  (VectorElement mode -> VectorElement mode) ->
  (VectorElement mode -> VectorElement mode) ->
  (VectorElement mode -> VectorElement mode) ->
  (VectorElement mode -> VectorElement mode -> ctx (VectorElement mode)) ->
  MachineConfig ->
  VectorConfig ->
  VectorConfig ->
  VectorConfig ->
  Vector mode ->
  Mask mode ->
  Vector mode ->
  Vector mode ->
  VL mode ->
  ctx (Vector mode)
elementWise
  transformLhs
  transformRhs
  transformRes
  elemFunc
  vconst@MachineConfig {..}
  lhsVectorConfig
  rhsVectorConfig
  destVectorConfig
  dest
  mask
  lhs
  rhs
  vlreg@VL {..} = do
    assert "arg and result should have the same maskMul" $
      vectorMaskMul lhsVectorConfig == vectorMaskMul destVectorConfig
    assert "arg and result should have the same maskMul" $
      vectorMaskMul rhsVectorConfig == vectorMaskMul destVectorConfig
    validVector vconst lhsVectorConfig lhs
    validVector vconst rhsVectorConfig rhs
    validVector vconst destVectorConfig dest
    validMask vconst (vectorMaskMul lhsVectorConfig) mask
    validVL vconst (vectorMaskMul lhsVectorConfig) vlreg
    lhsElements <- decomposeVector vconst lhs
    rhsElements <- decomposeVector vconst rhs
    let resElements :: [ctx (VectorElement mode)] =
          zipWith
            ( \lhsElement rhsElement -> do
                resultValue <-
                  elemFunc (transformLhs lhsElement) (transformRhs rhsElement)
                mrgReturn $ transformRes resultValue
            )
            lhsElements
            rhsElements
    handleVectorTailMasks vconst vlreg mask dest resElements

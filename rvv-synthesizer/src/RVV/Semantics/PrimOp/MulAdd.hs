{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Semantics.PrimOp.MulAdd (mulAdd) where

import Grisette (BV (bv), LogicalOp ((.&&)), mrgReturn)
import Grisette.Unified (GetBool, GetSomeWordN, mrgIf, symIte)
import RVV.Semantics.Element
  ( VectorElement (VectorElement, vectorElementData),
    decomposeVector,
    vectorElementIsInitialized,
  )
import RVV.Semantics.MachineConfig (MachineConfig (MachineConfig))
import RVV.Semantics.PrimOp.Util (SemConstraint, handleVectorTailMasks)
import RVV.Semantics.SizeConstraint (validMask, validVL, validVector)
import RVV.Semantics.Value (Mask, VL (VL), Vector)
import RVV.Semantics.VectorConfig
  ( VectorConfig,
    vectorElementBitWidth,
    vectorMaskMul,
  )
import RVV.Util.Context (assert)

mulAdd ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  (GetSomeWordN mode -> GetSomeWordN mode) ->
  (GetSomeWordN mode -> GetSomeWordN mode) ->
  (GetSomeWordN mode -> GetSomeWordN mode -> GetSomeWordN mode -> ctx (GetSomeWordN mode)) ->
  MachineConfig ->
  VectorConfig ->
  VectorConfig ->
  Vector mode ->
  Mask mode ->
  Vector mode ->
  Vector mode ->
  VL mode ->
  ctx (Vector mode)
mulAdd
  transformLhs
  transformRhs
  elemFunc
  vconst@MachineConfig {..}
  argVectorConfig
  resVectorConfig
  dest
  mask
  lhs
  rhs
  vlreg@VL {..} = do
    assert "arg and result should have the same maskMul" $
      vectorMaskMul argVectorConfig == vectorMaskMul resVectorConfig
    validVector vconst argVectorConfig lhs
    validVector vconst argVectorConfig rhs
    validVector vconst resVectorConfig dest
    validMask vconst (vectorMaskMul argVectorConfig) mask
    validVL vconst (vectorMaskMul argVectorConfig) vlreg
    destElements <- decomposeVector vconst dest
    lhsElements <- decomposeVector vconst lhs
    rhsElements <- decomposeVector vconst rhs
    let resElements :: [ctx (VectorElement mode)] =
          zipWith3
            ( \destElement lhsElement rhsElement -> do
                let operandsAreInitialized =
                      vectorElementIsInitialized destElement
                        .&& vectorElementIsInitialized lhsElement
                        .&& vectorElementIsInitialized rhsElement ::
                        GetBool mode
                resultValue <-
                  mrgIf
                    operandsAreInitialized
                    ( elemFunc
                        (vectorElementData destElement)
                        (transformLhs $ vectorElementData lhsElement)
                        (transformRhs $ vectorElementData rhsElement)
                    )
                    (return $ bv (vectorElementBitWidth vconst resVectorConfig) 0)
                let resultUninitialized =
                      symIte @mode
                        operandsAreInitialized
                        (bv (vectorElementBitWidth vconst resVectorConfig) 0)
                        (bv (vectorElementBitWidth vconst resVectorConfig) $ -1)
                mrgReturn $ VectorElement resultValue resultUninitialized
            )
            destElements
            lhsElements
            rhsElements
    handleVectorTailMasks vconst vlreg mask dest resElements

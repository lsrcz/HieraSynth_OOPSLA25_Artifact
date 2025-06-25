{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.OpSemantics.Slide
  ( applySlide,
    typeSlideVX,
    typeSlide1VX,
    typeSlideVI,
    typeSlide1VI,
  )
where

import Grisette (mrgReturn)
import HieraSynth.TypeSignature
  ( TypeSignature (TypeSignature, argTypes, resTypes),
  )
import Grisette.Unified (GetData, extractData)
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.PrimOp.Slide (slide1down, slide1up, slidedown, slideup)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.VectorConfig
  ( VectorConfig (elementWidthMul),
    vectorMaskMul,
  )
import RVV.Synthesizer.OpSemantics.Util
  ( getVectorDestMaskAndValidateTotalNum,
    typeVectorOpWithDestMask,
  )
import RVV.Synthesizer.Parameter.Destination (Destination)
import RVV.Synthesizer.Parameter.Masking (Masking)
import RVV.Synthesizer.Parameter.SlideDirection
  ( SlideDirection (SlideDown, SlideUp),
  )
import RVV.Synthesizer.Type
  ( ValueType (ScalarType, VLType, VectorType),
  )
import RVV.Synthesizer.Value
  ( Value (VectorValue),
    extractScalarValue,
    extractVLValue,
    extractVectorValue,
  )

applySlide ::
  forall mode ctx.
  (SemConstraint mode ctx) =>
  Bool ->
  MachineConfig ->
  VectorConfig ->
  GetData mode SlideDirection ->
  GetData mode Destination ->
  GetData mode Masking ->
  [Value mode] ->
  ctx [Value mode]
applySlide
  isSlide1
  vconst
  config
  wrappedDir
  wrappedDestination
  wrappedMasking
  values = do
    (remainingValues, vd, vm) <-
      getVectorDestMaskAndValidateTotalNum
        "slide"
        wrappedDestination
        wrappedMasking
        vconst
        config
        3
        values
    vl <- extractVLValue (head remainingValues)
    src <- extractVectorValue (remainingValues !! 1)
    scalar <- extractScalarValue (remainingValues !! 2)
    dir <- extractData wrappedDir
    res <-
      ( case (dir, isSlide1) of
          (SlideUp, False) -> slideup
          (SlideUp, True) -> slide1up
          (SlideDown, False) -> slidedown
          (SlideDown, True) -> slide1down
        )
        vconst
        config
        vd
        vm
        src
        scalar
        vl
    mrgReturn [VectorValue res]

typeSlideVX ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Destination ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeSlideVX config =
  typeVectorOpWithDestMask
    config
    ( TypeSignature
        { argTypes =
            [ VLType $ vectorMaskMul config,
              VectorType config,
              ScalarType 1
            ],
          resTypes = [VectorType config]
        }
    )

typeSlide1VX ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Destination ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeSlide1VX config =
  typeVectorOpWithDestMask
    config
    ( TypeSignature
        { argTypes =
            [ VLType $ vectorMaskMul config,
              VectorType config,
              ScalarType $ elementWidthMul config
            ],
          resTypes = [VectorType config]
        }
    )

typeSlideVI ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Destination ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeSlideVI config =
  typeVectorOpWithDestMask
    config
    ( TypeSignature
        { argTypes =
            [VLType $ vectorMaskMul config, VectorType config],
          resTypes = [VectorType config]
        }
    )

typeSlide1VI ::
  (SemConstraint mode ctx) =>
  VectorConfig ->
  GetData mode Destination ->
  GetData mode Masking ->
  ctx (TypeSignature ValueType)
typeSlide1VI config =
  typeVectorOpWithDestMask
    config
    ( TypeSignature
        { argTypes =
            [ VLType $ vectorMaskMul config,
              VectorType config
            ],
          resTypes = [VectorType config]
        }
    )

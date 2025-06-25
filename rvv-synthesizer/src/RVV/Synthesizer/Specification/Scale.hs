{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RVV.Synthesizer.Specification.Scale
  ( baseConfigScaleRatio,
    vconstScaleRatio,
    fullImm,
    scaleMachineConfig,
    fullImmScaleMachineConfig,
    zextUpscaleFun,
    sextUpscaleFun,
    scaleVector,
    scaleMask,
    downscaleVector,
    scaleValue,
    downscaleSpec,
    downscaleSpec',
  )
where

import Control.Monad (when, zipWithM)
import Data.Bits (FiniteBits (finiteBitSize))
import qualified Data.HashMap.Lazy as HM
import Data.Ratio (Ratio, denominator, numerator, (%))
import Grisette (BV (bv, bvConcat, bvSelect, bvSext, bvZext), mrgReturn)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import HieraSynth.Context (ConcreteContext, MonadContext)
import HieraSynth.Util.Show (showAsText)
import Grisette.Unified (GetSomeWordN)
import RVV.EvalMode (EvalMode)
import RVV.Semantics.BitSizeUtil (isPower2)
import RVV.Semantics.Element
  ( VectorElement (VectorElement),
    composeVector,
    decomposeVector,
  )
import RVV.Semantics.MachineConfig
  ( AllowPartialVL,
    MachineBaseConfig
      ( MachineBaseConfig,
        machineMemoryBlockIds,
        machinePointerUnit,
        machineScalarImmLength,
        machineScalarLength,
        machineVectorImmLength
      ),
    MachineConfig (MachineConfig, allowPartialVL, baseConfig, scalableConfig, useVLMask),
    MachineScalableConfig
      ( MachineScalableConfig,
        machineMemoryBlockLengths,
        machineVectorLength
      ),
  )
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Semantics.SizeConstraint (validMachineBaseConfig, validMachineConfig)
import RVV.Semantics.Value
  ( Mask (Mask),
    Scalar (Scalar),
    Vector (vectorConfig),
    VectorReg (VectorReg),
  )
import RVV.Semantics.VectorConfig (vectorElementBitWidth)
import RVV.Synthesizer.Matcher (RegListMatcher)
import RVV.Synthesizer.Value (Value (MaskValue, ScalarValue, VectorValue))
import RVV.Util.Context (assert)

baseConfigScaleRatio ::
  (MonadContext ctx) => Bool -> MachineBaseConfig -> MachineBaseConfig -> ctx (Ratio Int)
baseConfigScaleRatio haveMemory target source = do
  validMachineBaseConfig target
  validMachineBaseConfig source
  let xlenRatio = machineScalarLength target % machineScalarLength source
  assert "different memory block ids" $
    machineMemoryBlockIds target == machineMemoryBlockIds source
  when haveMemory
    $ assert
      ( "Different ptr unit ratio. expected ratio: "
          <> showAsText xlenRatio
          <> ", but got target: "
          <> showAsText (machinePointerUnit target)
          <> ", src: "
          <> showAsText (machinePointerUnit source)
      )
    $ machinePointerUnit target % machinePointerUnit source
      == xlenRatio
  return xlenRatio

vconstScaleRatio :: Bool -> MachineConfig -> MachineConfig -> ConcreteContext (Ratio Int)
vconstScaleRatio haveMemory target source = do
  validMachineConfig target
  validMachineConfig source
  ratio <- baseConfigScaleRatio haveMemory (baseConfig target) (baseConfig source)
  assert "different vlen ratio" $
    machineVectorLength (scalableConfig target) % machineVectorLength (scalableConfig source)
      == ratio
  sequence_ $
    HM.mapWithKey
      ( \k a -> do
          let b = machineMemoryBlockLengths (scalableConfig source) HM.! k
          assert "vconstRatio: memory block length ratio mismatch" $
            a % b == ratio
      )
      (machineMemoryBlockLengths $ scalableConfig target)
  return ratio

validScaleRatio :: (MonadContext ctx) => Ratio Int -> ctx ()
validScaleRatio ratio =
  assert "Scale isn't power of 2" $
    isPower2 n && isPower2 d && (n == 1 || d == 1)
  where
    n = numerator ratio
    d = denominator ratio

scaleInt :: (MonadContext ctx) => Bool -> Ratio Int -> Int -> ctx Int
scaleInt ceilingToOne ratio x = do
  validScaleRatio ratio
  let scaled = fromIntegral x * ratio
  if denominator scaled == 1
    then return $ numerator scaled
    else
      if ceilingToOne
        then mrgReturn 1
        else mrgThrowError "scaleInt: bad ratio"

fullImm :: (MonadContext ctx) => Ratio Int -> MachineConfig -> Int -> ctx Int
fullImm ratio MachineConfig {baseConfig = MachineBaseConfig {..}} _ = do
  validScaleRatio ratio
  scaleInt False ratio machineScalarLength

scaleMachineScalableConfig ::
  (MonadContext ctx) =>
  Ratio Int ->
  MachineScalableConfig ->
  ctx MachineScalableConfig
scaleMachineScalableConfig ratio MachineScalableConfig {..} = do
  newVlen <- scaleInt False ratio machineVectorLength
  newMachineConfigMemoryBlockLengths <-
    traverse (scaleInt False ratio) machineMemoryBlockLengths
  return $
    MachineScalableConfig
      { machineVectorLength = newVlen,
        machineMemoryBlockLengths = newMachineConfigMemoryBlockLengths
      }

scaleMachineConfig ::
  (MonadContext ctx) =>
  Bool ->
  (Ratio Int -> MachineConfig -> Int -> ctx Int) ->
  (Ratio Int -> MachineConfig -> Int -> ctx Int) ->
  Ratio Int ->
  MachineConfig ->
  ctx MachineConfig
scaleMachineConfig
  haveMemory
  scaleXImm
  scaleVImm
  scale
  vconst@MachineConfig
    { baseConfig = MachineBaseConfig {..},
      scalableConfig,
      useVLMask,
      allowPartialVL
    } = do
    assert "Scale isn't power of 2" $
      isPower2 n && isPower2 d && (n == 1 || d == 1)
    validMachineConfig vconst
    newXlen <- scaleInt False scale machineScalarLength
    newPtrUnit <- scaleInt (not haveMemory) scale machinePointerUnit
    newXImmLen <- scaleXImm scale vconst machineScalarImmLength
    newVImmLen <- scaleVImm scale vconst machineVectorImmLength
    newMachineScalableConfig <- scaleMachineScalableConfig scale scalableConfig
    let result =
          MachineConfig
            { baseConfig =
                MachineBaseConfig
                  { machineScalarLength = newXlen,
                    machineMemoryBlockIds = machineMemoryBlockIds,
                    machinePointerUnit = newPtrUnit,
                    machineScalarImmLength = newXImmLen,
                    machineVectorImmLength = newVImmLen
                  },
              scalableConfig = newMachineScalableConfig,
              useVLMask,
              allowPartialVL
            }
    validMachineConfig result
    return result
    where
      n = numerator scale
      d = denominator scale

fullImmScaleMachineConfig :: Bool -> Ratio Int -> MachineConfig -> ConcreteContext MachineConfig
fullImmScaleMachineConfig haveMemory = scaleMachineConfig haveMemory fullImm fullImm

zextUpscaleFun ::
  (EvalMode mode) => Ratio Int -> GetSomeWordN mode -> GetSomeWordN mode
zextUpscaleFun ratio x = bvZext (numerator ratio * finiteBitSize x) x

sextUpscaleFun ::
  (EvalMode mode) => Ratio Int -> GetSomeWordN mode -> GetSomeWordN mode
sextUpscaleFun ratio x = bvSext (numerator ratio * finiteBitSize x) x

scaleScalar ::
  (SemConstraint mode ctx) =>
  (Ratio Int -> GetSomeWordN mode -> GetSomeWordN mode) ->
  Ratio Int ->
  Scalar mode ->
  ctx (Scalar mode)
scaleScalar elemUpscaleFun ratio (Scalar d u) = do
  validScaleRatio ratio
  let isUpScale = numerator ratio > denominator ratio
   in if isUpScale
        then return $ Scalar (elemUpscaleFun ratio d) u
        else
          return $
            Scalar (bvSelect 0 (finiteBitSize d `div` denominator ratio) d) u

scaleVector ::
  (SemConstraint mode ctx) =>
  (Ratio Int -> GetSomeWordN mode -> GetSomeWordN mode) ->
  (Ratio Int -> MachineConfig -> ctx MachineConfig) ->
  Ratio Int ->
  MachineConfig ->
  Vector mode ->
  ctx (Vector mode)
scaleVector elemUpscaleFun vconstScaleFun ratio srcMachineConfig src = do
  targetMachineConfig <- vconstScaleFun ratio srcMachineConfig
  aelems <- decomposeVector srcMachineConfig src
  let isUpScale = numerator ratio > denominator ratio
  let scaleFun x =
        if isUpScale
          then elemUpscaleFun ratio x
          else
            bvSelect 0 (vectorElementBitWidth targetMachineConfig $ vectorConfig src) x
  let castVectorElement (VectorElement d u) =
        VectorElement (scaleFun d) (scaleFun u)
  composeVector targetMachineConfig (vectorConfig src) $ castVectorElement <$> aelems

downscaleVector ::
  (SemConstraint mode ctx) =>
  (Ratio Int -> MachineConfig -> ctx MachineConfig) ->
  Ratio Int ->
  MachineConfig ->
  Vector mode ->
  ctx (Vector mode)
downscaleVector = scaleVector (error "downscaleVector: not downscaling")

scaleMask ::
  (SemConstraint mode ctx) =>
  (Ratio Int -> MachineConfig -> ctx MachineConfig) ->
  Ratio Int ->
  MachineConfig ->
  Mask mode ->
  ctx (Mask mode)
scaleMask
  vconstScaleFun
  ratio
  srcMachineConfig@(MachineConfig {scalableConfig = srcConfig})
  (Mask n (VectorReg reg invalid)) = do
    MachineConfig {scalableConfig = targetConfig} <- vconstScaleFun ratio srcMachineConfig
    let targetVlen = machineVectorLength targetConfig
    let isUpScale = numerator ratio > denominator ratio
    if isUpScale
      then
        return $
          Mask
            n
            ( VectorReg
                (bvZext targetVlen reg)
                ( bvConcat
                    (bv (targetVlen - machineVectorLength srcConfig) 0)
                    invalid
                )
            )
      else
        return $
          Mask
            n
            ( VectorReg
                (bvSelect 0 targetVlen reg)
                (bvSelect 0 targetVlen invalid)
            )

scaleValue ::
  (SemConstraint mode ctx) =>
  (Ratio Int -> GetSomeWordN mode -> GetSomeWordN mode) ->
  (Ratio Int -> MachineConfig -> ctx MachineConfig) ->
  Ratio Int ->
  MachineConfig ->
  Value mode ->
  ctx (Value mode)
scaleValue elemUpscaleFun vconstScaleFun ratio srcMachineConfig v = do
  case v of
    VectorValue vec -> do
      r <- scaleVector elemUpscaleFun vconstScaleFun ratio srcMachineConfig vec
      return $ VectorValue r
    ScalarValue s -> ScalarValue <$> scaleScalar elemUpscaleFun ratio s
    MaskValue mask -> do
      r <- scaleMask vconstScaleFun ratio srcMachineConfig mask
      return $ MaskValue r
    _ -> error "Not implemented"

downscaleSpec ::
  (SemConstraint mode ctx) =>
  Bool ->
  [Ratio Int -> MachineConfig -> Value mode -> ctx (Value mode)] ->
  MachineBaseConfig ->
  Bool ->
  AllowPartialVL ->
  (MachineConfig -> [Value mode] -> ctx ([Value mode], RegListMatcher)) ->
  (MachineConfig -> [Value mode] -> ctx ([Value mode], RegListMatcher))
downscaleSpec
  haveMemory
  valueScaleFuns
  originalMachineBaseConfig
  useVLMask
  allowPartialVL
  originalSpec
  newMachineConfig
  inputs = do
    ratio <- baseConfigScaleRatio haveMemory (baseConfig newMachineConfig) originalMachineBaseConfig
    originalConfig <- scaleMachineScalableConfig (recip ratio) (scalableConfig newMachineConfig)
    let originalMachineConfig =
          MachineConfig
            { baseConfig = originalMachineBaseConfig,
              scalableConfig = originalConfig,
              useVLMask,
              allowPartialVL
            }
    scaledInputs <-
      zipWithM (\f -> f (1 / ratio) newMachineConfig) valueScaleFuns inputs
    (results, matcher) <- originalSpec originalMachineConfig scaledInputs
    rescaledOutputs <-
      zipWithM (\f -> f ratio originalMachineConfig) valueScaleFuns results
    return (rescaledOutputs, matcher)

downscaleSpec' ::
  (SemConstraint mode ctx) =>
  Bool ->
  (Ratio Int -> MachineConfig -> Value mode -> ctx (Value mode)) ->
  MachineBaseConfig ->
  Bool ->
  AllowPartialVL ->
  (MachineConfig -> [Value mode] -> ctx ([Value mode], RegListMatcher)) ->
  (MachineConfig -> [Value mode] -> ctx ([Value mode], RegListMatcher))
downscaleSpec' haveMemory valueScaleFun =
  downscaleSpec haveMemory (repeat valueScaleFun)

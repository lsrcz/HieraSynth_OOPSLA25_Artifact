{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module RVV.Synthesizer.Generator
  ( randomValidVectorElement,
    randomVectorElement,
    randomVectorWithElementGen,
    randomValidVector,
    randomVector,
    randomValidMask,
    randomMask,
    randomValidScalar,
    -- randomValidFullScalar,
    randomValidMemory,
    randomValidPtr,
    randomValidInput,
    randomValidInputs,
    randomValidInputsForProg,
    randomValidInputsForSymbol,
  )
where

import Data.Ratio (denominator, numerator)
import qualified Data.Text as T
import Grisette (BV (bv), arbitraryBV)
import HieraSynth.Program.ProgTyping (ProgTyping (typeProg))
import HieraSynth.Program.ProgUtil (ProgUtil (ProgTypeType))
import HieraSynth.Program.SymbolTable (SymbolTable, lookupSymbol)
import HieraSynth.TypeSignature (TypeSignature (argTypes))
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Element (VectorElement (VectorElement), composeVector)
-- FullScalarType,

import RVV.Semantics.MachineConfig
  ( MachineBaseConfig
      ( MachineBaseConfig,
        machineScalarLength
      ),
    MachineConfig
      ( MachineConfig,
        baseConfig,
        scalableConfig
      ),
    MachineScalableConfig
      ( MachineScalableConfig,
        machineMemoryBlockLengths,
        machineVectorLength
      ),
  )
import RVV.Semantics.Memory
  ( Memory (Memory),
    MemoryBlock (MemoryBlock),
    Ptr (Ptr),
  )
import RVV.Semantics.Multiplier (MaskMul, WidthMul (getWidthMul))
import RVV.Semantics.Value
  ( Mask (Mask),
    Scalar (Scalar),
    Vector,
    VectorReg (VectorReg),
  )
import RVV.Semantics.VectorConfig
  ( VectorConfig,
    vectorElementBitWidth,
    vectorNumAllElements,
  )
import RVV.Synthesizer.Type
  ( ValueType
      ( MaskType,
        MemType,
        PtrType,
        ScalarType,
        VLType,
        VectorType
      ),
  )
import RVV.Synthesizer.Value
  ( Value (MaskValue, MemValue, ScalarValue, VectorValue),
  )
import Test.QuickCheck.Counterexamples
  ( Arbitrary (arbitrary),
    Gen,
    oneof,
    vectorOf,
  )

randomValidVectorElement :: Int -> Gen (VectorElement 'C)
randomValidVectorElement n = do
  d <- arbitraryBV n
  return $ VectorElement d (bv n 0)

randomVectorElement :: Int -> Gen (VectorElement 'C)
randomVectorElement n = do
  d <- arbitraryBV n
  uninitialized <- arbitrary
  return $ VectorElement d (bv n (if uninitialized then -1 else 0))

randomVectorWithElementGen ::
  (Int -> Gen (VectorElement 'C)) -> MachineConfig -> VectorConfig -> Gen (Vector 'C)
randomVectorWithElementGen elemGen vconst vectorConfig = do
  let numElements = vectorNumAllElements vconst vectorConfig
  allElements <- vectorOf numElements (elemGen $ vectorElementBitWidth vconst vectorConfig)
  let grouped = composeVector vconst vectorConfig allElements
  case grouped of
    Left err ->
      error $ "BUG: failed to compose the value, error: " <> T.unpack err
    Right v -> return v

randomValidVector :: MachineConfig -> VectorConfig -> Gen (Vector 'C)
randomValidVector = randomVectorWithElementGen randomValidVectorElement

randomVector :: MachineConfig -> VectorConfig -> Gen (Vector 'C)
randomVector = randomVectorWithElementGen randomVectorElement

randomValidMask :: MachineConfig -> MaskMul -> Gen (Mask 'C)
randomValidMask MachineConfig {scalableConfig = MachineScalableConfig {..}} maskMul = do
  mask <- arbitraryBV machineVectorLength
  return $ Mask maskMul $ VectorReg mask (bv machineVectorLength 0)

randomMask :: MachineConfig -> MaskMul -> Gen (Mask 'C)
randomMask MachineConfig {scalableConfig = MachineScalableConfig {..}} maskMul = do
  mask <- arbitraryBV machineVectorLength
  uninitialized <- arbitraryBV machineVectorLength
  return $ Mask maskMul $ VectorReg mask uninitialized

randomValidScalar :: MachineConfig -> WidthMul -> Gen (Scalar 'C)
randomValidScalar MachineConfig {baseConfig = MachineBaseConfig {..}} xmul = do
  let bitWidth = fromIntegral machineScalarLength * getWidthMul xmul
  if denominator bitWidth == 1
    then Scalar <$> arbitraryBV (numerator bitWidth) <*> return False
    else error "randomValidScalar: invalid bit width"

-- randomValidFullScalar :: MachineConfig -> Gen (Scalar 'C)
-- randomValidFullScalar vconst = randomValidScalar vconst 1

randomValidMemory :: MachineConfig -> Gen (Memory 'C)
randomValidMemory MachineConfig {scalableConfig = MachineScalableConfig {..}} = do
  Memory
    <$> traverse
      ( \len -> do
          memoryData <- arbitraryBV len
          return $ MemoryBlock memoryData $ bv len 0
      )
      machineMemoryBlockLengths

randomValidPtr :: MachineConfig -> WidthMul -> [Int] -> [Integer] -> Gen (Ptr 'C)
randomValidPtr MachineConfig {baseConfig = MachineBaseConfig {..}} xmul blockIds offsets = do
  blockId <- oneof $ return <$> blockIds
  offset <- oneof $ return <$> offsets
  return $ Ptr xmul blockId (bv machineScalarLength offset) False

randomValidInput ::
  MachineConfig -> ValueType -> Gen (Value 'C)
randomValidInput vconst (VectorType vtype) =
  VectorValue <$> randomValidVector vconst vtype
randomValidInput vconst (MaskType maskMul) =
  MaskValue <$> randomValidMask vconst maskMul
-- randomValidInput vconst FullScalarType =
--   ScalarValue <$> randomValidFullScalar vconst
randomValidInput vconst (ScalarType xmul) =
  ScalarValue <$> randomValidScalar vconst xmul
randomValidInput _ PtrType {} =
  error "randomValidInput: PtrType is not supported"
randomValidInput vconst MemType =
  MemValue <$> randomValidMemory vconst
randomValidInput _ VLType {} =
  error "randomValidInput: VLType is not supported"

randomValidInputs ::
  MachineConfig -> [ValueType] -> Gen [Value 'C]
randomValidInputs vconst = traverse (randomValidInput vconst)

randomValidInputsForProg ::
  (ProgTyping prog, ProgTypeType prog ~ ValueType) =>
  MachineConfig ->
  prog ->
  Gen [Value 'C]
randomValidInputsForProg vconst prog =
  randomValidInputs vconst $ argTypes $ typeProg prog

randomValidInputsForSymbol ::
  (ProgTyping prog, ProgTypeType prog ~ ValueType) =>
  MachineConfig ->
  SymbolTable prog ->
  T.Text ->
  Gen [Value 'C]
randomValidInputsForSymbol vconst symTable symbol =
  let Right prog = lookupSymbol symTable symbol
   in randomValidInputsForProg vconst prog

{-# LANGUAGE DataKinds #-}

module RVV.Synthesizer.GeneratorTest (generatorTest) where

import Data.Bits (FiniteBits (finiteBitSize))
import Data.Foldable (traverse_)
import qualified Data.HashMap.Lazy as M
import Data.List (sort)
import Grisette (BV (bv), LogicalOp (false))
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Element (VectorElement (VectorElement))
import RVV.Semantics.Memory
  ( Memory (Memory),
    MemoryBlock (MemoryBlock),
    Ptr (Ptr),
  )
import RVV.Semantics.Value
  ( Mask (Mask),
    Scalar (Scalar),
    Vector (Vector),
    VectorReg (VectorReg),
  )
import RVV.Semantics.VectorConfigConstants (vconst8162, vconstWithMem, vectorConfigEF2M2)
import RVV.Synthesizer.Generator
  ( randomMask,
    randomValidMask,
    randomValidMemory,
    randomValidPtr,
    randomValidScalar,
    randomValidVector,
    randomValidVectorElement,
    randomVector,
    randomVectorElement,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (Assertion, assertBool, (@?=))
import Test.QuickCheck (forAll, ioProperty)

isValidVectorReg :: VectorReg 'C -> Assertion
isValidVectorReg (VectorReg d uninitialized) = do
  finiteBitSize d @?= 16
  uninitialized @?= bv 16 0

isVectorRegWithCorrectBitSize :: VectorReg 'C -> Assertion
isVectorRegWithCorrectBitSize (VectorReg d uninitialized) = do
  finiteBitSize d @?= 16
  finiteBitSize uninitialized @?= 16

generatorTest :: Test
generatorTest =
  testGroup
    "Generator"
    [ testProperty "randomValidVectorElement" $
        forAll (randomValidVectorElement 4) $
          \(VectorElement d uninitialized) -> ioProperty $ do
            finiteBitSize d @?= 4
            uninitialized @?= bv 4 0,
      testProperty "randomVectorElement" $
        forAll (randomVectorElement 4) $
          \(VectorElement d uninitialized) -> ioProperty $ do
            finiteBitSize d @?= 4
            assertBool "Must be fully initialized or uninitialized" $
              uninitialized `elem` [bv 4 0, bv 4 (-1)],
      testProperty "randomVector" $
        forAll (randomVector vconst8162 vectorConfigEF2M2) $
          \(Vector vtype vregs) -> ioProperty $ do
            vtype @?= vectorConfigEF2M2
            length vregs @?= 2
            traverse_ isVectorRegWithCorrectBitSize vregs,
      testProperty "randomValidVector" $
        forAll (randomValidVector vconst8162 vectorConfigEF2M2) $
          \(Vector vtype vregs) -> ioProperty $ do
            vtype @?= vectorConfigEF2M2
            length vregs @?= 2
            traverse_ isValidVectorReg vregs,
      testProperty "randomValidMask" $
        forAll (randomValidMask vconst8162 2) $
          \(Mask ratio vmask) -> ioProperty $ do
            ratio @?= 2
            isValidVectorReg vmask,
      testProperty "randomMask" $
        forAll (randomMask vconst8162 2) $
          \(Mask ratio vmask) -> ioProperty $ do
            ratio @?= 2
            isVectorRegWithCorrectBitSize vmask,
      testProperty "randomValidScalar" $
        forAll (randomValidScalar vconst8162 (1 / 2)) $
          \(Scalar v u) -> ioProperty $ do
            finiteBitSize v @?= 4
            u @?= false,
      testProperty "randomValidMemory"
        $ forAll
          ( randomValidMemory
              (vconstWithMem vconst8162 $ M.fromList [(0, 40), (1, 20)])
          )
        $ \(Memory m) -> ioProperty $ do
          sort (M.keys m) @?= [0, 1]
          case m M.! 0 of
            MemoryBlock d uninitialized -> do
              finiteBitSize d @?= 40
              uninitialized @?= bv 40 0
          case m M.! 1 of
            MemoryBlock d uninitialized -> do
              finiteBitSize d @?= 20
              uninitialized @?= bv 20 0,
      testProperty "randomValidPtr" $
        forAll (randomValidPtr vconst8162 4 [1, 2] [3, 4]) $
          \(Ptr width blockId offset u) -> ioProperty $ do
            width @?= 4
            assertBool "blockId must come from the chosen ones" $
              blockId `elem` [1, 2]
            assertBool "offset must come from the chosen ones" $
              offset `elem` [bv 8 3, bv 8 4]
            u @?= false
    ]

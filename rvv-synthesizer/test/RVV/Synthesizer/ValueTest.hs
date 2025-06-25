{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.ValueTest (valueTest) where

import qualified Data.HashMap.Lazy as M
import Grisette
  ( BV (bv),
    LogicalOp (false),
    Solvable (isym),
    isymBV,
    runFreshT,
    tryMerge,
  )
import Grisette.Lib.Base (mrgReturn)
import Grisette.Lib.Control.Monad.Except (mrgModifyError)
import HieraSynth.Context (SymbolicContext)
import HieraSynth.Program.ComponentSketch
  ( GenIntermediate (genIntermediate),
  )
import Grisette.Unified (EvalModeTag (C, S))
import RVV.Semantics.Memory
  ( Memory (Memory),
    MemoryBlock (MemoryBlock),
    Ptr (Ptr),
  )
import RVV.Semantics.Policy (muPolicy, symPolicy)
import RVV.Semantics.SizeConstraint (validVL)
import RVV.Semantics.Value
  ( Mask (Mask),
    Scalar (Scalar),
    VL (VL),
    VLValue (VLNum),
    Vector (Vector),
    VectorReg (VectorReg),
  )
import RVV.Semantics.VectorConfigConstants
  ( vconst8162,
    vconstWithMem,
    vectorConfigEF2M1,
    vectorConfigEF2M2,
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
  ( Value (MaskValue, MemValue, PtrValue, ScalarValue, VLValue, VectorValue),
    extractMaskValue,
    extractMemValue,
    extractPtrValue,
    extractScalarValue,
    extractVLValue,
    extractVectorValue,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import TestUtil.SymbolicAssertion ((.@?=))

valueTest :: Test
valueTest =
  testGroup
    "Value"
    [ testGroup
        "ExtractValue"
        [ testGroup
            "Value"
            [ testCase "extractVLValue" $ do
                let vl = VL 2 (VLNum (bv 8 0)) (muPolicy @'C) false
                let vlvalue = VLValue vl :: Value 'C
                let actual = extractVLValue vlvalue
                actual @?= Right vl,
              testCase "extractVectorValue" $ do
                let v = Vector vectorConfigEF2M1 [VectorReg (bv 8 0) (bv 8 0)]
                let vvalue = VectorValue v :: Value 'C
                let actual = extractVectorValue vvalue
                actual @?= Right v,
              testCase "extractMaskValue" $ do
                let v = Mask 1 $ VectorReg (bv 8 0) (bv 8 0)
                let vvalue = MaskValue v :: Value 'C
                let actual = extractMaskValue vvalue
                actual @?= Right v,
              testCase "extractScalarValue" $ do
                let x = Scalar (bv 8 0) false
                let xvalue = ScalarValue x :: Value 'C
                let actual = extractScalarValue xvalue
                actual @?= Right x,
              testCase "extractPtrValue" $ do
                let ptr = Ptr 4 1 (bv 8 0) false
                let ptrValue = PtrValue ptr :: Value 'C
                let actual = extractPtrValue ptrValue
                actual @?= Right ptr,
              testCase "extractMemValue" $ do
                let mem =
                      Memory (M.fromList [(1, MemoryBlock (bv 40 0) (bv 40 0))])
                let memValue = MemValue mem :: Value 'C
                let actual = extractMemValue memValue
                actual @?= Right mem
            ]
        ],
      testGroup
        "GenIntermediate"
        [ testCase "VLType" $ do
            let actual =
                  mrgModifyError (const "err") $
                    tryMerge $
                      flip runFreshT "a" $
                        genIntermediate vconst8162 (VLType 2 :: ValueType)
            let expected = do
                  let r =
                        VL
                          2
                          (VLNum (isymBV 8 "a" 0))
                          (symPolicy (isym "a" 1) (isym "a" 2))
                          false
                  mrgModifyError (const "err") $ validVL vconst8162 2 r
                  mrgReturn $ VLValue r ::
                    SymbolicContext (Value 'S)
            actual .@?= expected,
          testCase "MaskType" $ do
            let actual =
                  flip runFreshT "a" $
                    genIntermediate vconst8162 (MaskType 2)
            let expected =
                  mrgReturn
                    ( MaskValue $
                        Mask 2 $
                          VectorReg (isymBV 16 "a" 0) (isymBV 16 "a" 1) ::
                        Value 'S
                    ) ::
                    SymbolicContext (Value 'S)
            actual .@?= expected,
          testCase "VectorType" $ do
            let actual =
                  flip runFreshT "a" $
                    genIntermediate
                      vconst8162
                      (VectorType vectorConfigEF2M2)
            let expected =
                  mrgReturn
                    ( VectorValue $
                        Vector
                          vectorConfigEF2M2
                          [ VectorReg (isymBV 16 "a" 0) (isymBV 16 "a" 1),
                            VectorReg (isymBV 16 "a" 2) (isymBV 16 "a" 3)
                          ] ::
                        Value 'S
                    ) ::
                    SymbolicContext (Value 'S)
            actual .@?= expected,
          testCase "ScalarType" $ do
            let actual =
                  flip runFreshT "a" $
                    genIntermediate
                      vconst8162
                      (ScalarType (1 / 2) :: ValueType)
            let expected =
                  mrgReturn
                    ( ScalarValue $ Scalar (isymBV 4 "a" 0) false ::
                        Value 'S
                    ) ::
                    SymbolicContext (Value 'S)
            actual .@?= expected,
          testCase "ScalarType" $ do
            let actual =
                  flip runFreshT "a" $
                    genIntermediate
                      vconst8162
                      (ScalarType 1 :: ValueType)
            let expected =
                  mrgReturn
                    ( ScalarValue $ Scalar (isymBV 8 "a" 0) false ::
                        Value 'S
                    ) ::
                    SymbolicContext (Value 'S)
            actual .@?= expected,
          testCase "PtrType" $ do
            let actual =
                  tryMerge $
                    flip runFreshT "a" $
                      genIntermediate
                        ( vconstWithMem vconst8162 $
                            M.fromList [(0, 20), (1, 30)]
                        )
                        (PtrType 4 0 :: ValueType)
            let expected =
                  mrgReturn
                    ( PtrValue $ Ptr 4 0 (isymBV 8 "a" 0) (isym "a" 1) ::
                        Value 'S
                    ) ::
                    SymbolicContext (Value 'S)
            actual .@?= expected,
          testCase "MemType" $ do
            let actual =
                  tryMerge $
                    flip runFreshT "a" $
                      genIntermediate
                        ( vconstWithMem vconst8162 $
                            M.fromList [(0, 20), (1, 30)]
                        )
                        (MemType :: ValueType)
            let expected =
                  mrgReturn
                    ( MemValue $
                        Memory $
                          M.fromList
                            [ ( 0,
                                MemoryBlock (isymBV 20 "a" 0) (isymBV 20 "a" 1)
                              ),
                              ( 1,
                                MemoryBlock (isymBV 30 "a" 2) (isymBV 30 "a" 3)
                              )
                            ] ::
                        Value 'S
                    ) ::
                    SymbolicContext (Value 'S)
            actual .@?= expected
        ]
    ]

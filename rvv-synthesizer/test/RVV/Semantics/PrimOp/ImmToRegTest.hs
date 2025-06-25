{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}

module RVV.Semantics.PrimOp.ImmToRegTest (immToRegTest) where

import Grisette (BV (bv), LogicalOp (false), SomeWordN)
import HieraSynth.Context (ConcreteContext)
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Imm (Imm (ConstImm))
import RVV.Semantics.Policy (nonePolicy)
import RVV.Semantics.PrimOp.FullMask (fullMask)
import RVV.Semantics.PrimOp.ImmToReg
  ( fullImmToVector,
    immToScalar,
    immToScalarForVInst,
    immToVector,
  )
import RVV.Semantics.PrimOp.Undefined (undefinedVector)
import RVV.Semantics.Value
  ( Scalar (Scalar),
    VL (VL),
    VLValue (VLNum),
    Vector (Vector),
    VectorReg (VectorReg)
  )
import RVV.Semantics.VectorConfigConstants (vconst8162, vectorConfigEF2M2)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

immToRegTest :: Test
immToRegTest =
  testGroup
    "ImmToReg"
    [ testGroup "immToScalar" $ do
        (signed, expected) <- [(True, 0xfa), (False, 0x1a)]
        return $ testCase (if signed then "signed" else "unsigned") $ do
          let actual =
                immToScalar vconst8162 signed 1 (ConstImm 0b11010) ::
                  ConcreteContext (Scalar 'C)
          actual @?= Right (Scalar (bv 8 expected :: SomeWordN) false),
      testGroup "immToScalarForVInst" $ do
        (signed, expected) <- [(True, 0xfd), (False, 0x05)]
        return $ testCase (if signed then "signed" else "unsigned") $ do
          let actual =
                immToScalarForVInst
                  vconst8162
                  signed
                  1
                  (ConstImm 0b101) ::
                  ConcreteContext (Scalar 'C)
          actual @?= Right (Scalar (bv 8 expected :: SomeWordN) false),
      testGroup "immToVector" $ do
        (signed, expected) <-
          [ ( True,
              Vector
                vectorConfigEF2M2
                [ VectorReg (bv 16 0xeeee :: SomeWordN) (bv 16 0),
                  VectorReg (bv 16 0x00ee :: SomeWordN) (bv 16 0xff00)
                ]
            ),
            ( False,
              Vector
                vectorConfigEF2M2
                [ VectorReg (bv 16 0x6666 :: SomeWordN) (bv 16 0),
                  VectorReg (bv 16 0x0066 :: SomeWordN) (bv 16 0xff00)
                ]
            )
            ]
        return $ testCase (if signed then "signed" else "unsigned") $ do
          let actual =
                immToVector
                  vconst8162
                  vectorConfigEF2M2
                  signed
                  (undefinedVector vconst8162 vectorConfigEF2M2)
                  (fullMask vconst8162 4)
                  (VL 4 (VLNum (bv 8 0x6)) nonePolicy false)
                  (ConstImm 0b110) ::
                  ConcreteContext (Vector 'C)
          actual @?= Right expected,
      testGroup "fullImmToVector" $ do
        (signed, expected) <-
          [ ( True,
              Vector
                vectorConfigEF2M2
                [ VectorReg (bv 16 0xeeee :: SomeWordN) (bv 16 0),
                  VectorReg (bv 16 0xeeee :: SomeWordN) (bv 16 0)
                ]
            ),
            ( False,
              Vector
                vectorConfigEF2M2
                [ VectorReg (bv 16 0x6666 :: SomeWordN) (bv 16 0),
                  VectorReg (bv 16 0x6666 :: SomeWordN) (bv 16 0)
                ]
            )
            ]
        return $ testCase (if signed then "signed" else "unsigned") $ do
          let actual =
                fullImmToVector vconst8162 vectorConfigEF2M2 signed (ConstImm 0b110) ::
                  ConcreteContext (Vector 'C)
          actual @?= Right expected
    ]

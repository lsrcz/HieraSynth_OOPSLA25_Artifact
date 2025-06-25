{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RVV.Semantics.ElementTest (elemTest) where

import Grisette (BV (bv), SomeWordN)
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Element
  ( MaskElement (MaskElement),
    VectorElement (VectorElement),
    composeMask,
    composeVector,
    decomposeMask,
    decomposeVector,
    maskElementIsInitialized,
    vectorElementIsInitialized,
  )
import RVV.Semantics.Value
  ( Mask (Mask),
    Vector (Vector),
    VectorReg (VectorReg),
  )
import RVV.Semantics.VectorConfigConstants
  ( vconst8162,
    vectorConfigEF2M2,
    vectorConfigEF2MF2,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

-- Assume vconst8162

ef2m2Vector :: Vector 'C
ef2m2Vector =
  Vector
    vectorConfigEF2M2
    [ VectorReg (bv 16 0x1245) (bv 16 0xf00f),
      VectorReg (bv 16 0xabcd) (bv 16 0x0ff0)
    ]

ef2m2DecomposedVector :: [VectorElement 'C]
ef2m2DecomposedVector =
  [ VectorElement (bv 4 0x5) (bv 4 0xf),
    VectorElement (bv 4 0x4) (bv 4 0x0),
    VectorElement (bv 4 0x2) (bv 4 0x0),
    VectorElement (bv 4 0x1) (bv 4 0xf),
    VectorElement (bv 4 0xd) (bv 4 0x0),
    VectorElement (bv 4 0xc) (bv 4 0xf),
    VectorElement (bv 4 0xb) (bv 4 0xf),
    VectorElement (bv 4 0xa) (bv 4 0x0)
  ]

ef2mf2Vector :: Vector 'C
ef2mf2Vector =
  Vector vectorConfigEF2MF2 [VectorReg (bv 16 0x1245) (bv 16 0xf00f)]

ef2mf2DecomposedVector :: [VectorElement 'C]
ef2mf2DecomposedVector =
  [ VectorElement (bv 4 0x5) (bv 4 0xf),
    VectorElement (bv 4 0x4) (bv 4 0x0),
    VectorElement (bv 4 0x2) (bv 4 0x0),
    VectorElement (bv 4 0x1) (bv 4 0xf)
  ]

mul4Mask :: Mask 'C
mul4Mask = Mask 4 $ VectorReg (bv 16 0x1245) (bv 16 0xf0f0)

mul1Mask :: Mask 'C
mul1Mask = Mask 1 $ VectorReg (bv 16 0x1245) (bv 16 0xf0f0)

mul4DecomposedMask :: [MaskElement 'C]
mul4DecomposedMask =
  [ MaskElement True False,
    MaskElement False False,
    MaskElement True False,
    MaskElement False False,
    MaskElement False True,
    MaskElement False True,
    MaskElement True True,
    MaskElement False True
  ]

mul1DecomposedMask :: [MaskElement 'C]
mul1DecomposedMask = [MaskElement True False, MaskElement False False]

mul4RebuiltMask :: Mask 'C
mul4RebuiltMask = Mask 4 $ VectorReg (bv 16 0x0045) (bv 16 0xfff0)

mul1RebuiltMask :: Mask 'C
mul1RebuiltMask = Mask 1 $ VectorReg (bv 16 0x0001) (bv 16 0xfffc)

elemTest :: Test
elemTest =
  testGroup
    "VectorElement"
    [ testGroup
        "vectorElementIsInitialized"
        [ testCase "is initialized" $
            vectorElementIsInitialized (VectorElement (bv 4 0 :: SomeWordN) (bv 4 0))
              @?= True,
          testCase "is uninitialized" $
            vectorElementIsInitialized (VectorElement (bv 4 0 :: SomeWordN) (bv 4 1))
              @?= False
        ],
      testGroup
        "maskElementIsInitialized"
        [ testCase "is initialized" $
            maskElementIsInitialized (MaskElement True False) @?= True,
          testCase "is uninitialized" $
            maskElementIsInitialized (MaskElement True True) @?= False
        ],
      testGroup
        "decomposeVector"
        [ testCase "EF2M2" $ do
            let actual = decomposeVector vconst8162 ef2m2Vector
            let expected = Right ef2m2DecomposedVector
            actual @?= expected,
          testCase "EF2MF2" $ do
            let actual = decomposeVector vconst8162 ef2mf2Vector
            let expected = Right ef2mf2DecomposedVector
            actual @?= expected
        ],
      testGroup
        "composeVector"
        [ testCase "EF2M2" $ do
            let actual =
                  composeVector
                    vconst8162
                    vectorConfigEF2M2
                    ef2m2DecomposedVector
            let expected = Right ef2m2Vector
            actual @?= expected,
          testCase "EF2MF2" $ do
            let actual =
                  composeVector
                    vconst8162
                    vectorConfigEF2MF2
                    ef2mf2DecomposedVector
            let expected = Right ef2mf2Vector
            actual @?= expected
        ],
      testGroup
        "decomposeMask"
        [ testCase "maskMul=4" $ do
            decomposeMask vconst8162 mul4Mask @?= Right mul4DecomposedMask,
          testCase "maskMul=1" $ do
            decomposeMask vconst8162 mul1Mask @?= Right mul1DecomposedMask
        ],
      testGroup
        "composeMask"
        [ testCase "maskMul=4" $ do
            let actual = composeMask vconst8162 4 mul4DecomposedMask
            actual @?= Right mul4RebuiltMask,
          testCase "maskMul=1" $ do
            let actual = composeMask vconst8162 1 mul1DecomposedMask
            actual @?= Right mul1RebuiltMask
        ]
    ]

{-# LANGUAGE OverloadedStrings #-}

module RVV.Semantics.VectorConfigTest (vectorConfigTest) where

import Grisette (BV (bv), PPrint (pformat), SomeWordN)
import HieraSynth.Util.Pretty (renderDoc)
import RVV.Semantics.VectorConfig
  ( VectorConfig (VectorConfig),
    delegatedVectorMaskMul,
    getDelegatedVectorConfig,
    maskLengthMul,
    maskNumValidElements,
    maskValidElementIndices,
    narrowVectorConfig,
    scalarBitWidth,
    vectorElementIndices,
    vectorMaskMul,
    vectorNumAllElements,
    vectorNumRegisters,
    vectorNumValidElements,
    widenVectorConfig,
  )
import RVV.Semantics.VectorConfigConstants
  ( vconst32128,
    vconst64128,
    vconst64512,
    vconst8162,
    vectorConfigEF4M1,
    vectorConfigEF4M4,
    vectorConfigEF4M8,
    vectorConfigEF8M1,
    vectorConfigEF8M4,
    vectorConfigEF8M8,
    vectorConfigEF8MF2,
    vectorConfigEF8MF8,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

vectorConfigTest :: Test
vectorConfigTest =
  testGroup
    "VectorConfig"
    [ testGroup
        "vectorMaskMul"
        [ testCase "EF4M4" $ vectorMaskMul vectorConfigEF4M4 @?= 16,
          testCase "EF8M8" $ vectorMaskMul vectorConfigEF8M8 @?= 64,
          testCase "EF8M1" $ vectorMaskMul vectorConfigEF8M1 @?= 8,
          testCase "EF8MF8" $ vectorMaskMul vectorConfigEF8MF8 @?= 1
        ],
      testGroup
        "delegatedVectorMaskMul"
        [ testCase "EF4M1" $
            delegatedVectorMaskMul vconst64512 vectorConfigEF4M1 @?= Right 64,
          testCase "EF8MF2" $
            delegatedVectorMaskMul vconst64512 vectorConfigEF8MF2 @?= Right 32,
          testCase "EF8M1" $
            delegatedVectorMaskMul vconst64512 vectorConfigEF8M1 @?= Right 64,
          testCase "EF8MF8" $
            delegatedVectorMaskMul vconst64512 vectorConfigEF8MF8 @?= Right 8
        ],
      testGroup
        "vectorNumRegisters"
        [ testCase "EF4M4" $ vectorNumRegisters vectorConfigEF4M4 @?= 4,
          testCase "EF8M8" $ vectorNumRegisters vectorConfigEF8M8 @?= 8,
          testCase "EF8M1" $ vectorNumRegisters vectorConfigEF8M1 @?= 1,
          testCase "EF8MF8" $ vectorNumRegisters vectorConfigEF8MF8 @?= 1
        ],
      testGroup
        "maskNumValidElements"
        [ testCase "8" $ maskNumValidElements vconst64512 8 @?= 64,
          testCase "16" $ maskNumValidElements vconst64512 16 @?= 128,
          testCase "64" $ maskNumValidElements vconst64512 64 @?= 512
        ],
      testGroup
        "maskLengthMul"
        [ testCase "8" $ maskLengthMul vconst64512 8 @?= 1 / 8,
          testCase "16" $ maskLengthMul vconst64512 16 @?= 1 / 4,
          testCase "64" $ maskLengthMul vconst64512 64 @?= 1
        ],
      testGroup
        "maskValidElementIndices"
        [ testCase "8" $
            maskValidElementIndices vconst64512 8
              @?= [bv 64 i :: SomeWordN | i <- [0 .. 63]],
          testCase "16" $
            maskValidElementIndices vconst64512 16
              @?= [bv 64 i :: SomeWordN | i <- [0 .. 127]],
          testCase "64" $
            maskValidElementIndices vconst64512 64
              @?= [bv 64 i :: SomeWordN | i <- [0 .. 511]]
        ],
      testGroup
        "vectorNumValidElements"
        [ testCase "EF4M4" $ vectorNumValidElements vconst32128 vectorConfigEF4M4 @?= 64,
          testCase "EF8M8" $ vectorNumValidElements vconst32128 vectorConfigEF8M8 @?= 256,
          testCase "EF8M1" $ vectorNumValidElements vconst32128 vectorConfigEF8M1 @?= 32,
          testCase "EF8MF8" $ vectorNumValidElements vconst32128 vectorConfigEF8MF8 @?= 4
        ],
      testGroup
        "vectorNumAllElements"
        [ testCase "EF4M4" $ vectorNumAllElements vconst32128 vectorConfigEF4M4 @?= 64,
          testCase "EF8M8" $ vectorNumAllElements vconst32128 vectorConfigEF8M8 @?= 256,
          testCase "EF8M1" $ vectorNumAllElements vconst32128 vectorConfigEF8M1 @?= 32,
          testCase "EF8MF8" $ vectorNumAllElements vconst32128 vectorConfigEF8MF8 @?= 32
        ],
      testGroup
        "vectorElementIndices"
        [ testCase "EF4M4" $ do
            let actual = vectorElementIndices vconst32128 vectorConfigEF4M4
            let expected = [bv 32 i :: SomeWordN | i <- [0 .. 63]]
            actual @?= expected,
          testCase "EF8M8" $ do
            let actual = vectorElementIndices vconst32128 vectorConfigEF8M8
            let expected = [bv 32 i :: SomeWordN | i <- [0 .. 255]]
            actual @?= expected,
          testCase "EF8M1" $ do
            let actual = vectorElementIndices vconst32128 vectorConfigEF8M1
            let expected = [bv 32 i :: SomeWordN | i <- [0 .. 31]]
            actual @?= expected,
          testCase "EF8MF8" $ do
            let actual = vectorElementIndices vconst32128 vectorConfigEF8MF8
            let expected = [bv 32 i :: SomeWordN | i <- [0 .. 31]]
            actual @?= expected
        ],
      testGroup
        "widenVectorConfig"
        [testCase "EF4M4" $ widenVectorConfig vectorConfigEF8M4 @?= vectorConfigEF4M8],
      testGroup
        "narrowVectorConfig"
        [testCase "EF8M8" $ narrowVectorConfig vectorConfigEF4M8 @?= vectorConfigEF8M4],
      testGroup
        "pformat"
        [ testCase "EF4M4" $ renderDoc 80 (pformat vectorConfigEF4M4) @?= "ef4m4",
          testCase "EF8M8" $ renderDoc 80 (pformat vectorConfigEF8M8) @?= "ef8m8",
          testCase "EF8M1" $ renderDoc 80 (pformat vectorConfigEF8M1) @?= "ef8m1",
          testCase "EF8MF8" $ renderDoc 80 (pformat vectorConfigEF8MF8) @?= "ef8mf8"
        ],
      testGroup
        "scalarBitWidth"
        [ testCase "1" $ scalarBitWidth vconst8162 1 @?= 8,
          testCase "1/2" $ scalarBitWidth vconst8162 (1 / 2) @?= 4,
          testCase "1/4" $ scalarBitWidth vconst8162 (1 / 4) @?= 2
        ],
      testGroup
        "getDelegatedVectorConfig"
        [ testCase "VLEN128/EF8/Mask1" $
            getDelegatedVectorConfig vconst64128 (1 / 8) 1
              @?= Right (VectorConfig (1 / 8) (1 / 16)),
          testCase "VLEN512/EF8/Mask1" $
            getDelegatedVectorConfig vconst64512 (1 / 8) 1
              @?= Right (VectorConfig (1 / 8) (1 / 64)),
          testCase "VLEN512/EF8/Mask8" $
            getDelegatedVectorConfig vconst64512 (1 / 8) 8
              @?= Right (VectorConfig (1 / 8) (1 / 8)),
          testCase "VLEN512/EF8/Mask64" $
            getDelegatedVectorConfig vconst64512 (1 / 8) 64
              @?= Right (VectorConfig (1 / 8) 1)
        ]
    ]

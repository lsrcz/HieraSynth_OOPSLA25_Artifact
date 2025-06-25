{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module RVV.Semantics.PrimOp.SlideTest (slideTest) where

import Grisette (BV (bv), LogicalOp (false))
import HieraSynth.Context (ConcreteContext)
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Policy (nonePolicy, tumuPolicy)
import RVV.Semantics.PrimOp.FullMask (fullMask)
import RVV.Semantics.PrimOp.Slide
  ( slide1down,
    slide1up,
    slidedown,
    slideup,
    unsafeVectorRegShiftLeft,
    unsafeVectorRegShiftRight,
  )
import RVV.Semantics.Value
  ( Mask (Mask),
    Scalar (Scalar),
    VL (VL),
    VLValue (VLNum),
    Vector (Vector),
    VectorReg (VectorReg),
  )
import RVV.Semantics.VectorConfig (VectorConfig)
import RVV.Semantics.VectorConfigConstants
  ( vconst8162,
    vectorConfigEF2M2,
    vectorConfigEF2MF2,
    vectorConfigEF4M1,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

testVector :: Vector 'C
testVector =
  Vector
    vectorConfigEF2M2
    [ VectorReg (bv 16 0x1234) (bv 16 0xf0ff),
      VectorReg (bv 16 0x5678) (bv 16 0x00ff)
    ]

data SlideTest = SlideTest
  { slideTestName :: String,
    slideTestDest :: Vector 'C,
    slideTestMask :: Mask 'C,
    slideTestSrc :: Vector 'C,
    slideTestScalar :: Scalar 'C,
    slideTestVL :: VL 'C,
    slideTestVectorConfig :: VectorConfig,
    slideTestExpected :: Vector 'C
  }

slideTest :: Test
slideTest =
  testGroup
    "Slide"
    [ testGroup "unsafeVectorRegShiftLeft" $ do
        let shiftReg =
              unsafeVectorRegShiftLeft vconst8162 vectorConfigEF2M2 ::
                Vector 'C -> Scalar 'C -> ConcreteContext (Vector 'C)
        [ testCase "By 0" $ do
            let actual = shiftReg testVector (Scalar (bv 8 0) false)
            let expected = Right testVector
            actual @?= expected,
          testCase "By 1" $ do
            let actual = shiftReg testVector (Scalar (bv 8 1) false)
            let expected =
                  Right $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x2340) (bv 16 0x0ff0),
                        VectorReg (bv 16 0x6781) (bv 16 0x0fff)
                      ]
            actual @?= expected,
          testCase "By 5" $ do
            let actual = shiftReg testVector (Scalar (bv 8 5) false)
            let expected =
                  Right $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x0000) (bv 16 0x0000),
                        VectorReg (bv 16 0x2340) (bv 16 0x0ff0)
                      ]
            actual @?= expected,
          testCase "By 9" $ do
            let actual = shiftReg testVector (Scalar (bv 8 9) false)
            let expected =
                  Right $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x0000) (bv 16 0x0000),
                        VectorReg (bv 16 0x0000) (bv 16 0x0000)
                      ]
            actual @?= expected
          ],
      testGroup "unsafeVectorRegShiftRight" $ do
        let shiftReg =
              unsafeVectorRegShiftRight vconst8162 vectorConfigEF2M2 ::
                Vector 'C -> Scalar 'C -> ConcreteContext (Vector 'C)
        [ testCase "By 0" $ do
            let actual = shiftReg testVector (Scalar (bv 8 0) false)
            let expected = Right testVector
            actual @?= expected,
          testCase "By 1" $ do
            let actual = shiftReg testVector (Scalar (bv 8 1) false)
            let expected =
                  Right $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x8123) (bv 16 0xff0f),
                        VectorReg (bv 16 0x0567) (bv 16 0x000f)
                      ]
            actual @?= expected,
          testCase "By 5" $ do
            let actual = shiftReg testVector (Scalar (bv 8 5) false)
            let expected =
                  Right $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x0567) (bv 16 0x000f),
                        VectorReg (bv 16 0x0000) (bv 16 0x0000)
                      ]
            actual @?= expected,
          testCase "By 9" $ do
            let actual = shiftReg testVector (Scalar (bv 8 9) false)
            let expected =
                  Right $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x0000) (bv 16 0x0000),
                        VectorReg (bv 16 0x0000) (bv 16 0x0000)
                      ]
            actual @?= expected
          ],
      testGroup "slideup" $ do
        SlideTest {..} <-
          [ SlideTest
              { slideTestName = "Slideup offset 0, full mask & vl",
                slideTestDest =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x1234) (bv 16 0x0f0f),
                      VectorReg (bv 16 0x5678) (bv 16 0x00ff)
                    ],
                slideTestMask = fullMask vconst8162 4,
                slideTestSrc =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x9abc) (bv 16 0xf0f0),
                      VectorReg (bv 16 0xdefb) (bv 16 0xff00)
                    ],
                slideTestScalar = Scalar (bv 8 0) false,
                slideTestVL = VL 4 (VLNum $ bv 8 8) nonePolicy false,
                slideTestVectorConfig = vectorConfigEF2M2,
                slideTestExpected =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x9abc) (bv 16 0xf0f0),
                      VectorReg (bv 16 0xdefb) (bv 16 0xff00)
                    ]
              },
            SlideTest
              { slideTestName = "Slideup offset 1, full mask & vl",
                slideTestDest =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x1234) (bv 16 0x0f0f),
                      VectorReg (bv 16 0x5678) (bv 16 0x00ff)
                    ],
                slideTestMask = fullMask vconst8162 4,
                slideTestSrc =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x9abc) (bv 16 0xf0f0),
                      VectorReg (bv 16 0xdefb) (bv 16 0xff00)
                    ],
                slideTestScalar = Scalar (bv 8 1) false,
                slideTestVL = VL 4 (VLNum $ bv 8 8) nonePolicy false,
                slideTestVectorConfig = vectorConfigEF2M2,
                slideTestExpected =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0xabc4) (bv 16 0x0f0f),
                      VectorReg (bv 16 0xefb9) (bv 16 0xf00f)
                    ]
              },
            SlideTest
              { slideTestName = "Slideup offset 5, full mask & vl",
                slideTestDest =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x1234) (bv 16 0x0f0f),
                      VectorReg (bv 16 0x5678) (bv 16 0x00ff)
                    ],
                slideTestMask = fullMask vconst8162 4,
                slideTestSrc =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x9abc) (bv 16 0xf0f0),
                      VectorReg (bv 16 0xdefb) (bv 16 0xff00)
                    ],
                slideTestScalar = Scalar (bv 8 5) false,
                slideTestVL = VL 4 (VLNum $ bv 8 8) nonePolicy false,
                slideTestVectorConfig = vectorConfigEF2M2,
                slideTestExpected =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x1234) (bv 16 0x0f0f),
                      VectorReg (bv 16 0xabc8) (bv 16 0x0f0f)
                    ]
              },
            SlideTest
              { slideTestName = "Slideup offset 9, full mask & vl",
                slideTestDest =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x1234) (bv 16 0x0f0f),
                      VectorReg (bv 16 0x5678) (bv 16 0x00ff)
                    ],
                slideTestMask = fullMask vconst8162 4,
                slideTestSrc =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x9abc) (bv 16 0xf0f0),
                      VectorReg (bv 16 0xdefb) (bv 16 0xff00)
                    ],
                slideTestScalar = Scalar (bv 8 9) false,
                slideTestVL = VL 4 (VLNum $ bv 8 8) nonePolicy false,
                slideTestVectorConfig = vectorConfigEF2M2,
                slideTestExpected =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x1234) (bv 16 0x0f0f),
                      VectorReg (bv 16 0x5678) (bv 16 0x00ff)
                    ]
              },
            SlideTest
              { slideTestName = "Slideup offset 2, mask & vl",
                slideTestDest =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x1234) (bv 16 0x0000),
                      VectorReg (bv 16 0x5678) (bv 16 0x0000)
                    ],
                slideTestMask = Mask 4 $ VectorReg (bv 16 0x56) (bv 16 0),
                slideTestSrc =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x9abc) (bv 16 0x0000),
                      VectorReg (bv 16 0xdefb) (bv 16 0x0000)
                    ],
                slideTestScalar = Scalar (bv 8 2) false,
                slideTestVL = VL 4 (VLNum $ bv 8 6) nonePolicy false,
                slideTestVectorConfig = vectorConfigEF2M2,
                slideTestExpected =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x0c30) (bv 16 0xf00f),
                      VectorReg (bv 16 0x000a) (bv 16 0xfff0)
                    ]
              },
            SlideTest
              { slideTestName = "slideup/half/2",
                slideTestDest =
                  Vector
                    vectorConfigEF2MF2
                    [ VectorReg (bv 16 0x1234) (bv 16 0x0000)
                    ],
                slideTestMask = Mask 1 $ VectorReg (bv 16 0xff) (bv 16 0),
                slideTestSrc =
                  Vector
                    vectorConfigEF2MF2
                    [ VectorReg (bv 16 0x9abc) (bv 16 0x0000)
                    ],
                slideTestScalar = Scalar (bv 8 2) false,
                slideTestVL = VL 1 (VLNum $ bv 8 2) tumuPolicy false,
                slideTestVectorConfig = vectorConfigEF2MF2,
                slideTestExpected =
                  Vector
                    vectorConfigEF2MF2
                    [ VectorReg (bv 16 0x1234) (bv 16 0x0000)
                    ]
              },
            SlideTest
              { slideTestName = "slideup/half/0",
                slideTestDest =
                  Vector
                    vectorConfigEF2MF2
                    [ VectorReg (bv 16 0x1234) (bv 16 0x0000)
                    ],
                slideTestMask = Mask 1 $ VectorReg (bv 16 0xff) (bv 16 0),
                slideTestSrc =
                  Vector
                    vectorConfigEF2MF2
                    [ VectorReg (bv 16 0x9abc) (bv 16 0x0000)
                    ],
                slideTestScalar = Scalar (bv 8 0) false,
                slideTestVL = VL 1 (VLNum $ bv 8 2) tumuPolicy false,
                slideTestVectorConfig = vectorConfigEF2MF2,
                slideTestExpected =
                  Vector
                    vectorConfigEF2MF2
                    [ VectorReg (bv 16 0x12bc) (bv 16 0x0000)
                    ]
              },
            SlideTest
              { slideTestName = "slideup/half/1",
                slideTestDest =
                  Vector
                    vectorConfigEF2MF2
                    [ VectorReg (bv 16 0x1234) (bv 16 0x0000)
                    ],
                slideTestMask = Mask 1 $ VectorReg (bv 16 0xff) (bv 16 0),
                slideTestSrc =
                  Vector
                    vectorConfigEF2MF2
                    [ VectorReg (bv 16 0x9abc) (bv 16 0x0000)
                    ],
                slideTestScalar = Scalar (bv 8 1) false,
                slideTestVL = VL 1 (VLNum $ bv 8 2) tumuPolicy false,
                slideTestVectorConfig = vectorConfigEF2MF2,
                slideTestExpected =
                  Vector
                    vectorConfigEF2MF2
                    [ VectorReg (bv 16 0x12c4) (bv 16 0x0000)
                    ]
              },
            SlideTest
              { slideTestName = "slideup/EF4/1",
                slideTestDest =
                  Vector
                    vectorConfigEF4M1
                    [ VectorReg (bv 16 0x1234) (bv 16 0x0000)
                    ],
                slideTestMask = Mask 4 $ VectorReg (bv 16 0xff) (bv 16 0),
                slideTestSrc =
                  Vector
                    vectorConfigEF4M1
                    [ VectorReg (bv 16 0x9abc) (bv 16 0x0000)
                    ],
                slideTestScalar = Scalar (bv 8 2) false,
                slideTestVL = VL 4 (VLNum $ bv 8 8) tumuPolicy false,
                slideTestVectorConfig = vectorConfigEF4M1,
                slideTestExpected =
                  Vector
                    vectorConfigEF4M1
                    [ VectorReg (bv 16 0xabc4) (bv 16 0x0000)
                    ]
              }
            ]
        return $ testCase slideTestName $ do
          let actual =
                slideup
                  vconst8162
                  slideTestVectorConfig
                  slideTestDest
                  slideTestMask
                  slideTestSrc
                  slideTestScalar
                  slideTestVL
          actual @?= Right slideTestExpected,
      testGroup "slidedown" $ do
        SlideTest {..} <-
          [ SlideTest
              { slideTestName = "Slidedown offset 0, full mask & vl",
                slideTestDest =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x1234) (bv 16 0x0f0f),
                      VectorReg (bv 16 0x5678) (bv 16 0x00ff)
                    ],
                slideTestMask = fullMask vconst8162 4,
                slideTestSrc =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x9abc) (bv 16 0xf0f0),
                      VectorReg (bv 16 0xdefb) (bv 16 0xff00)
                    ],
                slideTestScalar = Scalar (bv 8 0) false,
                slideTestVL = VL 4 (VLNum $ bv 8 8) nonePolicy false,
                slideTestVectorConfig = vectorConfigEF2M2,
                slideTestExpected =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x9abc) (bv 16 0xf0f0),
                      VectorReg (bv 16 0xdefb) (bv 16 0xff00)
                    ]
              },
            SlideTest
              { slideTestName = "Slidedown offset 1, full mask & vl",
                slideTestDest =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x1234) (bv 16 0x0f0f),
                      VectorReg (bv 16 0x5678) (bv 16 0x00ff)
                    ],
                slideTestMask = fullMask vconst8162 4,
                slideTestSrc =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x9abc) (bv 16 0xf0f0),
                      VectorReg (bv 16 0xdefb) (bv 16 0xff00)
                    ],
                slideTestScalar = Scalar (bv 8 1) false,
                slideTestVL = VL 4 (VLNum $ bv 8 8) nonePolicy false,
                slideTestVectorConfig = vectorConfigEF2M2,
                slideTestExpected =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0xb9ab) (bv 16 0x0f0f),
                      VectorReg (bv 16 0x0def) (bv 16 0x0ff0)
                    ]
              },
            SlideTest
              { slideTestName = "Slidedown offset 5, full mask & vl",
                slideTestDest =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x1234) (bv 16 0x0f0f),
                      VectorReg (bv 16 0x5678) (bv 16 0x00ff)
                    ],
                slideTestMask = fullMask vconst8162 4,
                slideTestSrc =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x9abc) (bv 16 0xf0f0),
                      VectorReg (bv 16 0xdefb) (bv 16 0xff00)
                    ],
                slideTestScalar = Scalar (bv 8 5) false,
                slideTestVL = VL 4 (VLNum $ bv 8 8) nonePolicy false,
                slideTestVectorConfig = vectorConfigEF2M2,
                slideTestExpected =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x0def) (bv 16 0x0ff0),
                      VectorReg (bv 16 0x0000) (bv 16 0x0000)
                    ]
              },
            SlideTest
              { slideTestName = "Slidedown offset 9, full mask & vl",
                slideTestDest =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x1234) (bv 16 0x0f0f),
                      VectorReg (bv 16 0x5678) (bv 16 0x00ff)
                    ],
                slideTestMask = fullMask vconst8162 4,
                slideTestSrc =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x9abc) (bv 16 0xf0f0),
                      VectorReg (bv 16 0xdefb) (bv 16 0xff00)
                    ],
                slideTestScalar = Scalar (bv 8 9) false,
                slideTestVL = VL 4 (VLNum $ bv 8 8) nonePolicy false,
                slideTestVectorConfig = vectorConfigEF2M2,
                slideTestExpected =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0) (bv 16 0),
                      VectorReg (bv 16 0) (bv 16 0)
                    ]
              },
            SlideTest
              { slideTestName = "Slidedown offset 2, mask & vl",
                slideTestDest =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x1234) (bv 16 0x0000),
                      VectorReg (bv 16 0x5678) (bv 16 0x0000)
                    ],
                slideTestMask = Mask 4 $ VectorReg (bv 16 0x56) (bv 16 0),
                slideTestSrc =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x9abc) (bv 16 0x0000),
                      VectorReg (bv 16 0xdefb) (bv 16 0x0000)
                    ],
                slideTestScalar = Scalar (bv 8 2) false,
                slideTestVL = VL 4 (VLNum $ bv 8 6) nonePolicy false,
                slideTestVectorConfig = vectorConfigEF2M2,
                slideTestExpected =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x0b90) (bv 16 0xf00f),
                      VectorReg (bv 16 0x000e) (bv 16 0xfff0)
                    ]
              }
            ]
        return $ testCase slideTestName $ do
          let actual =
                slidedown
                  vconst8162
                  vectorConfigEF2M2
                  slideTestDest
                  slideTestMask
                  slideTestSrc
                  slideTestScalar
                  slideTestVL
          actual @?= Right slideTestExpected,
      testGroup "slide1up" $ do
        SlideTest {..} <-
          [ SlideTest
              { slideTestName = "slide1up v0.mask[i] enabled",
                slideTestDest =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x1234) (bv 16 0x0000),
                      VectorReg (bv 16 0x5678) (bv 16 0x0000)
                    ],
                slideTestMask = Mask 4 $ VectorReg (bv 16 0x57) (bv 16 0),
                slideTestSrc =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x9abc) (bv 16 0x0000),
                      VectorReg (bv 16 0xdefb) (bv 16 0x0000)
                    ],
                slideTestScalar = Scalar (bv 4 6) false,
                slideTestVL = VL 4 (VLNum $ bv 8 6) tumuPolicy false,
                slideTestVectorConfig = vectorConfigEF2M2,
                slideTestExpected =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x1bc6) (bv 16 0x0000),
                      VectorReg (bv 16 0x5679) (bv 16 0x0000)
                    ]
              },
            SlideTest
              { slideTestName = "slide1up v0.mask[i] disabled",
                slideTestDest =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x1234) (bv 16 0x0000),
                      VectorReg (bv 16 0x5678) (bv 16 0x0000)
                    ],
                slideTestMask = Mask 4 $ VectorReg (bv 16 0x56) (bv 16 0),
                slideTestSrc =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x9abc) (bv 16 0x0000),
                      VectorReg (bv 16 0xdefb) (bv 16 0x0000)
                    ],
                slideTestScalar = Scalar (bv 4 6) false,
                slideTestVL = VL 4 (VLNum $ bv 8 6) tumuPolicy false,
                slideTestVectorConfig = vectorConfigEF2M2,
                slideTestExpected =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x1bc4) (bv 16 0x0000),
                      VectorReg (bv 16 0x5679) (bv 16 0x0000)
                    ]
              }
            ]
        return $ testCase slideTestName $ do
          let actual =
                slide1up
                  vconst8162
                  vectorConfigEF2M2
                  slideTestDest
                  slideTestMask
                  slideTestSrc
                  slideTestScalar
                  slideTestVL
          actual @?= Right slideTestExpected,
      testGroup "slide1down" $ do
        SlideTest {..} <-
          [ SlideTest
              { slideTestName = "slide1down v0.mask[vl-1] enabled",
                slideTestDest =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x1234) (bv 16 0x0000),
                      VectorReg (bv 16 0x5678) (bv 16 0x0000)
                    ],
                slideTestMask = Mask 4 $ VectorReg (bv 16 0x37) (bv 16 0),
                slideTestSrc =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x9abc) (bv 16 0x0000),
                      VectorReg (bv 16 0xdefb) (bv 16 0x0000)
                    ],
                slideTestScalar = Scalar (bv 4 6) false,
                slideTestVL = VL 4 (VLNum $ bv 8 6) tumuPolicy false,
                slideTestVectorConfig = vectorConfigEF2M2,
                slideTestExpected =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x19ab) (bv 16 0x0000),
                      VectorReg (bv 16 0x566f) (bv 16 0x0000)
                    ]
              },
            SlideTest
              { slideTestName = "slide1down v0.mask[i] disabled",
                slideTestDest =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x1234) (bv 16 0x0000),
                      VectorReg (bv 16 0x5678) (bv 16 0x0000)
                    ],
                slideTestMask = Mask 4 $ VectorReg (bv 16 0x57) (bv 16 0),
                slideTestSrc =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x9abc) (bv 16 0x0000),
                      VectorReg (bv 16 0xdefb) (bv 16 0x0000)
                    ],
                slideTestScalar = Scalar (bv 4 6) false,
                slideTestVL = VL 4 (VLNum $ bv 8 6) tumuPolicy false,
                slideTestVectorConfig = vectorConfigEF2M2,
                slideTestExpected =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x19ab) (bv 16 0x0000),
                      VectorReg (bv 16 0x567f) (bv 16 0x0000)
                    ]
              },
            SlideTest
              { slideTestName = "slide1down/half/2",
                slideTestDest =
                  Vector
                    vectorConfigEF2MF2
                    [ VectorReg (bv 16 0x1234) (bv 16 0x0000)
                    ],
                slideTestMask = Mask 1 $ VectorReg (bv 16 0x57) (bv 16 0),
                slideTestSrc =
                  Vector
                    vectorConfigEF2MF2
                    [ VectorReg (bv 16 0x9abc) (bv 16 0x0000)
                    ],
                slideTestScalar = Scalar (bv 4 6) false,
                slideTestVL = VL 1 (VLNum $ bv 8 2) tumuPolicy false,
                slideTestVectorConfig = vectorConfigEF2MF2,
                slideTestExpected =
                  Vector
                    vectorConfigEF2MF2
                    [ VectorReg (bv 16 0x126b) (bv 16 0x0000)
                    ]
              },
            SlideTest
              { slideTestName = "slide1down/half/0",
                slideTestDest =
                  Vector
                    vectorConfigEF2MF2
                    [ VectorReg (bv 16 0x1234) (bv 16 0x0000)
                    ],
                slideTestMask = Mask 1 $ VectorReg (bv 16 0x57) (bv 16 0),
                slideTestSrc =
                  Vector
                    vectorConfigEF2MF2
                    [ VectorReg (bv 16 0x9abc) (bv 16 0x0000)
                    ],
                slideTestScalar = Scalar (bv 4 6) false,
                slideTestVL = VL 1 (VLNum $ bv 8 0) tumuPolicy false,
                slideTestVectorConfig = vectorConfigEF2MF2,
                slideTestExpected =
                  Vector
                    vectorConfigEF2MF2
                    [ VectorReg (bv 16 0x1234) (bv 16 0x0000)
                    ]
              },
            SlideTest
              { slideTestName = "slide1down/half/1",
                slideTestDest =
                  Vector
                    vectorConfigEF2MF2
                    [ VectorReg (bv 16 0x1234) (bv 16 0x0000)
                    ],
                slideTestMask = Mask 1 $ VectorReg (bv 16 0x57) (bv 16 0),
                slideTestSrc =
                  Vector
                    vectorConfigEF2MF2
                    [ VectorReg (bv 16 0x9abc) (bv 16 0x0000)
                    ],
                slideTestScalar = Scalar (bv 4 6) false,
                slideTestVL = VL 1 (VLNum $ bv 8 1) tumuPolicy false,
                slideTestVectorConfig = vectorConfigEF2MF2,
                slideTestExpected =
                  Vector
                    vectorConfigEF2MF2
                    [ VectorReg (bv 16 0x1236) (bv 16 0x0000)
                    ]
              }
            ]
        return $ testCase slideTestName $ do
          let actual =
                slide1down
                  vconst8162
                  slideTestVectorConfig
                  slideTestDest
                  slideTestMask
                  slideTestSrc
                  slideTestScalar
                  slideTestVL
          actual @?= Right slideTestExpected
    ]

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.OpSemantics.SlideTest (slideTest) where

import Grisette (BV (bv), LogicalOp (false))
import HieraSynth.TypeSignature
  ( TypeSignature (TypeSignature, argTypes, resTypes),
  )
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Imm (Imm (ConstImm))
import RVV.Semantics.Policy (nonePolicy, tumuPolicy)
import RVV.Semantics.Value
  ( Mask (Mask),
    Scalar (Scalar),
    VL (VL),
    VLValue (VLNum),
    Vector (Vector),
    VectorReg (VectorReg),
  )
import RVV.Semantics.VectorConfigConstants (vconst8162, vectorConfigEF2M2)
import RVV.Synthesizer.OpSemantics.Common
  ( SemanticsTest
      ( SemanticsTest,
        semanticsTestExpected,
        semanticsTestInput,
        semanticsTestMachineConfig,
        semanticsTestName,
        semanticsTestOp
      ),
    TypingTest (TypingTest, typingTestExpected, typingTestName, typingTestOp),
    semanticsTest,
    typingTest,
  )
import RVV.Synthesizer.Operator.Common.RHSSpec (RHSSpec (ImmRHS, ScalarRHS))
import RVV.Synthesizer.Operator.Slide (slide, slide1)
import RVV.Synthesizer.Parameter.Destination (pd, ud)
import RVV.Synthesizer.Parameter.Masking (pm)
import RVV.Synthesizer.Parameter.SlideDirection (mkSlideDown, mkSlideUp)
import RVV.Synthesizer.Type
  ( ValueType
      ( MaskType,
        ScalarType,
        VLType,
        VectorType
      ),
  )
import RVV.Synthesizer.Value
  ( Value (MaskValue, ScalarValue, VLValue, VectorValue),
  )
import Test.Framework (Test, testGroup)

slideTest :: Test
slideTest =
  testGroup
    "Slide"
    [ testGroup "Semantics" . fmap semanticsTest $
        [ SemanticsTest
            { semanticsTestName = "SlideupVX",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = slide @'C vectorConfigEF2M2 mkSlideUp pd pm ScalarRHS,
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 6)) nonePolicy false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x9abc) (bv 16 0x0000),
                        VectorReg (bv 16 0xdefb) (bv 16 0x0000)
                      ],
                  ScalarValue $ Scalar (bv 8 2) false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x1234) (bv 16 0x0000),
                        VectorReg (bv 16 0x5678) (bv 16 0x0000)
                      ],
                  MaskValue $ Mask 4 $ VectorReg (bv 16 0x56) (bv 16 0)
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x0c30) (bv 16 0xf00f),
                          VectorReg (bv 16 0x000a) (bv 16 0xfff0)
                        ] ::
                      Value 'C
                  ]
            },
          SemanticsTest
            { semanticsTestName = "SlidedownVX",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = slide @'C vectorConfigEF2M2 mkSlideDown pd pm ScalarRHS,
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 6)) nonePolicy false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x9abc) (bv 16 0x0000),
                        VectorReg (bv 16 0xdefb) (bv 16 0x0000)
                      ],
                  ScalarValue $ Scalar (bv 8 2) false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x1234) (bv 16 0x0000),
                        VectorReg (bv 16 0x5678) (bv 16 0x0000)
                      ],
                  MaskValue $ Mask 4 $ VectorReg (bv 16 0x56) (bv 16 0)
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x0b90) (bv 16 0xf00f),
                          VectorReg (bv 16 0x000e) (bv 16 0xfff0)
                        ] ::
                      Value 'C
                  ]
            },
          SemanticsTest
            { semanticsTestName = "SlideupVI",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp =
                slide @'C vectorConfigEF2M2 mkSlideUp pd pm (ImmRHS (ConstImm 2)),
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 6)) nonePolicy false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x9abc) (bv 16 0x0000),
                        VectorReg (bv 16 0xdefb) (bv 16 0x0000)
                      ],
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x1234) (bv 16 0x0000),
                        VectorReg (bv 16 0x5678) (bv 16 0x0000)
                      ],
                  MaskValue $ Mask 4 $ VectorReg (bv 16 0x56) (bv 16 0)
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x0c30) (bv 16 0xf00f),
                          VectorReg (bv 16 0x000a) (bv 16 0xfff0)
                        ] ::
                      Value 'C
                  ]
            },
          SemanticsTest
            { semanticsTestName = "SlidedownVI",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp =
                slide @'C vectorConfigEF2M2 mkSlideDown pd pm (ImmRHS (ConstImm 2)),
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 6)) nonePolicy false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x9abc) (bv 16 0x0000),
                        VectorReg (bv 16 0xdefb) (bv 16 0x0000)
                      ],
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x1234) (bv 16 0x0000),
                        VectorReg (bv 16 0x5678) (bv 16 0x0000)
                      ],
                  MaskValue $ Mask 4 $ VectorReg (bv 16 0x56) (bv 16 0)
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x0b90) (bv 16 0xf00f),
                          VectorReg (bv 16 0x000e) (bv 16 0xfff0)
                        ] ::
                      Value 'C
                  ]
            },
          SemanticsTest
            { semanticsTestName = "Slide1upVX",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = slide1 @'C vectorConfigEF2M2 mkSlideUp pd pm ScalarRHS,
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 6)) tumuPolicy false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x9abc) (bv 16 0x0000),
                        VectorReg (bv 16 0xdefb) (bv 16 0x0000)
                      ],
                  ScalarValue $ Scalar (bv 4 6) false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x1234) (bv 16 0x0000),
                        VectorReg (bv 16 0x5678) (bv 16 0x0000)
                      ],
                  MaskValue $ Mask 4 $ VectorReg (bv 16 0x57) (bv 16 0)
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x1bc6) (bv 16 0),
                          VectorReg (bv 16 0x5679) (bv 16 0)
                        ] ::
                      Value 'C
                  ]
            },
          SemanticsTest
            { semanticsTestName = "Slide1downVX",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = slide1 @'C vectorConfigEF2M2 mkSlideDown pd pm ScalarRHS,
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 6)) tumuPolicy false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x9abc) (bv 16 0x0000),
                        VectorReg (bv 16 0xdefb) (bv 16 0x0000)
                      ],
                  ScalarValue $ Scalar (bv 4 6) false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x1234) (bv 16 0x0000),
                        VectorReg (bv 16 0x5678) (bv 16 0x0000)
                      ],
                  MaskValue $ Mask 4 $ VectorReg (bv 16 0x37) (bv 16 0)
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x19ab) (bv 16 0x0000),
                          VectorReg (bv 16 0x566f) (bv 16 0x0000)
                        ] ::
                      Value 'C
                  ]
            },
          SemanticsTest
            { semanticsTestName = "Slide1upVI",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = slide1 @'C vectorConfigEF2M2 mkSlideUp pd pm (ImmRHS (ConstImm 6)),
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 6)) tumuPolicy false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x9abc) (bv 16 0x0000),
                        VectorReg (bv 16 0xdefb) (bv 16 0x0000)
                      ],
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x1234) (bv 16 0x0000),
                        VectorReg (bv 16 0x5678) (bv 16 0x0000)
                      ],
                  MaskValue $ Mask 4 $ VectorReg (bv 16 0x57) (bv 16 0)
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x1bce) (bv 16 0),
                          VectorReg (bv 16 0x5679) (bv 16 0)
                        ] ::
                      Value 'C
                  ]
            },
          SemanticsTest
            { semanticsTestName = "Slide1downVI",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = slide1 @'C vectorConfigEF2M2 mkSlideDown pd pm (ImmRHS (ConstImm 6)),
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 6)) tumuPolicy false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x9abc) (bv 16 0x0000),
                        VectorReg (bv 16 0xdefb) (bv 16 0x0000)
                      ],
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x1234) (bv 16 0x0000),
                        VectorReg (bv 16 0x5678) (bv 16 0x0000)
                      ],
                  MaskValue $ Mask 4 $ VectorReg (bv 16 0x37) (bv 16 0)
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x19ab) (bv 16 0x0000),
                          VectorReg (bv 16 0x56ef) (bv 16 0x0000)
                        ] ::
                      Value 'C
                  ]
            }
        ],
      testGroup "Typing" . fmap typingTest $
        [ TypingTest
            { typingTestName = "SlideupVX",
              typingTestOp = slide @'C vectorConfigEF2M2 mkSlideUp pd pm ScalarRHS,
              typingTestExpected =
                TypeSignature
                  { argTypes =
                      [ VLType 4,
                        VectorType vectorConfigEF2M2,
                        ScalarType 1,
                        VectorType vectorConfigEF2M2,
                        MaskType 4
                      ],
                    resTypes = [VectorType vectorConfigEF2M2]
                  }
            },
          TypingTest
            { typingTestName = "SlidedownVX",
              typingTestOp = slide @'C vectorConfigEF2M2 mkSlideDown pd pm ScalarRHS,
              typingTestExpected =
                TypeSignature
                  { argTypes =
                      [ VLType 4,
                        VectorType vectorConfigEF2M2,
                        ScalarType 1,
                        VectorType vectorConfigEF2M2,
                        MaskType 4
                      ],
                    resTypes = [VectorType vectorConfigEF2M2]
                  }
            },
          TypingTest
            { typingTestName = "SlideupVI",
              typingTestOp =
                slide @'C vectorConfigEF2M2 mkSlideUp ud pm (ImmRHS (ConstImm 2)),
              typingTestExpected =
                TypeSignature
                  { argTypes =
                      [ VLType 4,
                        VectorType vectorConfigEF2M2,
                        MaskType 4
                      ],
                    resTypes = [VectorType vectorConfigEF2M2]
                  }
            },
          TypingTest
            { typingTestName = "SlidedownVI",
              typingTestOp =
                slide @'C vectorConfigEF2M2 mkSlideDown ud pm (ImmRHS (ConstImm 2)),
              typingTestExpected =
                TypeSignature
                  { argTypes =
                      [ VLType 4,
                        VectorType vectorConfigEF2M2,
                        MaskType 4
                      ],
                    resTypes = [VectorType vectorConfigEF2M2]
                  }
            },
          TypingTest
            { typingTestName = "Slide1upVX",
              typingTestOp = slide1 @'C vectorConfigEF2M2 mkSlideUp pd pm ScalarRHS,
              typingTestExpected =
                TypeSignature
                  { argTypes =
                      [ VLType 4,
                        VectorType vectorConfigEF2M2,
                        ScalarType (1 / 2),
                        VectorType vectorConfigEF2M2,
                        MaskType 4
                      ],
                    resTypes = [VectorType vectorConfigEF2M2]
                  }
            },
          TypingTest
            { typingTestName = "Slide1downVX",
              typingTestOp = slide1 @'C vectorConfigEF2M2 mkSlideDown pd pm ScalarRHS,
              typingTestExpected =
                TypeSignature
                  { argTypes =
                      [ VLType 4,
                        VectorType vectorConfigEF2M2,
                        ScalarType (1 / 2),
                        VectorType vectorConfigEF2M2,
                        MaskType 4
                      ],
                    resTypes = [VectorType vectorConfigEF2M2]
                  }
            },
          TypingTest
            { typingTestName = "Slide1upVI",
              typingTestOp = slide1 @'C vectorConfigEF2M2 mkSlideUp pd pm (ImmRHS (ConstImm 1)),
              typingTestExpected =
                TypeSignature
                  { argTypes =
                      [ VLType 4,
                        VectorType vectorConfigEF2M2,
                        VectorType vectorConfigEF2M2,
                        MaskType 4
                      ],
                    resTypes = [VectorType vectorConfigEF2M2]
                  }
            },
          TypingTest
            { typingTestName = "Slide1downVX",
              typingTestOp = slide1 @'C vectorConfigEF2M2 mkSlideDown pd pm (ImmRHS (ConstImm 1)),
              typingTestExpected =
                TypeSignature
                  { argTypes =
                      [ VLType 4,
                        VectorType vectorConfigEF2M2,
                        VectorType vectorConfigEF2M2,
                        MaskType 4
                      ],
                    resTypes = [VectorType vectorConfigEF2M2]
                  }
            }
        ]
    ]

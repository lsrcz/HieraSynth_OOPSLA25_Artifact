{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.OpSemantics.SingleWidthTest
  ( singleWidthTest,
  )
where

import Grisette (BV (bv), LogicalOp (false))
import HieraSynth.TypeSignature
  ( TypeSignature (TypeSignature, argTypes, resTypes),
  )
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Imm (Imm (ConstImm))
import RVV.Semantics.Policy (tumuPolicy)
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
import RVV.Synthesizer.Operator.Common.RHSSpec
  ( RHSSpec (FullScalarRHS, ImmRHS, ScalarRHS, VectorRHS),
  )
import RVV.Synthesizer.Operator.SingleWidthIntBinary (singleWidthIntBinary)
import RVV.Synthesizer.Parameter.Destination (pd, ud)
import RVV.Synthesizer.Parameter.Masking (fm, pm)
import RVV.Synthesizer.Parameter.SingleWidthIntBinaryOpCode (mkAdd, mkSll)
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

-- x is uninitialized mask
-- i is uninitialized element
-- m is masked out element
-- t is tail
-- msb -> ttimx.m. <- lsb
concreteDest :: Vector 'C
concreteDest =
  Vector
    vectorConfigEF2M2
    [ VectorReg (bv 16 0xcccc) (bv 16 0),
      VectorReg (bv 16 0xcccc) (bv 16 0)
    ]

concreteLhs :: Vector 'C
concreteLhs =
  Vector
    vectorConfigEF2M2
    [ VectorReg (bv 16 0x5432) (bv 16 0),
      VectorReg (bv 16 0x9876) (bv 16 0x00f0)
    ]

concreteRhs :: Vector 'C
concreteRhs =
  Vector
    vectorConfigEF2M2
    [ VectorReg (bv 16 0xea1f) (bv 16 0),
      VectorReg (bv 16 0xa59d) (bv 16 0)
    ]

concreteMask :: Mask 'C
concreteMask = Mask 4 $ VectorReg (bv 16 0x0025) (bv 16 0x0008)

concreteVL :: VL 'C
concreteVL = VL 4 (VLNum (bv 8 0x0006)) tumuPolicy false

singleWidthTest :: Test
singleWidthTest =
  testGroup
    "SingleWidthIntBinary"
    [ testGroup "Semantics" . fmap semanticsTest $
        [ SemanticsTest
            { semanticsTestName = "VectorRHS/all inputs",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = singleWidthIntBinary @'C vectorConfigEF2M2 mkAdd pd pm VectorRHS,
              semanticsTestInput =
                [ VLValue concreteVL,
                  VectorValue concreteLhs,
                  VectorValue concreteRhs,
                  VectorValue concreteDest,
                  MaskValue concreteMask
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0xcec1) (bv 16 0xf000),
                          VectorReg (bv 16 0xcc0c) (bv 16 0x00f0)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "VectorRHS/use undefined dest",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = singleWidthIntBinary @'C vectorConfigEF2M2 mkAdd ud pm VectorRHS,
              semanticsTestInput =
                [ VLValue concreteVL,
                  VectorValue concreteLhs,
                  VectorValue concreteRhs,
                  MaskValue concreteMask
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x0e01) (bv 16 0xf0f0),
                          VectorReg (bv 16 0x0000) (bv 16 0xffff)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "VectorRHS/use full mask",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = singleWidthIntBinary @'C vectorConfigEF2M2 mkAdd pd fm VectorRHS,
              semanticsTestInput =
                [ VLValue concreteVL,
                  VectorValue concreteLhs,
                  VectorValue concreteRhs,
                  VectorValue concreteDest
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x3e41) (bv 16 0x0000),
                          VectorReg (bv 16 0xcc03) (bv 16 0x00f0)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName =
                "VectorRHS/use full mask and undefined dest",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = singleWidthIntBinary @'C vectorConfigEF2M2 mkAdd ud fm VectorRHS,
              semanticsTestInput =
                [ VLValue concreteVL,
                  VectorValue concreteLhs,
                  VectorValue concreteRhs
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x3e41) (bv 16 0x0000),
                          VectorReg (bv 16 0x0003) (bv 16 0xfff0)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "ScalarRHS",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = singleWidthIntBinary @'C vectorConfigEF2M2 mkAdd pd pm ScalarRHS,
              semanticsTestInput =
                [ VLValue concreteVL,
                  VectorValue concreteLhs,
                  ScalarValue $ Scalar (bv 4 0x19) false,
                  VectorValue concreteDest,
                  MaskValue concreteMask
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0xcdcb) (bv 16 0xf000),
                          VectorReg (bv 16 0xcc0c) (bv 16 0x00f0)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "FullScalarRHS",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = singleWidthIntBinary @'C vectorConfigEF2M2 mkSll pd pm FullScalarRHS,
              semanticsTestInput =
                [ VLValue concreteVL,
                  VectorValue concreteLhs,
                  ScalarValue $ Scalar (bv 8 0x1) false,
                  VectorValue concreteDest,
                  MaskValue concreteMask
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0xc8c4) (bv 16 0xf000),
                          VectorReg (bv 16 0xccec) (bv 16 0x00f0)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "ImmRHS/positive imm",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = singleWidthIntBinary @'C vectorConfigEF2M2 mkAdd pd pm (ImmRHS (ConstImm 0b011)),
              semanticsTestInput =
                [ VLValue concreteVL,
                  VectorValue concreteLhs,
                  VectorValue concreteDest,
                  MaskValue concreteMask
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0xc7c5) (bv 16 0xf000),
                          VectorReg (bv 16 0xccac) (bv 16 0x00f0)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "ImmRHS/negative imm",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = singleWidthIntBinary @'C vectorConfigEF2M2 mkAdd pd pm (ImmRHS (ConstImm 0b101)),
              semanticsTestInput =
                [ VLValue concreteVL,
                  VectorValue concreteLhs,
                  VectorValue concreteDest,
                  MaskValue concreteMask
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0xc1cf) (bv 16 0xf000),
                          VectorReg (bv 16 0xcc4c) (bv 16 0x00f0)
                        ]
                  ]
            }
        ],
      testGroup "Typing" . fmap typingTest $
        [ TypingTest
            { typingTestName = "VectorRHS",
              typingTestOp = singleWidthIntBinary @'C vectorConfigEF2M2 mkAdd pd pm VectorRHS,
              typingTestExpected =
                TypeSignature
                  { argTypes =
                      [ VLType 4,
                        VectorType vectorConfigEF2M2,
                        VectorType vectorConfigEF2M2,
                        VectorType vectorConfigEF2M2,
                        MaskType 4
                      ],
                    resTypes = [VectorType vectorConfigEF2M2]
                  }
            },
          TypingTest
            { typingTestName = "VectorRHS/use undefined dest",
              typingTestOp = singleWidthIntBinary @'C vectorConfigEF2M2 mkAdd ud pm VectorRHS,
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
            { typingTestName = "VectorRHS/use full mask",
              typingTestOp = singleWidthIntBinary @'C vectorConfigEF2M2 mkAdd pd fm VectorRHS,
              typingTestExpected =
                TypeSignature
                  { argTypes =
                      [ VLType 4,
                        VectorType vectorConfigEF2M2,
                        VectorType vectorConfigEF2M2,
                        VectorType vectorConfigEF2M2
                      ],
                    resTypes = [VectorType vectorConfigEF2M2]
                  }
            },
          TypingTest
            { typingTestName = "VectorRHS/use full mask and undefined dest",
              typingTestOp = singleWidthIntBinary @'C vectorConfigEF2M2 mkAdd ud fm VectorRHS,
              typingTestExpected =
                TypeSignature
                  { argTypes =
                      [ VLType 4,
                        VectorType vectorConfigEF2M2,
                        VectorType vectorConfigEF2M2
                      ],
                    resTypes = [VectorType vectorConfigEF2M2]
                  }
            },
          TypingTest
            { typingTestName = "ScalarRHS",
              typingTestOp = singleWidthIntBinary @'C vectorConfigEF2M2 mkAdd pd pm ScalarRHS,
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
            { typingTestName = "FullScalarRHS",
              typingTestOp = singleWidthIntBinary @'C vectorConfigEF2M2 mkSll pd pm FullScalarRHS,
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
            { typingTestName = "ImmRHS",
              typingTestOp = singleWidthIntBinary @'C vectorConfigEF2M2 mkAdd pd pm (ImmRHS (ConstImm 3)),
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

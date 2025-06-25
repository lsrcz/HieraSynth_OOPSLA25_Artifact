{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.OpSemantics.NarrowingRightShiftTest
  ( narrowingRightShiftTest,
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
  ( Scalar (Scalar),
    VL (VL),
    VLValue (VLNum),
    Vector (Vector),
    VectorReg (VectorReg),
  )
import RVV.Semantics.VectorConfigConstants (vconst8162, vectorConfigE1M4, vectorConfigEF2M2)
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
import RVV.Synthesizer.Operator.Common.RHSSpec (RHSSpec (ImmRHS, ScalarRHS, VectorRHS))
import RVV.Synthesizer.Operator.NarrowingRightShift (narrowingRightShift)
import RVV.Synthesizer.Parameter.Destination (pd, ud)
import RVV.Synthesizer.Parameter.Masking (fm, pm)
import RVV.Synthesizer.Parameter.NarrowingRightShiftOpCode (mkNSra)
import RVV.Synthesizer.Type
  ( ValueType (MaskType, ScalarType, VLType, VectorType),
  )
import RVV.Synthesizer.Value (Value (ScalarValue, VLValue, VectorValue))
import Test.Framework (Test, testGroup)

concreteLhs :: Vector 'C
concreteLhs =
  Vector
    vectorConfigE1M4
    [ VectorReg (bv 16 0xa832) (bv 16 0),
      VectorReg (bv 16 0x9876) (bv 16 0x00f0),
      VectorReg (bv 16 0xa832) (bv 16 0xff00),
      VectorReg (bv 16 0x9876) (bv 16 0)
    ]

concreteRhs :: Vector 'C
concreteRhs =
  Vector
    vectorConfigEF2M2
    [ VectorReg (bv 16 0x1256) (bv 16 0),
      VectorReg (bv 16 0x0123) (bv 16 0)
    ]

concreteVL :: VL 'C
concreteVL = VL 4 (VLNum (bv 8 0x0006)) tumuPolicy false

narrowingRightShiftTest :: Test
narrowingRightShiftTest =
  testGroup
    "NarrowingRightShift"
    [ testGroup "Semantics" . fmap semanticsTest $
        [ SemanticsTest
            { semanticsTestName = "narrowingRightShiftWV",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = narrowingRightShift @'C vectorConfigEF2M2 mkNSra ud fm VectorRHS,
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
                        [ VectorReg (bv 16 0xcdd0) (bv 16 0x0c00),
                          VectorReg (bv 16 0x00a6) (bv 16 0xfff0)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "narrowingRightShiftWX",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = narrowingRightShift @'C vectorConfigEF2M2 mkNSra ud fm ScalarRHS,
              semanticsTestInput =
                [ VLValue concreteVL,
                  VectorValue concreteLhs,
                  ScalarValue $ Scalar (bv 8 2) false
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x6dac) (bv 16 0x0c00),
                          VectorReg (bv 16 0x00ac) (bv 16 0xfff0)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "narrowingRightShiftWI",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = narrowingRightShift @'C vectorConfigEF2M2 mkNSra ud fm (ImmRHS (ConstImm 2)),
              semanticsTestInput =
                [ VLValue concreteVL,
                  VectorValue concreteLhs
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x6dac) (bv 16 0x0c00),
                          VectorReg (bv 16 0x00ac) (bv 16 0xfff0)
                        ]
                  ]
            }
        ],
      testGroup "Typing" . fmap typingTest $
        [ TypingTest
            { typingTestName = "narrowingRightShiftWV",
              typingTestOp = narrowingRightShift @'C vectorConfigEF2M2 mkNSra pd fm VectorRHS,
              typingTestExpected =
                TypeSignature
                  { argTypes =
                      [ VLType 4,
                        VectorType vectorConfigE1M4,
                        VectorType vectorConfigEF2M2,
                        VectorType vectorConfigEF2M2
                      ],
                    resTypes = [VectorType vectorConfigEF2M2]
                  }
            },
          TypingTest
            { typingTestName = "narrowingRightShiftWX",
              typingTestOp = narrowingRightShift @'C vectorConfigEF2M2 mkNSra pd pm ScalarRHS,
              typingTestExpected =
                TypeSignature
                  { argTypes =
                      [ VLType 4,
                        VectorType vectorConfigE1M4,
                        ScalarType 1,
                        VectorType vectorConfigEF2M2,
                        MaskType 4
                      ],
                    resTypes = [VectorType vectorConfigEF2M2]
                  }
            },
          TypingTest
            { typingTestName = "narrowingRightShiftWI",
              typingTestOp = narrowingRightShift @'C vectorConfigEF2M2 mkNSra ud fm (ImmRHS (ConstImm 1)),
              typingTestExpected =
                TypeSignature
                  { argTypes =
                      [ VLType 4,
                        VectorType vectorConfigE1M4
                      ],
                    resTypes = [VectorType vectorConfigEF2M2]
                  }
            }
        ]
    ]

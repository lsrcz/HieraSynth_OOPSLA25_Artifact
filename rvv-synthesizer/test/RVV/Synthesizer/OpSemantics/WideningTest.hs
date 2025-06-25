{-# LANGUAGE DataKinds #-}

module RVV.Synthesizer.OpSemantics.WideningTest (wideningTest) where

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
import RVV.Semantics.VectorConfigConstants
  ( vconst8162,
    vectorConfigE1M4,
    vectorConfigEF2M2,
  )
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
  ( RHSSpec (ImmRHS, ScalarRHS, VectorRHS),
  )
import RVV.Synthesizer.Operator.WideningIntBinary (wideningIntBinary)
import RVV.Synthesizer.Parameter.Destination (pd, ud)
import RVV.Synthesizer.Parameter.Masking (fm)
import RVV.Synthesizer.Parameter.WideningIntBinaryOpCode (mkWAdd, mkWMulsu)
import RVV.Synthesizer.Type
  ( ValueType (ScalarType, VLType, VectorType),
  )
import RVV.Synthesizer.Value (Value (ScalarValue, VLValue, VectorValue))
import Test.Framework (Test, testGroup)

concreteLhs :: Vector 'C
concreteLhs =
  Vector
    vectorConfigEF2M2
    [ VectorReg (bv 16 0xa832) (bv 16 0),
      VectorReg (bv 16 0x9876) (bv 16 0x00f0)
    ]

concreteWideLhs :: Vector 'C
concreteWideLhs =
  Vector
    vectorConfigE1M4
    [ VectorReg (bv 16 0xa832) (bv 16 0),
      VectorReg (bv 16 0x9876) (bv 16 0),
      VectorReg (bv 16 0xa832) (bv 16 0xff00),
      VectorReg (bv 16 0x9876) (bv 16 0)
    ]

concreteRhs :: Vector 'C
concreteRhs =
  Vector
    vectorConfigEF2M2
    [ VectorReg (bv 16 0xe51f) (bv 16 0),
      VectorReg (bv 16 0xa59d) (bv 16 0)
    ]

concreteVL :: VL 'C
concreteVL = VL 4 (VLNum (bv 8 0x0006)) tumuPolicy false

wideningTest :: Test
wideningTest =
  testGroup
    "Widening"
    [ testGroup "Semantics" . fmap semanticsTest $
        [ SemanticsTest
            { semanticsTestName = "widening/narrowLHS/VectorRHS",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = wideningIntBinary vectorConfigE1M4 mkWMulsu ud fm False VectorRHS,
              semanticsTestInput =
                [ VLValue concreteVL,
                  VectorValue concreteLhs,
                  VectorValue concreteRhs
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigE1M4
                        [ VectorReg (bv 16 0x031e) (bv 16 0x0000),
                          VectorReg (bv 16 0xacd8) (bv 16 0x0000),
                          VectorReg (bv 16 0x3f4e) (bv 16 0xff00),
                          VectorReg (bv 16 0x0000) (bv 16 0xffff)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "widening/narrowLHS/ScalarRHS",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = wideningIntBinary vectorConfigE1M4 mkWMulsu ud fm False ScalarRHS,
              semanticsTestInput =
                [ VLValue concreteVL,
                  VectorValue concreteLhs,
                  ScalarValue $ Scalar (bv 4 0x9) false
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigE1M4
                        [ VectorReg (bv 16 0x1b12) (bv 16 0x0000),
                          VectorReg (bv 16 0xcab8) (bv 16 0x0000),
                          VectorReg (bv 16 0x3f36) (bv 16 0xff00),
                          VectorReg (bv 16 0x0000) (bv 16 0xffff)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "widening/narrowLHS/VectorRHS",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = wideningIntBinary vectorConfigE1M4 mkWMulsu ud fm False (ImmRHS (ConstImm 2)),
              semanticsTestInput =
                [ VLValue concreteVL,
                  VectorValue concreteLhs
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigE1M4
                        [ VectorReg (bv 16 0x0604) (bv 16 0x0000),
                          VectorReg (bv 16 0xf4f0) (bv 16 0x0000),
                          VectorReg (bv 16 0x0e0c) (bv 16 0xff00),
                          VectorReg (bv 16 0x0000) (bv 16 0xffff)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "widening/wideLHS/VectorRHS",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = wideningIntBinary vectorConfigE1M4 mkWAdd ud fm True VectorRHS,
              semanticsTestInput =
                [ VLValue concreteVL,
                  VectorValue concreteWideLhs,
                  VectorValue concreteRhs
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigE1M4
                        [ VectorReg (bv 16 0xa931) (bv 16 0x0000),
                          VectorReg (bv 16 0x967b) (bv 16 0x0000),
                          VectorReg (bv 16 0xa12f) (bv 16 0xff00),
                          VectorReg (bv 16 0x0000) (bv 16 0xffff)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "widening/wideLHS/ScalarRHS",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = wideningIntBinary vectorConfigE1M4 mkWAdd ud fm True ScalarRHS,
              semanticsTestInput =
                [ VLValue concreteVL,
                  VectorValue concreteWideLhs,
                  ScalarValue $ Scalar (bv 4 9) false
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigE1M4
                        [ VectorReg (bv 16 0xa12b) (bv 16 0x0000),
                          VectorReg (bv 16 0x916f) (bv 16 0x0000),
                          VectorReg (bv 16 0xa12b) (bv 16 0xff00),
                          VectorReg (bv 16 0x0000) (bv 16 0xffff)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "widening/wideLHS/ImmRHS",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = wideningIntBinary vectorConfigE1M4 mkWAdd ud fm True (ImmRHS (ConstImm 4)),
              semanticsTestInput =
                [ VLValue concreteVL,
                  VectorValue concreteWideLhs
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigE1M4
                        [ VectorReg (bv 16 0xa42e) (bv 16 0x0000),
                          VectorReg (bv 16 0x9472) (bv 16 0x0000),
                          VectorReg (bv 16 0xa42e) (bv 16 0xff00),
                          VectorReg (bv 16 0x0000) (bv 16 0xffff)
                        ]
                  ]
            }
        ],
      testGroup "Typing" . fmap typingTest $
        [ TypingTest
            { typingTestName = "widening/narrowLHS/VectorRHS",
              typingTestOp = wideningIntBinary vectorConfigE1M4 mkWMulsu pd fm False VectorRHS,
              typingTestExpected =
                TypeSignature
                  { argTypes =
                      [ VLType 4,
                        VectorType vectorConfigEF2M2,
                        VectorType vectorConfigEF2M2,
                        VectorType vectorConfigE1M4
                      ],
                    resTypes = [VectorType vectorConfigE1M4]
                  }
            },
          TypingTest
            { typingTestName = "widening/narrowLHS/ScalarRHS",
              typingTestOp = wideningIntBinary vectorConfigE1M4 mkWMulsu pd fm False ScalarRHS,
              typingTestExpected =
                TypeSignature
                  { argTypes =
                      [ VLType 4,
                        VectorType vectorConfigEF2M2,
                        ScalarType (1 / 2),
                        VectorType vectorConfigE1M4
                      ],
                    resTypes = [VectorType vectorConfigE1M4]
                  }
            },
          TypingTest
            { typingTestName = "widening/narrowLHS/ImmRHS",
              typingTestOp = wideningIntBinary vectorConfigE1M4 mkWMulsu pd fm False (ImmRHS (ConstImm 1)),
              typingTestExpected =
                TypeSignature
                  { argTypes =
                      [ VLType 4,
                        VectorType vectorConfigEF2M2,
                        VectorType vectorConfigE1M4
                      ],
                    resTypes = [VectorType vectorConfigE1M4]
                  }
            },
          TypingTest
            { typingTestName = "widening/wideLHS/VectorRHS",
              typingTestOp = wideningIntBinary vectorConfigE1M4 mkWAdd ud fm True VectorRHS,
              typingTestExpected =
                TypeSignature
                  { argTypes =
                      [ VLType 4,
                        VectorType vectorConfigE1M4,
                        VectorType vectorConfigEF2M2
                      ],
                    resTypes = [VectorType vectorConfigE1M4]
                  }
            },
          TypingTest
            { typingTestName = "widening/wideLHS/ScalarRHS",
              typingTestOp = wideningIntBinary vectorConfigE1M4 mkWAdd ud fm True ScalarRHS,
              typingTestExpected =
                TypeSignature
                  { argTypes =
                      [ VLType 4,
                        VectorType vectorConfigE1M4,
                        ScalarType (1 / 2)
                      ],
                    resTypes = [VectorType vectorConfigE1M4]
                  }
            },
          TypingTest
            { typingTestName = "widening/wideLHS/ImmRHS",
              typingTestOp = wideningIntBinary vectorConfigE1M4 mkWAdd ud fm True (ImmRHS (ConstImm 1)),
              typingTestExpected =
                TypeSignature
                  { argTypes =
                      [ VLType 4,
                        VectorType vectorConfigE1M4
                      ],
                    resTypes = [VectorType vectorConfigE1M4]
                  }
            }
        ]
    ]

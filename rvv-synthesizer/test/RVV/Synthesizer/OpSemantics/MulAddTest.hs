{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.OpSemantics.MulAddTest (mulAddTest) where

import Grisette (BV (bv), LogicalOp (false), SomeWordN)
import HieraSynth.TypeSignature
  ( TypeSignature (TypeSignature, argTypes, resTypes),
  )
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Imm (Imm (ConstImm))
import RVV.Semantics.Policy (tuPolicy)
import RVV.Semantics.Value
  ( Mask (Mask),
    Scalar (Scalar),
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
import RVV.Synthesizer.Operator.SingleWidthMulAdd (singleWidthMulAdd)
import RVV.Synthesizer.Operator.WideningMulAdd (wideningMulAdd)
import RVV.Synthesizer.Parameter.Masking (pm)
import RVV.Synthesizer.Parameter.SingleWidthMulAddOpCode
  ( mkMAcc,
  )
import RVV.Synthesizer.Parameter.WideningMulAddOpCode
  ( mkWMAcc,
    mkWMAccsu,
  )
import RVV.Synthesizer.Type
  ( ValueType (MaskType, ScalarType, VLType, VectorType),
  )
import RVV.Synthesizer.Value
  ( Value (MaskValue, ScalarValue, VLValue, VectorValue),
  )
import Test.Framework (Test, testGroup)

mulAddTest :: Test
mulAddTest =
  testGroup
    "MulAdd"
    [ testGroup "Semantics" . fmap semanticsTest $
        [ SemanticsTest
            { semanticsTestName = "SingleWidthMulAdd[vector_rhs]",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp =
                singleWidthMulAdd @'C vectorConfigEF2M2 mkMAcc pm VectorRHS,
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 0x7 :: SomeWordN)) tuPolicy false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x1234) (bv 16 0x0000),
                        VectorReg (bv 16 0x2345) (bv 16 0x0000)
                      ],
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x15ae) (bv 16 0x0000),
                        VectorReg (bv 16 0x4cf3) (bv 16 0x0000)
                      ],
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x4321) (bv 16 0),
                        VectorReg (bv 16 0x8765) (bv 16 0x0f00)
                      ],
                  MaskValue $ Mask 4 $ VectorReg (bv 16 0x0067) (bv 16 0)
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x0d09) (bv 16 0xf000),
                          VectorReg (bv 16 0x8020) (bv 16 0x0f0f)
                        ] ::
                      Value 'C
                  ]
            },
          SemanticsTest
            { semanticsTestName = "WideningMulAdd[vector_rhs]",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp =
                wideningMulAdd @'C vectorConfigE1M4 mkWMAccsu pm VectorRHS,
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 0x7 :: SomeWordN)) tuPolicy false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0xf1e4) (bv 16 0x0000),
                        VectorReg (bv 16 0x2345) (bv 16 0x0000)
                      ],
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x15ae) (bv 16 0x0000),
                        VectorReg (bv 16 0x4cf3) (bv 16 0x0000)
                      ],
                  VectorValue $
                    Vector
                      vectorConfigE1M4
                      [ VectorReg (bv 16 0x4321) (bv 16 0),
                        VectorReg (bv 16 0x8765) (bv 16 0),
                        VectorReg (bv 16 0xcba9) (bv 16 0),
                        VectorReg (bv 16 0x0fed) (bv 16 0xff)
                      ],
                  MaskValue $ Mask 4 $ VectorReg (bv 16 0x0067) (bv 16 0)
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigE1M4
                        [ VectorReg (bv 16 0x2f59) (bv 16 0x0000),
                          VectorReg (bv 16 0x006a) (bv 16 0xff00),
                          VectorReg (bv 16 0x0700) (bv 16 0x00ff),
                          VectorReg (bv 16 0x0f00) (bv 16 0x00ff)
                        ] ::
                      Value 'C
                  ]
            },
          SemanticsTest
            { semanticsTestName = "SingleWidthMulAdd[scalar_rhs]",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp =
                singleWidthMulAdd @'C vectorConfigEF2M2 mkMAcc pm ScalarRHS,
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 0x7 :: SomeWordN)) tuPolicy false,
                  ScalarValue $ Scalar (bv 4 7) false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x15ae) (bv 16 0x0000),
                        VectorReg (bv 16 0x4cf3) (bv 16 0x0000)
                      ],
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x4321) (bv 16 0),
                        VectorReg (bv 16 0x8765) (bv 16 0x0f00)
                      ],
                  MaskValue $ Mask 4 $ VectorReg (bv 16 0x0067) (bv 16 0)
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x0683) (bv 16 0xf000),
                          VectorReg (bv 16 0x80f0) (bv 16 0x0f0f)
                        ] ::
                      Value 'C
                  ]
            },
          SemanticsTest
            { semanticsTestName = "WideningMulAdd[scalar_rhs]",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp =
                wideningMulAdd @'C vectorConfigE1M4 mkWMAccsu pm ScalarRHS,
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 0x7 :: SomeWordN)) tuPolicy false,
                  ScalarValue $ Scalar (bv 4 9) false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x15ae) (bv 16 0x0000),
                        VectorReg (bv 16 0x4cf3) (bv 16 0x0000)
                      ],
                  VectorValue $
                    Vector
                      vectorConfigE1M4
                      [ VectorReg (bv 16 0x4321) (bv 16 0),
                        VectorReg (bv 16 0x8765) (bv 16 0),
                        VectorReg (bv 16 0xcba9) (bv 16 0),
                        VectorReg (bv 16 0x0fed) (bv 16 0xff)
                      ],
                  MaskValue $ Mask 4 $ VectorReg (bv 16 0x0067) (bv 16 0)
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigE1M4
                        [ VectorReg (bv 16 0xfdbf) (bv 16 0x0000),
                          VectorReg (bv 16 0x0042) (bv 16 0xff00),
                          VectorReg (bv 16 0x6200) (bv 16 0x00ff),
                          VectorReg (bv 16 0x0f00) (bv 16 0x00ff)
                        ] ::
                      Value 'C
                  ]
            },
          SemanticsTest
            { semanticsTestName = "SingleWidthMulAdd[imm_rhs]",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp =
                singleWidthMulAdd @'C vectorConfigEF2M2 mkMAcc pm (ImmRHS (ConstImm 2)),
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 0x7 :: SomeWordN)) tuPolicy false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x15ae) (bv 16 0x0000),
                        VectorReg (bv 16 0x4cf3) (bv 16 0x0000)
                      ],
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x4321) (bv 16 0),
                        VectorReg (bv 16 0x8765) (bv 16 0x0f00)
                      ],
                  MaskValue $ Mask 4 $ VectorReg (bv 16 0x0067) (bv 16 0)
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x0d6d) (bv 16 0xf000),
                          VectorReg (bv 16 0x8040) (bv 16 0x0f0f)
                        ] ::
                      Value 'C
                  ]
            },
          SemanticsTest
            { semanticsTestName = "WideningMulAdd[imm_rhs]",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp =
                wideningMulAdd @'C vectorConfigE1M4 mkWMAccsu pm (ImmRHS (ConstImm 2)),
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 0x7 :: SomeWordN)) tuPolicy false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x15ae) (bv 16 0x0000),
                        VectorReg (bv 16 0x4cf3) (bv 16 0x0000)
                      ],
                  VectorValue $
                    Vector
                      vectorConfigE1M4
                      [ VectorReg (bv 16 0x4321) (bv 16 0),
                        VectorReg (bv 16 0x8765) (bv 16 0),
                        VectorReg (bv 16 0xcba9) (bv 16 0),
                        VectorReg (bv 16 0x0fed) (bv 16 0xff)
                      ],
                  MaskValue $ Mask 4 $ VectorReg (bv 16 0x0067) (bv 16 0)
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigE1M4
                        [ VectorReg (bv 16 0x573d) (bv 16 0x0000),
                          VectorReg (bv 16 0x006f) (bv 16 0xff00),
                          VectorReg (bv 16 0xe900) (bv 16 0x00ff),
                          VectorReg (bv 16 0x0f00) (bv 16 0x00ff)
                        ] ::
                      Value 'C
                  ]
            }
        ],
      testGroup "Typing" . fmap typingTest $
        [ TypingTest
            { typingTestName = "SingleWidthMulAdd[vector_rhs]",
              typingTestOp = singleWidthMulAdd @'C vectorConfigEF2M2 mkMAcc pm VectorRHS,
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
            { typingTestName = "WideningMulAdd[vector_rhs]",
              typingTestOp = wideningMulAdd @'C vectorConfigE1M4 mkWMAcc pm VectorRHS,
              typingTestExpected =
                TypeSignature
                  { argTypes =
                      [ VLType 4,
                        VectorType vectorConfigEF2M2,
                        VectorType vectorConfigEF2M2,
                        VectorType vectorConfigE1M4,
                        MaskType 4
                      ],
                    resTypes = [VectorType vectorConfigE1M4]
                  }
            },
          TypingTest
            { typingTestName = "SingleWidthMulAdd[scalar_rhs]",
              typingTestOp =
                singleWidthMulAdd @'C vectorConfigEF2M2 mkMAcc pm ScalarRHS,
              typingTestExpected =
                TypeSignature
                  { argTypes =
                      [ VLType 4,
                        ScalarType (1 / 2),
                        VectorType vectorConfigEF2M2,
                        VectorType vectorConfigEF2M2,
                        MaskType 4
                      ],
                    resTypes = [VectorType vectorConfigEF2M2]
                  }
            },
          TypingTest
            { typingTestName = "WideningMulAdd[scalar_rhs]",
              typingTestOp =
                wideningMulAdd @'C vectorConfigE1M4 mkWMAcc pm ScalarRHS,
              typingTestExpected =
                TypeSignature
                  { argTypes =
                      [ VLType 4,
                        ScalarType (1 / 2),
                        VectorType vectorConfigEF2M2,
                        VectorType vectorConfigE1M4,
                        MaskType 4
                      ],
                    resTypes = [VectorType vectorConfigE1M4]
                  }
            }
        ]
    ]

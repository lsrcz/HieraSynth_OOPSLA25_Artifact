{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.OpSemantics.FixedPointClipTest (fixedPointClipTest) where

import Grisette (BV (bv), LogicalOp (false))
import HieraSynth.TypeSignature
  ( TypeSignature (TypeSignature, argTypes, resTypes),
  )
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Imm (Imm (ConstImm))
import RVV.Semantics.Policy (nonePolicy)
import RVV.Semantics.Value
  ( Scalar (Scalar),
    VL (VL),
    Vector (Vector),
    VectorReg (VectorReg),
    VLValue (VLNum)
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
import RVV.Synthesizer.Operator.FixedPointClip (fixedPointClip)
import RVV.Synthesizer.Parameter.Destination (pd, ud)
import RVV.Synthesizer.Parameter.FixedPointRoundingMode (mkFixedRNU)
import RVV.Synthesizer.Parameter.Masking (fm, pm)
import RVV.Synthesizer.Type (ValueType (MaskType, ScalarType, VLType, VectorType))
import RVV.Synthesizer.Value (Value (ScalarValue, VLValue, VectorValue))
import Test.Framework (Test, testGroup)

fixedPointClipTest :: Test
fixedPointClipTest =
  testGroup
    "FixedPointClip"
    [ testGroup "Semantics" . fmap semanticsTest $
        [ SemanticsTest
            { semanticsTestName = "fixedPointClipWV",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp =
                fixedPointClip @'C vectorConfigEF2M2 True mkFixedRNU ud fm VectorRHS,
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 0x6)) nonePolicy false,
                  VectorValue $
                    Vector
                      vectorConfigE1M4
                      [ VectorReg (bv 16 0x0809) (bv 16 0),
                        VectorReg (bv 16 0x0a0b) (bv 16 0),
                        VectorReg (bv 16 0x0c0d) (bv 16 0),
                        VectorReg (bv 16 0x0e0f) (bv 16 0)
                      ],
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0xab89) (bv 16 0),
                        VectorReg (bv 16 0xefcd) (bv 16 0)
                      ]
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x3185) (bv 16 0),
                          VectorReg (bv 16 0x0010) (bv 16 0xff00)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "fixedPointClipWX",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp =
                fixedPointClip @'C vectorConfigEF2M2 True mkFixedRNU ud fm ScalarRHS,
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 0x6)) nonePolicy false,
                  VectorValue $
                    Vector
                      vectorConfigE1M4
                      [ VectorReg (bv 16 0x0809) (bv 16 0),
                        VectorReg (bv 16 0x0a0b) (bv 16 0),
                        VectorReg (bv 16 0x0c0d) (bv 16 0),
                        VectorReg (bv 16 0x0e0f) (bv 16 0)
                      ],
                  ScalarValue $ Scalar (bv 8 2) false
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x3322) (bv 16 0),
                          VectorReg (bv 16 0x0033) (bv 16 0xff00)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "fixedPointClipWI",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp =
                fixedPointClip @'C vectorConfigEF2M2 True mkFixedRNU ud fm (ImmRHS (ConstImm 2)),
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 0x6)) nonePolicy false,
                  VectorValue $
                    Vector
                      vectorConfigE1M4
                      [ VectorReg (bv 16 0x0809) (bv 16 0),
                        VectorReg (bv 16 0x0a0b) (bv 16 0),
                        VectorReg (bv 16 0x0c0d) (bv 16 0),
                        VectorReg (bv 16 0x0e0f) (bv 16 0)
                      ]
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x3322) (bv 16 0),
                          VectorReg (bv 16 0x0033) (bv 16 0xff00)
                        ]
                  ]
            }
        ],
      testGroup "Typing" . fmap typingTest $
        [ TypingTest
            { typingTestName = "FixedPointClipWV",
              typingTestOp =
                fixedPointClip @'C vectorConfigEF2M2 True mkFixedRNU pd fm VectorRHS,
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
            { typingTestName = "FixedPointClipWX",
              typingTestOp =
                fixedPointClip @'C vectorConfigEF2M2 True mkFixedRNU pd pm ScalarRHS,
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
            { typingTestName = "FixedPointClipWI",
              typingTestOp =
                fixedPointClip @'C vectorConfigEF2M2 True mkFixedRNU ud fm (ImmRHS (ConstImm 2)),
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

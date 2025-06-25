{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.OpSemantics.MergeTest (mergeTest) where

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
import RVV.Synthesizer.Operator.Common.RHSSpec (RHSSpec (ImmRHS, ScalarRHS, VectorRHS))
import RVV.Synthesizer.Operator.Merge (merge)
import RVV.Synthesizer.Parameter.Destination (pd, ud)
import RVV.Synthesizer.Type
  ( ValueType (MaskType, ScalarType, VLType, VectorType),
  )
import RVV.Synthesizer.Value
  ( Value (MaskValue, ScalarValue, VLValue, VectorValue),
  )
import Test.Framework (Test, testGroup)

mergeTest :: Test
mergeTest =
  testGroup
    "Merge"
    [ testGroup "Semantics" . fmap semanticsTest $
        [ SemanticsTest
            { semanticsTestName = "VMergeVVM",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = merge @'C vectorConfigEF2M2 pd VectorRHS,
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 6)) tuPolicy false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x1234) (bv 16 0),
                        VectorReg (bv 16 0x5678) (bv 16 0)
                      ],
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x9abc) (bv 16 0),
                        VectorReg (bv 16 0x0def) (bv 16 0)
                      ],
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0xcccc) (bv 16 0xffff),
                        VectorReg (bv 16 0xcccc) (bv 16 0xffff)
                      ],
                  MaskValue $ Mask 4 $ VectorReg (bv 16 0x00aa) (bv 16 0)
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x92b4 :: SomeWordN) (bv 16 0),
                          VectorReg (bv 16 0xcce8) (bv 16 0xff00)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "VMergeVXM",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = merge @'C vectorConfigEF2M2 ud ScalarRHS,
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 6)) tuPolicy false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x1234) (bv 16 0),
                        VectorReg (bv 16 0x5678) (bv 16 0)
                      ],
                  ScalarValue $ Scalar (bv 4 0x56) false,
                  MaskValue $ Mask 4 $ VectorReg (bv 16 0x00aa) (bv 16 0)
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x6264 :: SomeWordN) (bv 16 0),
                          VectorReg (bv 16 0x0068) (bv 16 0xff00)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "VMergeVIM",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = merge @'C vectorConfigEF2M2 ud (ImmRHS (ConstImm 4)),
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 6)) tuPolicy false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x1234) (bv 16 0),
                        VectorReg (bv 16 0x5678) (bv 16 0)
                      ],
                  MaskValue $ Mask 4 $ VectorReg (bv 16 0x00aa) (bv 16 0)
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0xc2c4 :: SomeWordN) (bv 16 0),
                          VectorReg (bv 16 0x00c8) (bv 16 0xff00)
                        ]
                  ]
            }
        ],
      testGroup "Typing" . fmap typingTest $
        [ TypingTest
            { typingTestName = "VMergeVVM",
              typingTestOp = merge @'C vectorConfigEF2M2 pd VectorRHS,
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
            { typingTestName = "VMergeVXM",
              typingTestOp = merge @'C vectorConfigEF2M2 ud ScalarRHS,
              typingTestExpected =
                TypeSignature
                  { argTypes =
                      [ VLType 4,
                        VectorType vectorConfigEF2M2,
                        ScalarType (1 / 2),
                        MaskType 4
                      ],
                    resTypes = [VectorType vectorConfigEF2M2]
                  }
            },
          TypingTest
            { typingTestName = "VMergeVIM",
              typingTestOp = merge @'C vectorConfigEF2M2 ud (ImmRHS (ConstImm 4)),
              typingTestExpected =
                TypeSignature
                  { argTypes =
                      [ VLType 4,
                        VectorType vectorConfigEF2M2,
                        MaskType 4
                      ],
                    resTypes = [VectorType vectorConfigEF2M2]
                  }
            }
        ]
    ]

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.OpSemantics.CompareTest (compareTest) where

import Grisette (BV (bv), LogicalOp (false))
import HieraSynth.TypeSignature
  ( TypeSignature (TypeSignature),
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
import RVV.Synthesizer.Operator.Common.RHSSpec (RHSSpec (ImmRHS, ScalarRHS, VectorRHS))
import RVV.Synthesizer.Operator.VectorCompare (vectorCompare)
import RVV.Synthesizer.Parameter.Destination (pd, ud)
import RVV.Synthesizer.Parameter.IntCompareOpCode (mkMSEq)
import RVV.Synthesizer.Parameter.Masking (fm, pm)
import RVV.Synthesizer.Type (ValueType (MaskType, ScalarType, VLType, VectorType))
import RVV.Synthesizer.Value (Value (MaskValue, ScalarValue, VLValue, VectorValue))
import Test.Framework (Test, testGroup)

compareTest :: Test
compareTest =
  testGroup
    "Compare"
    [ testGroup "Semantics" . fmap semanticsTest $
        [ SemanticsTest
            { semanticsTestName = "CompareVV",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = vectorCompare @'C vectorConfigEF2M2 mkMSEq pd pm VectorRHS,
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 0x07)) tumuPolicy false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x1fb4) (bv 16 0),
                        VectorReg (bv 16 0xbd25) (bv 16 0)
                      ],
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x2fac) (bv 16 0),
                        VectorReg (bv 16 0xbd35) (bv 16 0)
                      ],
                  MaskValue $ Mask 4 $ VectorReg (bv 16 0xffff) (bv 16 0),
                  MaskValue $ Mask 4 $ VectorReg (bv 16 0x1367) (bv 16 0)
                ],
              semanticsTestExpected =
                Just
                  [ MaskValue $ Mask 4 $ VectorReg (bv 16 0x5c) (bv 16 0xff80)
                  ]
            },
          SemanticsTest
            { semanticsTestName = "CompareVX",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = vectorCompare @'C vectorConfigEF2M2 mkMSEq pd fm ScalarRHS,
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 0x07)) tumuPolicy false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x1fb4) (bv 16 0),
                        VectorReg (bv 16 0xbf2f) (bv 16 0)
                      ],
                  ScalarValue $ Scalar (bv 4 0xf) false,
                  MaskValue $ Mask 4 $ VectorReg (bv 16 0xffff) (bv 16 0)
                ],
              semanticsTestExpected =
                Just
                  [ MaskValue $ Mask 4 $ VectorReg (bv 16 0x54) (bv 16 0xff80)
                  ]
            },
          SemanticsTest
            { semanticsTestName = "CompareVI",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp =
                vectorCompare @'C vectorConfigEF2M2 mkMSEq ud pm (ImmRHS (ConstImm 3)),
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 0x07)) tumuPolicy false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x13b4) (bv 16 0),
                        VectorReg (bv 16 0xb323) (bv 16 0)
                      ],
                  MaskValue $ Mask 4 $ VectorReg (bv 16 0x1367) (bv 16 0)
                ],
              semanticsTestExpected =
                Just
                  [ MaskValue $ Mask 4 $ VectorReg (bv 16 0x44) (bv 16 0xff98)
                  ]
            }
        ],
      testGroup "Typing" . fmap typingTest $
        [ TypingTest
            { typingTestName = "CompareVV",
              typingTestOp = vectorCompare @'C vectorConfigEF2M2 mkMSEq pd pm VectorRHS,
              typingTestExpected =
                TypeSignature
                  [ VLType 4,
                    VectorType vectorConfigEF2M2,
                    VectorType vectorConfigEF2M2,
                    MaskType 4,
                    MaskType 4
                  ]
                  [MaskType 4]
            },
          TypingTest
            { typingTestName = "CompareVX",
              typingTestOp = vectorCompare @'C vectorConfigEF2M2 mkMSEq ud pm ScalarRHS,
              typingTestExpected =
                TypeSignature
                  [ VLType 4,
                    VectorType vectorConfigEF2M2,
                    ScalarType (1 / 2),
                    MaskType 4
                  ]
                  [MaskType 4]
            },
          TypingTest
            { typingTestName = "CompareVI",
              typingTestOp = vectorCompare @'C vectorConfigEF2M2 mkMSEq ud fm (ImmRHS (ConstImm 3)),
              typingTestExpected =
                TypeSignature
                  [ VLType 4,
                    VectorType vectorConfigEF2M2
                  ]
                  [MaskType 4]
            }
        ]
    ]

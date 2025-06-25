{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.OpSemantics.IotaTest (iotaTest) where

import Grisette (BV (bv), LogicalOp (false), SomeWordN)
import HieraSynth.TypeSignature
  ( TypeSignature (TypeSignature, argTypes, resTypes),
  )
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Policy (tuPolicy)
import RVV.Semantics.Value
  ( Mask (Mask),
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
import RVV.Synthesizer.Operator.VectorIndex (vectorId, vectorIota)
import RVV.Synthesizer.Parameter.Destination (pd, ud)
import RVV.Synthesizer.Parameter.Masking (pm)
import RVV.Synthesizer.Type (ValueType (MaskType, VLType, VectorType))
import RVV.Synthesizer.Value (Value (MaskValue, VLValue, VectorValue))
import Test.Framework (Test, testGroup)

iotaTest :: Test
iotaTest =
  testGroup
    "Iota"
    [ testGroup "Semantics" . fmap semanticsTest $
        [ SemanticsTest
            { semanticsTestName = "VIota",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = vectorIota @'C vectorConfigEF2M2 ud pm,
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 7)) tuPolicy false,
                  MaskValue $ Mask 4 $ VectorReg (bv 16 0x0063) (bv 16 0),
                  MaskValue $ Mask 4 $ VectorReg (bv 16 0x00ef) (bv 16 0)
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x2210 :: SomeWordN) (bv 16 0x0000),
                          VectorReg (bv 16 0x0320) (bv 16 0xf00f)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "VId",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = vectorId @'C vectorConfigEF2M2 pd pm,
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 7)) tuPolicy false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0xcccc) (bv 16 0xffff),
                        VectorReg (bv 16 0xcccc) (bv 16 0xffff)
                      ],
                  MaskValue $ Mask 4 $ VectorReg (bv 16 0x00e9) (bv 16 0)
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x3000 :: SomeWordN) (bv 16 0x0ff0),
                          VectorReg (bv 16 0xc650) (bv 16 0xf00f)
                        ]
                  ]
            }
        ],
      testGroup "Typing" . fmap typingTest $
        [ TypingTest
            { typingTestName = "VIota",
              typingTestOp = vectorIota @'C vectorConfigEF2M2 ud pm,
              typingTestExpected =
                TypeSignature
                  { argTypes =
                      [ VLType 4,
                        MaskType 4,
                        MaskType 4
                      ],
                    resTypes = [VectorType vectorConfigEF2M2]
                  }
            },
          TypingTest
            { typingTestName = "VId",
              typingTestOp = vectorId @'C vectorConfigEF2M2 pd pm,
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

{-# LANGUAGE DataKinds #-}

module RVV.Synthesizer.OpSemantics.ReinterpretTest (reinterpretTest) where

import Grisette (BV (bv))
import HieraSynth.TypeSignature (TypeSignature (TypeSignature))
import RVV.Semantics.Value (Mask (Mask), Vector (Vector), VectorReg (VectorReg))
import RVV.Semantics.VectorConfigConstants
  ( vconst8162,
    vectorConfigE1M2,
    vectorConfigEF2M1,
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
import RVV.Synthesizer.Operator.Reinterpret (maskToVector, vectorToMask, vectorToVector, maskToMask)
import RVV.Synthesizer.Type (ValueType (MaskType, VectorType))
import RVV.Synthesizer.Value (Value (MaskValue, VectorValue))
import Test.Framework (Test, testGroup)

reinterpretTest :: Test
reinterpretTest =
  testGroup
    "Reinterpret"
    [ testGroup "Semantics" . fmap semanticsTest $
        [ SemanticsTest
            { semanticsTestName = "VectorToVector",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = vectorToVector vectorConfigEF2M2 vectorConfigE1M2,
              semanticsTestInput =
                [ VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x1234) (bv 16 0x5678),
                        VectorReg (bv 16 0x542d) (bv 16 0xabd3)
                      ]
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigE1M2
                        [ VectorReg (bv 16 0x1234) (bv 16 0x5678),
                          VectorReg (bv 16 0x542d) (bv 16 0xabd3)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "MaskToVector",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = maskToVector 2 vectorConfigEF2M1,
              semanticsTestInput =
                [MaskValue $ Mask 2 (VectorReg (bv 16 0x1234) (bv 16 0x5678))],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector vectorConfigEF2M1 [VectorReg (bv 16 0x1234) (bv 16 0x5678)]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "VectorToMask",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = vectorToMask vectorConfigEF2M1 2,
              semanticsTestInput =
                [ VectorValue $
                    Vector vectorConfigEF2M1 [VectorReg (bv 16 0x1234) (bv 16 0x5678)]
                ],
              semanticsTestExpected =
                Just
                  [MaskValue $ Mask 2 (VectorReg (bv 16 0x1234) (bv 16 0x5678))]
            },
          SemanticsTest
            { semanticsTestName = "MaskToMask",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = maskToMask 1 2,
              semanticsTestInput =
                [MaskValue $ Mask 1 (VectorReg (bv 16 0x1234) (bv 16 0x5678))],
              semanticsTestExpected =
                Just
                  [MaskValue $ Mask 2 (VectorReg (bv 16 0x1234) (bv 16 0x5678))]
            }
        ],
      testGroup "Typing" . fmap typingTest $
        [ TypingTest
            { typingTestName = "VectorToVector",
              typingTestOp = vectorToVector vectorConfigEF2M2 vectorConfigE1M2,
              typingTestExpected =
                TypeSignature
                  [VectorType vectorConfigEF2M2]
                  [VectorType vectorConfigE1M2]
            },
          TypingTest
            { typingTestName = "MaskToVector",
              typingTestOp = maskToVector 2 vectorConfigEF2M1,
              typingTestExpected =
                TypeSignature [MaskType 2] [VectorType vectorConfigEF2M1]
            },
          TypingTest
            { typingTestName = "VectorToMask",
              typingTestOp = vectorToMask vectorConfigEF2M1 2,
              typingTestExpected =
                TypeSignature [VectorType vectorConfigEF2M1] [MaskType 2]
            },
          TypingTest
            { typingTestName = "MaskToMask",
              typingTestOp = maskToMask 1 2,
              typingTestExpected =
                TypeSignature [MaskType 1] [MaskType 2]
            }
        ]
    ]

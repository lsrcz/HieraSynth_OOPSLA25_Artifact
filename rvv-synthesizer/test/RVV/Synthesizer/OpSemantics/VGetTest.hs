{-# LANGUAGE DataKinds #-}

module RVV.Synthesizer.OpSemantics.VGetTest (vgetTest) where

import Grisette (BV (bv))
import HieraSynth.TypeSignature (TypeSignature (TypeSignature))
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Value (Mask (Mask), Vector (Vector), VectorReg (VectorReg))
import RVV.Semantics.VectorConfigConstants
  ( vconst8162,
    vectorConfigEF2M1,
    vectorConfigEF2M2,
    vectorConfigEF2M4,
    vectorConfigEF2MF2,
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
import RVV.Synthesizer.Operator.Extract (extractMask, extractVector, vectorTruncate)
import RVV.Synthesizer.Type (ValueType (MaskType, VectorType))
import RVV.Synthesizer.Value (Value (MaskValue, VectorValue))
import Test.Framework (Test, testGroup)

vgetTest :: Test
vgetTest =
  testGroup
    "Extract"
    [ testGroup "Semantics" . fmap semanticsTest $
        [ SemanticsTest
            { semanticsTestName = "ExtractVector",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = extractVector vectorConfigEF2MF2 vectorConfigEF2M4 3,
              semanticsTestInput =
                [ VectorValue $
                    Vector
                      vectorConfigEF2M4
                      [ VectorReg (bv 16 0x1234) (bv 16 0x5678),
                        VectorReg (bv 16 0x4567) (bv 16 0x0123),
                        VectorReg (bv 16 0x7890) (bv 16 0x3456),
                        VectorReg (bv 16 0x1562) (bv 16 0x612d)
                      ]
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2MF2
                        [VectorReg (bv 16 0x0045) (bv 16 0xff01)] ::
                      Value 'C
                  ]
            },
          SemanticsTest
            { semanticsTestName = "ExtractVector/0,True",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = extractVector vectorConfigEF2MF2 vectorConfigEF2M4 0,
              semanticsTestInput =
                [ VectorValue $
                    Vector
                      vectorConfigEF2M4
                      [ VectorReg (bv 16 0x1234) (bv 16 0x5678),
                        VectorReg (bv 16 0x4567) (bv 16 0x0123),
                        VectorReg (bv 16 0x7890) (bv 16 0x3456),
                        VectorReg (bv 16 0x1562) (bv 16 0x612d)
                      ]
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2MF2
                        [VectorReg (bv 16 0x0034) (bv 16 0xff78)] ::
                      Value 'C
                  ]
            },
          SemanticsTest
            { semanticsTestName = "ExtractVector/0,False",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = extractVector vectorConfigEF2MF2 vectorConfigEF2M4 0,
              semanticsTestInput =
                [ VectorValue $
                    Vector
                      vectorConfigEF2M4
                      [ VectorReg (bv 16 0x1234) (bv 16 0x5678),
                        VectorReg (bv 16 0x4567) (bv 16 0x0123),
                        VectorReg (bv 16 0x7890) (bv 16 0x3456),
                        VectorReg (bv 16 0x1562) (bv 16 0x612d)
                      ]
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2MF2
                        [VectorReg (bv 16 0x0034) (bv 16 0xff78)] ::
                      Value 'C
                  ]
            },
          SemanticsTest
            { semanticsTestName = "VectorTruncate",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = vectorTruncate vectorConfigEF2MF2 vectorConfigEF2M4,
              semanticsTestInput =
                [ VectorValue $
                    Vector
                      vectorConfigEF2M4
                      [ VectorReg (bv 16 0x1234) (bv 16 0x5678),
                        VectorReg (bv 16 0x4567) (bv 16 0x0123),
                        VectorReg (bv 16 0x7890) (bv 16 0x3456),
                        VectorReg (bv 16 0x1562) (bv 16 0x612d)
                      ]
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2MF2
                        [VectorReg (bv 16 0x0034) (bv 16 0xff78)] ::
                      Value 'C
                  ]
            },
          SemanticsTest
            { semanticsTestName = "ExtractMask/full",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = extractMask 8 8 0,
              semanticsTestInput =
                [MaskValue $ Mask 8 (VectorReg (bv 16 0x1234) (bv 16 0x5678))],
              semanticsTestExpected =
                Just [MaskValue $ Mask 8 (VectorReg (bv 16 0x1234) (bv 16 0x5678))]
            },
          SemanticsTest
            { semanticsTestName = "ExtractMask/same",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = extractMask 4 4 0,
              semanticsTestInput =
                [MaskValue $ Mask 4 (VectorReg (bv 16 0x1234) (bv 16 0x5678))],
              semanticsTestExpected =
                Just [MaskValue $ Mask 4 (VectorReg (bv 16 0x0034) (bv 16 0xff78))]
            },
          SemanticsTest
            { semanticsTestName = "ExtractMask/0",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = extractMask 2 4 0,
              semanticsTestInput =
                [MaskValue $ Mask 4 (VectorReg (bv 16 0x1234) (bv 16 0x5678))],
              semanticsTestExpected =
                Just [MaskValue $ Mask 2 (VectorReg (bv 16 0x0004) (bv 16 0xfff8))]
            },
          SemanticsTest
            { semanticsTestName = "ExtractMask/1",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = extractMask 2 4 1,
              semanticsTestInput =
                [MaskValue $ Mask 4 (VectorReg (bv 16 0x1234) (bv 16 0x5678))],
              semanticsTestExpected =
                Just [MaskValue $ Mask 2 (VectorReg (bv 16 0x0003) (bv 16 0xfff7))]
            }
        ],
      testGroup "Typing" . fmap typingTest $
        [ TypingTest
            { typingTestName = "ExtractVector",
              typingTestOp = extractVector vectorConfigEF2MF2 vectorConfigEF2M2 2,
              typingTestExpected =
                TypeSignature
                  [VectorType vectorConfigEF2M2]
                  [VectorType vectorConfigEF2MF2]
            },
          TypingTest
            { typingTestName = "ExtractVector/0,True",
              typingTestOp = extractVector vectorConfigEF2MF2 vectorConfigEF2M2 0,
              typingTestExpected =
                TypeSignature
                  [VectorType vectorConfigEF2M2]
                  [VectorType vectorConfigEF2MF2]
            },
          TypingTest
            { typingTestName = "ExtractVector/0,False",
              typingTestOp = extractVector vectorConfigEF2MF2 vectorConfigEF2M2 0,
              typingTestExpected =
                TypeSignature
                  [VectorType vectorConfigEF2M2]
                  [VectorType vectorConfigEF2MF2]
            },
          TypingTest
            { typingTestName = "VectorTruncate",
              typingTestOp = vectorTruncate vectorConfigEF2M1 vectorConfigEF2M2,
              typingTestExpected =
                TypeSignature
                  [VectorType vectorConfigEF2M2]
                  [VectorType vectorConfigEF2M1]
            },
          TypingTest
            { typingTestName = "VectorTruncate/False",
              typingTestOp = vectorTruncate vectorConfigEF2M1 vectorConfigEF2M2,
              typingTestExpected =
                TypeSignature
                  [VectorType vectorConfigEF2M2]
                  [VectorType vectorConfigEF2M1]
            },
          TypingTest
            { typingTestName = "ExtractMask",
              typingTestOp = extractMask 2 4 0,
              typingTestExpected =
                TypeSignature
                  [MaskType 4]
                  [MaskType 2]
            }
        ]
    ]

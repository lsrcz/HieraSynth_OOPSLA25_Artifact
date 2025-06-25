{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.OpSemantics.VSetTest (vsetTest) where

import Grisette (BV (bv))
import HieraSynth.TypeSignature (TypeSignature (TypeSignature))
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Value (Mask (Mask), Vector (Vector), VectorReg (VectorReg))
import RVV.Semantics.VectorConfigConstants
  ( vconst8162,
    vectorConfigE1M2,
    vectorConfigEF2M1,
    vectorConfigEF2M2,
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
import RVV.Synthesizer.Operator.Insert (insertMask, insertVector, vectorExtend)
import RVV.Synthesizer.Parameter.Destination (pd, ud)
import RVV.Synthesizer.Type
  ( ValueType (MaskType, VectorType),
  )
import RVV.Synthesizer.Value (Value (MaskValue, VectorValue))
import Test.Framework (Test, testGroup)

vsetTest :: Test
vsetTest =
  testGroup
    "Insert"
    [ testGroup "Semantics" . fmap semanticsTest $
        [ SemanticsTest
            { semanticsTestName = "InsertVector",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = insertVector @'C vectorConfigEF2MF2 vectorConfigEF2M2 pd 2,
              semanticsTestInput =
                [ VectorValue $
                    Vector
                      vectorConfigEF2MF2
                      [VectorReg (bv 16 0xeeff) (bv 16 0xffee)] ::
                    Value 'C,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x4321) (bv 16 0x8765),
                        VectorReg (bv 16 0x7654) (bv 16 0x3210)
                      ]
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x4321) (bv 16 0x8765),
                          VectorReg (bv 16 0x76ff) (bv 16 0x32ee)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "InsertVector/undef dest",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = insertVector @'C vectorConfigEF2MF2 vectorConfigEF2M2 ud 2,
              semanticsTestInput =
                [ VectorValue $
                    Vector vectorConfigEF2MF2 [VectorReg (bv 16 0x7654) (bv 16 0x3210)]
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x0000) (bv 16 0xffff),
                          VectorReg (bv 16 0x0054) (bv 16 0xff10)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "VectorExtend",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = vectorExtend vectorConfigEF2M1 vectorConfigEF2M2,
              semanticsTestInput =
                [ VectorValue $
                    Vector
                      vectorConfigEF2M1
                      [ VectorReg (bv 16 0xabcd) (bv 16 0xdcba)
                      ]
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0xabcd) (bv 16 0xdcba),
                          VectorReg (bv 16 0) (bv 16 0xffff)
                        ] ::
                      Value 'C
                  ]
            },
          SemanticsTest
            { semanticsTestName = "InsertMask/full",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = insertMask @'C 8 8 pd 0,
              semanticsTestInput =
                [ MaskValue $ Mask 8 (VectorReg (bv 16 0x1234) (bv 16 0x5678)),
                  MaskValue $ Mask 8 (VectorReg (bv 16 0x9abc) (bv 16 0xdef0))
                ],
              semanticsTestExpected =
                Just [MaskValue $ Mask 8 (VectorReg (bv 16 0x1234) (bv 16 0x5678))]
            },
          SemanticsTest
            { semanticsTestName = "InsertMask/same",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = insertMask @'C 4 4 pd 0,
              semanticsTestInput =
                [ MaskValue $ Mask 4 (VectorReg (bv 16 0x1234) (bv 16 0x5678)),
                  MaskValue $ Mask 4 (VectorReg (bv 16 0x9abc) (bv 16 0xdef0))
                ],
              semanticsTestExpected =
                Just [MaskValue $ Mask 4 (VectorReg (bv 16 0x9a34) (bv 16 0xde78))]
            },
          SemanticsTest
            { semanticsTestName = "InsertMask/full dest, 0",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = insertMask @'C 4 8 pd 0,
              semanticsTestInput =
                [ MaskValue $ Mask 4 (VectorReg (bv 16 0x1234) (bv 16 0x5678)),
                  MaskValue $ Mask 8 (VectorReg (bv 16 0x9abc) (bv 16 0xdef0))
                ],
              semanticsTestExpected =
                Just [MaskValue $ Mask 8 (VectorReg (bv 16 0x9a34) (bv 16 0xde78))]
            },
          SemanticsTest
            { semanticsTestName = "InsertMask/full dest, inner",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = insertMask @'C 2 8 pd 1,
              semanticsTestInput =
                [ MaskValue $ Mask 2 (VectorReg (bv 16 0x1234) (bv 16 0x5678)),
                  MaskValue $ Mask 8 (VectorReg (bv 16 0x9abc) (bv 16 0xdef0))
                ],
              semanticsTestExpected =
                Just [MaskValue $ Mask 8 (VectorReg (bv 16 0x9a4c) (bv 16 0xde80))]
            },
          SemanticsTest
            { semanticsTestName = "InsertMask/full dest, end",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = insertMask @'C 2 8 pd 3,
              semanticsTestInput =
                [ MaskValue $ Mask 2 (VectorReg (bv 16 0x1234) (bv 16 0x5678)),
                  MaskValue $ Mask 8 (VectorReg (bv 16 0x9abc) (bv 16 0xdef0))
                ],
              semanticsTestExpected =
                Just [MaskValue $ Mask 8 (VectorReg (bv 16 0x4abc) (bv 16 0x8ef0))]
            },
          SemanticsTest
            { semanticsTestName = "InsertMask/partial dest, end",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = insertMask @'C 2 4 pd 1,
              semanticsTestInput =
                [ MaskValue $ Mask 2 (VectorReg (bv 16 0x1234) (bv 16 0x5678)),
                  MaskValue $ Mask 4 (VectorReg (bv 16 0x9abc) (bv 16 0xdef0))
                ],
              semanticsTestExpected =
                Just [MaskValue $ Mask 4 (VectorReg (bv 16 0x9a4c) (bv 16 0xde80))]
            },
          SemanticsTest
            { semanticsTestName = "InsertMask/undef dest",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = insertMask @'C 4 8 ud 1,
              semanticsTestInput =
                [MaskValue $ Mask 4 (VectorReg (bv 16 0x1234) (bv 16 0x5678))],
              semanticsTestExpected =
                Just [MaskValue $ Mask 8 (VectorReg (bv 16 0x3400) (bv 16 0x78ff))]
            }
        ],
      testGroup "Typing" . fmap typingTest $
        [ TypingTest
            { typingTestName = "InsertVector",
              typingTestOp = insertVector @'C vectorConfigEF2MF2 vectorConfigE1M2 pd 2,
              typingTestExpected =
                TypeSignature
                  [VectorType vectorConfigEF2MF2, VectorType vectorConfigE1M2]
                  [VectorType vectorConfigE1M2]
            },
          TypingTest
            { typingTestName = "InsertVector/undef dest",
              typingTestOp = insertVector @'C vectorConfigEF2MF2 vectorConfigE1M2 ud 2,
              typingTestExpected =
                TypeSignature
                  [VectorType vectorConfigEF2MF2]
                  [VectorType vectorConfigE1M2]
            },
          TypingTest
            { typingTestName = "VectorExtend",
              typingTestOp = vectorExtend vectorConfigEF2M1 vectorConfigEF2M2,
              typingTestExpected =
                TypeSignature
                  [VectorType vectorConfigEF2M1]
                  [VectorType vectorConfigEF2M2]
            },
          TypingTest
            { typingTestName = "InsertMask",
              typingTestOp = insertMask @'C 4 8 pd 0,
              typingTestExpected =
                TypeSignature
                  [MaskType 4, MaskType 8]
                  [MaskType 8]
            },
          TypingTest
            { typingTestName = "InsertMask",
              typingTestOp = insertMask @'C 2 8 ud 1,
              typingTestExpected =
                TypeSignature [MaskType 2] [MaskType 8]
            }
        ]
    ]

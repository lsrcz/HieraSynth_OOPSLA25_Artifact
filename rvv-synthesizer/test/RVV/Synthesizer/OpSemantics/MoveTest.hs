{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.OpSemantics.MoveTest (moveTest) where

import Grisette (BV (bv), LogicalOp (false, true))
import HieraSynth.TypeSignature (TypeSignature (TypeSignature))
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Imm (Imm (ConstImm))
import RVV.Semantics.Policy (nonePolicy, tuPolicy)
import RVV.Semantics.Value
  ( Mask (Mask),
    Scalar (Scalar),
    VL (VL),
    VLValue (VLNum),
    Vector (Vector),
    VectorReg (VectorReg),
  )
import RVV.Semantics.VectorConfigConstants
  ( vconst8162,
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
import RVV.Synthesizer.Operator.Common.RHSSpec (RHSSpec (ImmRHS, ScalarRHS, VectorRHS))
import RVV.Synthesizer.Operator.Move
  ( moveFirstElementToScalar,
    moveScalarToFirstElement,
    moveToMask,
    moveToVector,
  )
import RVV.Synthesizer.Parameter.Destination (pd)
import RVV.Synthesizer.Type (ValueType (MaskType, ScalarType, VLType, VectorType))
import RVV.Synthesizer.Value (Value (MaskValue, ScalarValue, VLValue, VectorValue))
import Test.Framework (Test, testGroup)

moveTest :: Test
moveTest =
  testGroup
    "Move"
    [ testGroup "Semantics" . fmap semanticsTest $
        [ SemanticsTest
            { semanticsTestName = "VMVVX",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = moveToVector @'C vectorConfigEF2M2 pd ScalarRHS,
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 5)) tuPolicy false,
                  ScalarValue $ Scalar (bv 4 0x9) false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x1234) (bv 16 0),
                        VectorReg (bv 16 0x4567) (bv 16 0)
                      ]
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x9999) (bv 16 0),
                          VectorReg (bv 16 0x4569) (bv 16 0)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "VMVVV",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = moveToVector @'C vectorConfigEF2M2 pd VectorRHS,
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 5)) tuPolicy false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x89ab) (bv 16 0),
                        VectorReg (bv 16 0xcdef) (bv 16 0)
                      ],
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x1234) (bv 16 0),
                        VectorReg (bv 16 0x4567) (bv 16 0)
                      ]
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x89ab) (bv 16 0),
                          VectorReg (bv 16 0x456f) (bv 16 0)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "VMVSX",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = moveScalarToFirstElement @'C vectorConfigEF2M2 pd,
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 5)) tuPolicy false,
                  ScalarValue $ Scalar (bv 4 0x9) false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x1234) (bv 16 0),
                        VectorReg (bv 16 0x4567) (bv 16 0)
                      ]
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x1239) (bv 16 0),
                          VectorReg (bv 16 0x4567) (bv 16 0)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "VMVSX/Uninitialized scalar",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = moveScalarToFirstElement @'C vectorConfigEF2M2 pd,
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 5)) tuPolicy false,
                  ScalarValue $ Scalar (bv 4 0x9) true,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x1234) (bv 16 0),
                        VectorReg (bv 16 0x4567) (bv 16 0)
                      ]
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x1239) (bv 16 0x000f),
                          VectorReg (bv 16 0x4567) (bv 16 0)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "VMVSX/Uninitialized VL",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = moveScalarToFirstElement @'C vectorConfigEF2M2 pd,
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 5)) tuPolicy true,
                  ScalarValue $ Scalar (bv 4 0x9) false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x1234) (bv 16 0),
                        VectorReg (bv 16 0x4567) (bv 16 0)
                      ]
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x1239) (bv 16 0xffff),
                          VectorReg (bv 16 0x4567) (bv 16 0xffff)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "VMVSX/0",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = moveScalarToFirstElement @'C vectorConfigEF2M2 pd,
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 0)) tuPolicy false,
                  ScalarValue $ Scalar (bv 4 0x9) false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x1234) (bv 16 0),
                        VectorReg (bv 16 0x4567) (bv 16 0)
                      ]
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x1234) (bv 16 0),
                          VectorReg (bv 16 0x4567) (bv 16 0)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "VMVSX/ta",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = moveScalarToFirstElement @'C vectorConfigEF2M2 pd,
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 5)) nonePolicy false,
                  ScalarValue $ Scalar (bv 4 0x9) false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x1234) (bv 16 0),
                        VectorReg (bv 16 0x4567) (bv 16 0)
                      ]
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x0009) (bv 16 0xfff0),
                          VectorReg (bv 16 0x0000) (bv 16 0xffff)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "VMVXS",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = moveFirstElementToScalar vectorConfigEF2M2,
              semanticsTestInput =
                [ VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x1234) (bv 16 0),
                        VectorReg (bv 16 0x4567) (bv 16 0)
                      ]
                ],
              semanticsTestExpected =
                Just [ScalarValue $ Scalar (bv 4 0x4) false]
            },
          SemanticsTest
            { semanticsTestName = "VMVXS/invalid",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = moveFirstElementToScalar vectorConfigEF2M2,
              semanticsTestInput =
                [ VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x1234) (bv 16 1),
                        VectorReg (bv 16 0x4567) (bv 16 0)
                      ]
                ],
              semanticsTestExpected =
                Just [ScalarValue $ Scalar (bv 4 0x4) true]
            },
          SemanticsTest
            { semanticsTestName = "VMVVI",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = moveToVector @'C vectorConfigEF2M2 pd (ImmRHS (ConstImm 0x5)),
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 5)) tuPolicy false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x1234) (bv 16 0),
                        VectorReg (bv 16 0x4567) (bv 16 0)
                      ]
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0xdddd) (bv 16 0),
                          VectorReg (bv 16 0x456d) (bv 16 0)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "VMVMX",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = moveToMask @'C (1 / 2) 2 ScalarRHS,
              semanticsTestInput = [ScalarValue $ Scalar (bv 4 0x9) false],
              semanticsTestExpected =
                Just [MaskValue $ Mask 2 $ VectorReg (bv 16 0x0009) (bv 16 0xfff0)]
            },
          SemanticsTest
            { semanticsTestName = "VMVMI",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = moveToMask @'C (1 / 2) 2 (ImmRHS (ConstImm 0x5)),
              semanticsTestInput = [],
              semanticsTestExpected =
                Just [MaskValue $ Mask 2 $ VectorReg (bv 16 0x000d) (bv 16 0xfff0)]
            }
        ],
      testGroup "Typing" . fmap typingTest $
        [ TypingTest
            { typingTestName = "VMVVV",
              typingTestOp = moveToVector @'C vectorConfigEF2M2 pd VectorRHS,
              typingTestExpected =
                TypeSignature
                  [ VLType 4,
                    VectorType vectorConfigEF2M2,
                    VectorType vectorConfigEF2M2
                  ]
                  [VectorType vectorConfigEF2M2]
            },
          TypingTest
            { typingTestName = "VMVVX",
              typingTestOp = moveToVector @'C vectorConfigEF2M2 pd ScalarRHS,
              typingTestExpected =
                TypeSignature
                  [VLType 4, ScalarType (1 / 2), VectorType vectorConfigEF2M2]
                  [VectorType vectorConfigEF2M2]
            },
          TypingTest
            { typingTestName = "VMVSX",
              typingTestOp = moveScalarToFirstElement @'C vectorConfigEF2M2 pd,
              typingTestExpected =
                TypeSignature
                  [VLType 4, ScalarType (1 / 2), VectorType vectorConfigEF2M2]
                  [VectorType vectorConfigEF2M2]
            },
          TypingTest
            { typingTestName = "VMVXS",
              typingTestOp = moveFirstElementToScalar vectorConfigEF2M2,
              typingTestExpected =
                TypeSignature [VectorType vectorConfigEF2M2] [ScalarType (1 / 2)]
            },
          TypingTest
            { typingTestName = "VMVVI",
              typingTestOp = moveToVector @'C vectorConfigEF2M2 pd (ImmRHS (ConstImm 0x5)),
              typingTestExpected =
                TypeSignature
                  [VLType 4, VectorType vectorConfigEF2M2]
                  [VectorType vectorConfigEF2M2]
            },
          TypingTest
            { typingTestName = "VMVMX",
              typingTestOp = moveToMask @'C (1 / 2) 4 ScalarRHS,
              typingTestExpected =
                TypeSignature [ScalarType (1 / 2)] [MaskType 4]
            },
          TypingTest
            { typingTestName = "VMVMI",
              typingTestOp = moveToMask @'C (1 / 2) 4 (ImmRHS (ConstImm 0x5)),
              typingTestExpected = TypeSignature [] [MaskType 4]
            }
        ]
    ]

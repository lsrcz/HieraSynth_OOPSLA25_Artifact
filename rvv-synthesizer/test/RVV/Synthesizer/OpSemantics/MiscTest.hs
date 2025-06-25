{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.OpSemantics.MiscTest (miscTest) where

import Grisette
  ( BV (bv),
    LogicalOp (false),
    SomeWordN,
  )
import HieraSynth.Combinator.Sum (inj)
import HieraSynth.TypeSignature
  ( TypeSignature (TypeSignature, argTypes, resTypes),
  )
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Policy (muPolicy, tuPolicy)
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
import RVV.Synthesizer.Operator.SetVectorLength
  ( setMaxVectorLength,
    setRelayedVectorLength,
    setVectorLength,
  )
import RVV.Synthesizer.Operator.Undefined
  ( Undefined (UndefinedMask, UndefinedVector),
  )
import RVV.Synthesizer.Type
  ( ValueType
      ( MaskType,
        ScalarType,
        VLType,
        VectorType
      ),
  )
import RVV.Synthesizer.Value
  ( Value (MaskValue, ScalarValue, VLValue, VectorValue),
  )
import Test.Framework (Test, testGroup)

miscTest :: Test
miscTest =
  testGroup
    "Misc"
    [ testGroup "Semantics" . fmap semanticsTest $
        [ SemanticsTest
            { semanticsTestName = "SetVectorLength",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = setVectorLength @'C 4 tuPolicy,
              semanticsTestInput =
                [ ScalarValue $ Scalar (bv 8 13 :: SomeWordN) false
                ],
              semanticsTestExpected =
                Just [VLValue $ VL 4 (VLNum (bv 8 0x07)) tuPolicy false]
            },
          SemanticsTest
            { semanticsTestName = "SetMaxVectorLength",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = setMaxVectorLength @'C 4 tuPolicy,
              semanticsTestInput = [],
              semanticsTestExpected =
                Just [VLValue $ VL 4 (VLNum (bv 8 0x08)) tuPolicy false]
            },
          SemanticsTest
            { semanticsTestName = "SetRelayedVectorLength-same",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = setRelayedVectorLength @'C 4 4 tuPolicy,
              semanticsTestInput =
                [VLValue $ VL 4 (VLNum (bv 8 7 :: SomeWordN)) muPolicy false],
              semanticsTestExpected =
                Just [VLValue $ VL 4 (VLNum (bv 8 7 :: SomeWordN)) tuPolicy false]
            },
          SemanticsTest
            { semanticsTestName = "SetRelayedVectorLength-shrink",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = setRelayedVectorLength @'C 2 4 tuPolicy,
              semanticsTestInput =
                [VLValue $ VL 4 (VLNum (bv 8 7 :: SomeWordN)) muPolicy false],
              semanticsTestExpected =
                Just [VLValue $ VL 2 (VLNum (bv 8 0x04)) tuPolicy false]
            },
          SemanticsTest
            { semanticsTestName = "UndefinedMask",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = inj $ UndefinedMask 4,
              semanticsTestInput = [],
              semanticsTestExpected =
                Just [MaskValue $ Mask 4 $ VectorReg (bv 16 0) (bv 16 (-1))]
            },
          SemanticsTest
            { semanticsTestName = "UndefinedVector",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = inj $ UndefinedVector vectorConfigEF2M2,
              semanticsTestInput = [],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0) (bv 16 (-1)),
                          VectorReg (bv 16 0) (bv 16 (-1))
                        ]
                  ]
            }
        ],
      testGroup "Typing" . fmap typingTest $
        [ TypingTest
            { typingTestName = "SetVectorLength",
              typingTestOp = setVectorLength @'C 4 tuPolicy,
              typingTestExpected =
                TypeSignature
                  { argTypes = [ScalarType 1],
                    resTypes = [VLType 4]
                  }
            },
          TypingTest
            { typingTestName = "SetMaxVectorLength",
              typingTestOp = setMaxVectorLength @'C 4 tuPolicy,
              typingTestExpected =
                TypeSignature
                  { argTypes = [],
                    resTypes = [VLType 4]
                  }
            },
          TypingTest
            { typingTestName = "SetRelayedVectorLength",
              typingTestOp = setRelayedVectorLength @'C 4 2 tuPolicy,
              typingTestExpected =
                TypeSignature
                  { argTypes = [VLType 2],
                    resTypes = [VLType 4]
                  }
            },
          TypingTest
            { typingTestName = "UndefinedMask",
              typingTestOp = inj $ UndefinedMask 4,
              typingTestExpected =
                TypeSignature
                  { argTypes = [],
                    resTypes = [MaskType 4]
                  }
            },
          TypingTest
            { typingTestName = "UndefinedVector",
              typingTestOp = inj $ UndefinedVector vectorConfigEF2M2,
              typingTestExpected =
                TypeSignature
                  { argTypes = [],
                    resTypes = [VectorType vectorConfigEF2M2]
                  }
            }
        ]
    ]

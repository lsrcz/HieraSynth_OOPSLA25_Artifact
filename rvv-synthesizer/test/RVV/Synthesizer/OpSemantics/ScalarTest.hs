{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.OpSemantics.ScalarTest (scalarTest) where

import Grisette (BV (bv), LogicalOp (false))
import HieraSynth.TypeSignature
  ( TypeSignature (TypeSignature, argTypes, resTypes),
  )
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Imm (Imm (ConstImm))
import RVV.Semantics.Policy (tuPolicy)
import RVV.Semantics.Value (Scalar (Scalar), VL (VL), VLValue (VLNum))
import RVV.Semantics.VectorConfigConstants (vconst8162)
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
import RVV.Synthesizer.Operator.Common.RHSSpec (RHSSpec (ImmRHS, ScalarRHS))
import RVV.Synthesizer.Operator.Scalar
  ( scalarLongImm,
  )
import RVV.Synthesizer.Operator.ScalarOperator (scalarBin, scalarBinVectorLength)
import RVV.Synthesizer.Parameter.SingleWidthIntBinaryOpCode (mkAdd, mkSll, mkSub)
import RVV.Synthesizer.Type (ValueType (ScalarType, VLType))
import RVV.Synthesizer.Value (Value (ScalarValue, VLValue))
import Test.Framework (Test, testGroup)

scalarTest :: Test
scalarTest =
  testGroup
    "Scalar"
    [ testGroup "Semantics" . fmap semanticsTest $
        [ SemanticsTest
            { semanticsTestName = "ScalarBin",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = scalarBin @'C mkAdd 1 ScalarRHS,
              semanticsTestInput =
                [ ScalarValue $ Scalar (bv 8 0x3a) false :: Value 'C,
                  ScalarValue $ Scalar (bv 8 0x71) false
                ],
              semanticsTestExpected =
                Just [ScalarValue $ Scalar (bv 8 0xab) false]
            },
          SemanticsTest
            { semanticsTestName = "ScalarBinImmPositive",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = scalarBin @'C mkSub 1 (ImmRHS $ ConstImm 0b1010),
              semanticsTestInput = [ScalarValue $ Scalar (bv 8 0x71) false],
              semanticsTestExpected =
                Just [ScalarValue $ Scalar (bv 8 0x67) false]
            },
          SemanticsTest
            { semanticsTestName = "ScalarBinImmNegative",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = scalarBin @'C mkSub 1 (ImmRHS $ ConstImm 0b11010),
              semanticsTestInput = [ScalarValue $ Scalar (bv 8 0x71) false],
              semanticsTestExpected =
                Just [ScalarValue $ Scalar (bv 8 0x77) false]
            },
          SemanticsTest
            { semanticsTestName = "ScalarBinVectorLength",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = scalarBinVectorLength @'C mkSll 4 ScalarRHS,
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 0x2)) tuPolicy false,
                  ScalarValue $ Scalar (bv 8 0x2) false :: Value 'C
                ],
              semanticsTestExpected =
                Just [ScalarValue $ Scalar (bv 8 0x8) false]
            },
          SemanticsTest
            { semanticsTestName = "ScalarLong",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = scalarLongImm @'C 1 (ConstImm 0xd4),
              semanticsTestInput = [],
              semanticsTestExpected =
                Just [ScalarValue $ Scalar (bv 8 0xd4) false]
            }
        ],
      testGroup "Typing" . fmap typingTest $
        [ TypingTest
            { typingTestName = "ScalarBin",
              typingTestOp = scalarBin @'C mkAdd 2 ScalarRHS,
              typingTestExpected =
                TypeSignature
                  { argTypes = [ScalarType 2, ScalarType 2],
                    resTypes = [ScalarType 2]
                  }
            },
          TypingTest
            { typingTestName = "ScalarBinImm",
              typingTestOp = scalarBin @'C mkAdd 2 (ImmRHS $ ConstImm 0),
              typingTestExpected =
                TypeSignature
                  { argTypes = [ScalarType 2],
                    resTypes = [ScalarType 2]
                  }
            },
          TypingTest
            { typingTestName = "ScalarBinVectorLength",
              typingTestOp = scalarBinVectorLength @'C mkAdd 4 ScalarRHS,
              typingTestExpected =
                TypeSignature
                  { argTypes = [VLType 4, ScalarType 1],
                    resTypes = [ScalarType 1]
                  }
            },
          TypingTest
            { typingTestName = "ScalarLong",
              typingTestOp = scalarLongImm @'C 2 (ConstImm 0xd4),
              typingTestExpected = TypeSignature [] [ScalarType 2]
            }
        ]
    ]

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module RVV.Synthesizer.OpSemantics.VlenbTest (vlenbTest) where

import Grisette
  ( BV (bv),
    LogicalOp (false),
    SomeWordN,
  )
import HieraSynth.TypeSignature
  ( TypeSignature (TypeSignature, argTypes, resTypes),
  )
import RVV.Semantics.Value (Scalar (Scalar))
import RVV.Semantics.VectorConfigConstants (vconst64512, vconst8162)
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
import RVV.Synthesizer.Operator.Vlenb (vlenb)
import RVV.Synthesizer.Type (ValueType (ScalarType))
import RVV.Synthesizer.Value (Value (ScalarValue))
import Test.Framework (Test, testGroup)

vlenbTest :: Test
vlenbTest =
  testGroup
    "Vlenb"
    [ testGroup "Semantics" . fmap semanticsTest $
        [ SemanticsTest
            { semanticsTestName = "vconst8162",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = vlenb,
              semanticsTestInput = [],
              semanticsTestExpected =
                Just [ScalarValue $ Scalar (bv 8 8 :: SomeWordN) false]
            },
          SemanticsTest
            { semanticsTestName = "vconst64512",
              semanticsTestMachineConfig = vconst64512,
              semanticsTestOp = vlenb,
              semanticsTestInput = [],
              semanticsTestExpected =
                Just [ScalarValue $ Scalar (bv 64 64 :: SomeWordN) false]
            }
        ],
      testGroup "Typing" . fmap typingTest $
        [ TypingTest
            { typingTestName = "Vlenb",
              typingTestOp = vlenb,
              typingTestExpected =
                TypeSignature
                  { argTypes = [],
                    resTypes = [ScalarType 1]
                  }
            }
        ]
    ]

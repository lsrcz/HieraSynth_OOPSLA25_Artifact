{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.OpSemantics.MaskLogicalTest (maskLogicalTest) where

import Grisette (BV (bv), LogicalOp (false))
import HieraSynth.TypeSignature (TypeSignature (TypeSignature))
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Policy (tumuPolicy)
import RVV.Semantics.Value
  ( Mask (Mask),
    VL (VL),
    VLValue (VLNum),
    VectorReg (VectorReg),
  )
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
import RVV.Synthesizer.Operator.MaskLogical (maskLogical)
import RVV.Synthesizer.Parameter.MaskLogicalOpCode (mkMAnd)
import RVV.Synthesizer.Type (ValueType (MaskType, VLType))
import RVV.Synthesizer.Value (Value (MaskValue, VLValue))
import Test.Framework (Test, testGroup)

maskLogicalTest :: Test
maskLogicalTest =
  testGroup
    "MaskLogical"
    [ testGroup "Semantics" . fmap semanticsTest $
        [ SemanticsTest
            { semanticsTestName = "MaskLogical",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = maskLogical @'C 4 mkMAnd,
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 6)) tumuPolicy false,
                  MaskValue $ Mask 4 $ VectorReg (bv 16 0x00aa) (bv 16 0),
                  MaskValue $ Mask 4 $ VectorReg (bv 16 0x002f) (bv 16 0)
                ],
              semanticsTestExpected =
                Just
                  [ MaskValue (Mask 4 $ VectorReg (bv 16 0x002a) (bv 16 0xffc0))
                  ]
            }
        ],
      testGroup "Typing" . fmap typingTest $
        [ TypingTest
            { typingTestName = "MaskLogical",
              typingTestOp = maskLogical @'C 4 mkMAnd,
              typingTestExpected =
                TypeSignature
                  [VLType 4, MaskType 4, MaskType 4]
                  [MaskType 4]
            }
        ]
    ]

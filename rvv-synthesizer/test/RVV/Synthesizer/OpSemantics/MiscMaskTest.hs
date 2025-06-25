{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.OpSemantics.MiscMaskTest (miscMaskTest) where

import Control.Monad.Identity (Identity (Identity))
import Grisette (BV (bv))
import HieraSynth.TypeSignature (TypeSignature (TypeSignature))
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Policy (muPolicy, nonePolicy)
import RVV.Semantics.Value
  ( Mask (Mask),
    Scalar (Scalar),
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
import RVV.Synthesizer.Operator.MiscMask
  ( maskFirstOne,
    maskPopCount,
    maskSetBit,
  )
import RVV.Synthesizer.Parameter.Destination (pd)
import RVV.Synthesizer.Parameter.Masking (fm, pm)
import RVV.Synthesizer.Parameter.SetMaskMethod
  ( SetMaskMethod (BeforeFirst, IncludingFirst, OnlyFirst),
  )
import RVV.Synthesizer.Type (ValueType (MaskType, ScalarType, VLType))
import RVV.Synthesizer.Value (Value (MaskValue, ScalarValue, VLValue))
import Test.Framework (Test, testGroup)

miscMaskTest :: Test
miscMaskTest =
  testGroup
    "MiscMask"
    [ testGroup "Semantics" . fmap semanticsTest $
        [ SemanticsTest
            { semanticsTestName = "VCPop",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = maskPopCount @'C 8 pm,
              semanticsTestInput =
                [ VLValue $ VL 8 (VLNum (bv 8 11)) nonePolicy False,
                  MaskValue $ Mask 8 (VectorReg (bv 16 0x245f) (bv 16 0)),
                  MaskValue $ Mask 8 (VectorReg (bv 16 0x57d9) (bv 16 0))
                ],
              semanticsTestExpected =
                Just [ScalarValue $ Scalar (bv 8 5) False]
            },
          SemanticsTest
            { semanticsTestName = "VFirst",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = maskFirstOne @'C 8 pm,
              semanticsTestInput =
                [ VLValue $ VL 8 (VLNum (bv 8 11)) nonePolicy False,
                  MaskValue $ Mask 8 (VectorReg (bv 16 0x2458) (bv 16 0)),
                  MaskValue $ Mask 8 (VectorReg (bv 16 0x57d9) (bv 16 0))
                ],
              semanticsTestExpected =
                Just [ScalarValue $ Scalar (bv 8 3) False]
            }
        ]
          ++ ( do
                 (name, op, expected) <-
                   [ ( "VSetMask[SetBeforeFirst]",
                       BeforeFirst,
                       Mask 8 (VectorReg (bv 16 0x037b) (bv 16 0xf800))
                     ),
                     ( "VSetMask[SetIncludingFirst]",
                       IncludingFirst,
                       Mask 8 (VectorReg (bv 16 0x03fb) (bv 16 0xf800))
                     ),
                     ( "VSetMask[SetOnlyFirst]",
                       OnlyFirst,
                       Mask 8 (VectorReg (bv 16 0x0389) (bv 16 0xf800))
                     )
                     ]
                 return $
                   SemanticsTest
                     { semanticsTestName = name,
                       semanticsTestMachineConfig = vconst8162,
                       semanticsTestOp = maskSetBit (Identity op) 8 pd pm,
                       semanticsTestInput =
                         [ VLValue $ VL 8 (VLNum (bv 8 11)) muPolicy False,
                           MaskValue $ Mask 8 (VectorReg (bv 16 0x5789) (bv 16 0)),
                           MaskValue $ Mask 8 (VectorReg (bv 16 0x5789) (bv 16 0)),
                           MaskValue $ Mask 8 (VectorReg (bv 16 0x1cf2) (bv 16 0))
                         ],
                       semanticsTestExpected = Just [MaskValue expected]
                     }
             ),
      testGroup "Typing" . fmap typingTest $
        [ TypingTest
            { typingTestName = "VCPop",
              typingTestOp = maskPopCount @'C 8 pm,
              typingTestExpected =
                TypeSignature
                  [VLType 8, MaskType 8, MaskType 8]
                  [ScalarType 1]
            },
          TypingTest
            { typingTestName = "VFirst",
              typingTestOp = maskFirstOne @'C 8 fm,
              typingTestExpected =
                TypeSignature
                  [VLType 8, MaskType 8]
                  [ScalarType 1]
            },
          TypingTest
            { typingTestName = "VSetMask",
              typingTestOp = maskSetBit (Identity BeforeFirst) 8 pd pm,
              typingTestExpected =
                TypeSignature
                  [ VLType 8,
                    MaskType 8,
                    MaskType 8,
                    MaskType 8
                  ]
                  [MaskType 8]
            }
        ]
    ]

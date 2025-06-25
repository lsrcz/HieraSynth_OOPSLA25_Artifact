{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.OpSemantics.MaskElementWiseTest
  ( maskElementWiseTest,
  )
where

import Grisette (BV (bv), LogicalOp (false))
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Imm (Imm (ConstImm))
import RVV.Semantics.Value (Mask (Mask), Scalar (Scalar), VectorReg (VectorReg))
import RVV.Semantics.VectorConfigConstants
  ( vconst8162,
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
    semanticsTest,
  )
import RVV.Synthesizer.Operator.Common.RHSSpec
  ( RHSSpec (ImmRHS, ScalarRHS, VectorRHS),
  )
import RVV.Synthesizer.Operator.DelegatedVectorBinaryOnMask
  ( delegatedVectorBinaryOnMask,
  )
import RVV.Synthesizer.Parameter.SingleWidthIntBinaryOpCode (mkAnd)
import RVV.Synthesizer.Value (Value (MaskValue, ScalarValue))
import Test.Framework (Test, testGroup)

maskElementWiseTest :: Test
maskElementWiseTest =
  testGroup
    "DelegatedVectorBinaryOnMask"
    [ testGroup "Semantics" . fmap semanticsTest $
        [ SemanticsTest
            { semanticsTestName = "VectorRHS",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = delegatedVectorBinaryOnMask @'C (1 / 2) 4 mkAnd VectorRHS,
              semanticsTestInput =
                [ MaskValue $ Mask 4 $ VectorReg (bv 16 0x5555) (bv 16 0x0000),
                  MaskValue $ Mask 4 $ VectorReg (bv 16 0x3333) (bv 16 0x0000)
                ],
              semanticsTestExpected =
                Just
                  [MaskValue $ Mask 4 $ VectorReg (bv 16 0x0011) (bv 16 0xff00)]
            },
          SemanticsTest
            { semanticsTestName = "ScalarRHS",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = delegatedVectorBinaryOnMask @'C (1 / 2) 4 mkAnd ScalarRHS,
              semanticsTestInput =
                [ MaskValue $ Mask 4 $ VectorReg (bv 16 0x5555) (bv 16 0x0000),
                  ScalarValue $ Scalar (bv 4 0x3) false
                ],
              semanticsTestExpected =
                Just
                  [MaskValue $ Mask 4 $ VectorReg (bv 16 0x0011) (bv 16 0xff00)]
            },
          SemanticsTest
            { semanticsTestName = "ImmRHS",
              semanticsTestMachineConfig = vconst8162,
              semanticsTestOp = delegatedVectorBinaryOnMask @'C (1 / 2) 4 mkAnd (ImmRHS (ConstImm 0x3)),
              semanticsTestInput =
                [MaskValue $ Mask 4 $ VectorReg (bv 16 0x5555) (bv 16 0x0000)],
              semanticsTestExpected =
                Just
                  [MaskValue $ Mask 4 $ VectorReg (bv 16 0x0011) (bv 16 0xff00)]
            }
        ]
    ]

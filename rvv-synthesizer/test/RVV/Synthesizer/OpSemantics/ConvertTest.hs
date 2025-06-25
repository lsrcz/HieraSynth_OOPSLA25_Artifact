{-# LANGUAGE DataKinds #-}

module RVV.Synthesizer.OpSemantics.ConvertTest (convertTest) where

import qualified Data.HashMap.Lazy as M
import Grisette (BV (bv), LogicalOp (false))
import HieraSynth.TypeSignature (TypeSignature (TypeSignature))
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Memory (Ptr (Ptr))
import RVV.Semantics.Policy (tuPolicy)
import RVV.Semantics.Value
  ( Scalar (Scalar),
    VL (VL),
    VLValue (VLNum),
  )
import RVV.Semantics.VectorConfigConstants (vconst8162, vconstWithMem)
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
import RVV.Synthesizer.Operator.Reinterpret
  ( ptrToScalar,
    scalarToPtr,
    vlToScalar,
  )
import RVV.Synthesizer.Type (ValueType (PtrType, ScalarType, VLType))
import RVV.Synthesizer.Value (Value (PtrValue, ScalarValue, VLValue))
import Test.Framework (Test, testGroup)

vconst :: MachineConfig
vconst = vconstWithMem vconst8162 $ M.fromList [(0, 20)]

convertTest :: Test
convertTest =
  testGroup
    "Convert"
    [ testGroup "Semantics" . fmap semanticsTest $
        [ SemanticsTest
            { semanticsTestName = "VLToScalar",
              semanticsTestMachineConfig = vconst,
              semanticsTestOp = vlToScalar 4,
              semanticsTestInput = [VLValue $ VL 4 (VLNum (bv 8 0x01)) tuPolicy false],
              semanticsTestExpected = Just [ScalarValue $ Scalar (bv 8 0x01) false]
            },
          SemanticsTest
            { semanticsTestName = "VLToScalar/bad ratio",
              semanticsTestMachineConfig = vconst,
              semanticsTestOp = vlToScalar 2,
              semanticsTestInput = [VLValue $ VL 4 (VLNum (bv 8 0x01)) tuPolicy false],
              semanticsTestExpected = Nothing
            },
          SemanticsTest
            { semanticsTestName = "PtrToScalar",
              semanticsTestMachineConfig = vconst,
              semanticsTestOp = ptrToScalar 4 0,
              semanticsTestInput = [PtrValue $ Ptr 4 0 (bv 8 0x01) false],
              semanticsTestExpected = Just [ScalarValue $ Scalar (bv 8 0x01) false]
            },
          SemanticsTest
            { semanticsTestName = "PtrToScalar/bad block id",
              semanticsTestMachineConfig = vconst,
              semanticsTestOp = ptrToScalar 4 1,
              semanticsTestInput = [PtrValue $ Ptr 4 0 (bv 8 0x01) false],
              semanticsTestExpected = Nothing
            },
          SemanticsTest
            { semanticsTestName = "PtrToScalar/bad width",
              semanticsTestMachineConfig = vconst,
              semanticsTestOp = ptrToScalar 8 0,
              semanticsTestInput = [PtrValue $ Ptr 4 0 (bv 8 0x01) false],
              semanticsTestExpected = Nothing
            },
          SemanticsTest
            { semanticsTestName = "ScalarToPtr",
              semanticsTestMachineConfig = vconst,
              semanticsTestOp = scalarToPtr 4 0,
              semanticsTestInput = [ScalarValue $ Scalar (bv 8 0x01) false],
              semanticsTestExpected = Just [PtrValue $ Ptr 4 0 (bv 8 0x01) false]
            },
          SemanticsTest
            { semanticsTestName = "ScalarToPtr/bad block id",
              semanticsTestMachineConfig = vconst,
              semanticsTestOp = scalarToPtr 4 1,
              semanticsTestInput = [ScalarValue $ Scalar (bv 8 0x01) false],
              semanticsTestExpected = Nothing
            }
        ],
      testGroup "Typing" . fmap typingTest $
        [ TypingTest
            { typingTestName = "VLToScalar",
              typingTestOp = vlToScalar 4,
              typingTestExpected = TypeSignature [VLType 4] [ScalarType 1]
            },
          TypingTest
            { typingTestName = "PtrToScalar",
              typingTestOp = ptrToScalar 4 0,
              typingTestExpected = TypeSignature [PtrType 4 0] [ScalarType 1]
            },
          TypingTest
            { typingTestName = "ScalarToPtr",
              typingTestOp = scalarToPtr 4 0,
              typingTestExpected = TypeSignature [ScalarType 1] [PtrType 4 0]
            }
        ]
    ]

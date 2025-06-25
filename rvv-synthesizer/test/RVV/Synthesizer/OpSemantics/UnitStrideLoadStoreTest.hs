{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.OpSemantics.UnitStrideLoadStoreTest
  ( -- unitStrideLoadStoreTest,
  )
where
  {-

import qualified Data.HashMap.Lazy as M
import Grisette (BV (bv), LogicalOp (false))
import HieraSynth.TypeSignature
  ( TypeSignature (TypeSignature, argTypes, resTypes),
  )
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Memory
  ( Memory (Memory),
    MemoryBlock (MemoryBlock),
    Ptr (Ptr),
  )
import RVV.Semantics.Policy (nonePolicy)
import RVV.Semantics.Value
  ( VL (VL),
    VLValue (VLNum),
    Vector (Vector),
    VectorReg (VectorReg),
  )
import RVV.Semantics.VectorConfigConstants (vconst8162, vconstWithMem, vectorConfigEF2M2)
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
import RVV.Synthesizer.Operator.Load (load)
import RVV.Synthesizer.Operator.Store (store)
import RVV.Synthesizer.Parameter.Destination (pd, ud)
import RVV.Synthesizer.Parameter.Masking (fm)
import RVV.Synthesizer.Type
  ( ValueType (MemType, PtrType, VLType, VectorType),
  )
import RVV.Synthesizer.Value (Value (MemValue, PtrValue, VLValue, VectorValue))
import Test.Framework (Test, testGroup)

memoryBlock :: MemoryBlock 'C
memoryBlock = MemoryBlock (bv 40 0x123456789a) (bv 40 0x1035013020)

memory :: Memory 'C
memory = Memory (M.fromList [(0, memoryBlock)])

storeMemoryBlock :: MemoryBlock 'C
storeMemoryBlock = MemoryBlock (bv 40 0x5555555555) (bv 40 0xcccccccccc)

storeMemory :: Memory 'C
storeMemory = Memory (M.fromList [(0, storeMemoryBlock)])

unitStrideLoadStoreTest :: Test
unitStrideLoadStoreTest =
  testGroup
    "UnitStrideLoadStore"
    [ testGroup "Semantics" . fmap semanticsTest $
        [ SemanticsTest
            { semanticsTestName = "Load",
              semanticsTestMachineConfig =
                vconstWithMem vconst8162 $ M.fromList [(0, 40)],
              semanticsTestOp = load @'C vectorConfigEF2M2 0 ud fm,
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 6)) nonePolicy false,
                  MemValue memory,
                  PtrValue $ Ptr (1 / 2) 0 (bv 8 2) false
                ],
              semanticsTestExpected =
                Just
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x6789) (bv 16 0x1302),
                          VectorReg (bv 16 0x0045) (bv 16 0xff50)
                        ]
                  ]
            },
          SemanticsTest
            { semanticsTestName = "Store",
              semanticsTestMachineConfig =
                vconstWithMem vconst8162 $ M.fromList [(0, 40)],
              semanticsTestOp = store @'C vectorConfigEF2M2 0 fm,
              semanticsTestInput =
                [ VLValue $ VL 4 (VLNum (bv 8 6)) nonePolicy false,
                  MemValue storeMemory,
                  PtrValue $ Ptr (1 / 2) 0 (bv 8 2) false,
                  VectorValue $
                    Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0x789a) (bv 16 0x3020),
                        VectorReg (bv 16 0x3456) (bv 16 0x3501)
                      ]
                ],
              semanticsTestExpected =
                Just
                  [ MemValue $
                      Memory $
                        M.fromList
                          [ ( 0,
                              MemoryBlock
                                (bv 40 0x55556789a5)
                                (bv 40 0xccc013020c)
                            )
                          ]
                  ]
            }
        ],
      testGroup "Typing" . fmap typingTest $
        [ TypingTest
            { typingTestName = "Load",
              typingTestOp = load @'C vectorConfigEF2M2 0 pd fm,
              typingTestExpected =
                TypeSignature
                  { argTypes =
                      [ VLType 4,
                        MemType,
                        PtrType (1 / 2) 0,
                        VectorType vectorConfigEF2M2
                      ],
                    resTypes = [VectorType vectorConfigEF2M2]
                  }
            },
          TypingTest
            { typingTestName = "Store",
              typingTestOp = store @'C vectorConfigEF2M2 0 fm,
              typingTestExpected =
                TypeSignature
                  { argTypes =
                      [ VLType 4,
                        MemType,
                        PtrType (1 / 2) 0,
                        VectorType vectorConfigEF2M2
                      ],
                    resTypes = [MemType]
                  }
            }
        ]
    ]
-}
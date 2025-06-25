{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module RVV.Semantics.PrimOp.UnitStrideLoadStoreTest
  ( unitStrideLoadStoreTest,
  )
where

import qualified Data.HashMap.Lazy as M
import Grisette (BV (bv), LogicalOp (false), SomeBV, WordN)
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Memory
  ( BlockId,
    Memory (Memory),
    MemoryBlock (MemoryBlock),
    Ptr (Ptr),
  )
import RVV.Semantics.Policy (muPolicy, nonePolicy)
import RVV.Semantics.PrimOp.FullMask (fullMask)
import RVV.Semantics.PrimOp.UnitStrideLoadStore
  ( unitStrideLoad,
    unitStrideStore,
    unitStrideValidAccess,
  )
import RVV.Semantics.Value
  ( Mask (Mask),
    VL (VL),
    Vector (Vector),
    VectorReg (VectorReg),
  )
import RVV.Semantics.VectorConfig (VectorConfig)
import RVV.Semantics.VectorConfigConstants (vconst8162, vconstWithMem, vectorConfigEF2M2)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

{-
data UnitStrideValidAccessTest = UnitStrideValidAccessTest
  { validAccessTestName :: String,
    validAccessTestMachineConfig :: MachineConfig,
    validAccessTestVectorConfig :: VectorConfig,
    validAccessTestMask :: Mask 'C,
    validAccessTestLenBlock :: Int,
    validAccessTestBlockId :: BlockId,
    validAccessTestPtr :: Ptr 'C,
    validAccessTestVLNum :: SomeBV WordN,
    validAccessTestExpected :: Bool
  }

vconst :: MachineConfig
vconst = vconstWithMem vconst8162 (M.fromList [(0, 40)])

memoryBlock :: MemoryBlock 'C
memoryBlock = MemoryBlock (bv 40 0x123456789a) (bv 40 0x1035013020)

memory :: Memory 'C
memory = Memory (M.fromList [(0, memoryBlock)])

data UnitStrideLoadTest = UnitStrideLoadTest
  { loadTestName :: String,
    loadTestMask :: Mask 'C,
    loadTestBlockId :: BlockId,
    loadTestPtr :: Ptr 'C,
    loadTestVL :: VL 'C,
    loadTestExpected :: Maybe (Vector 'C)
  }

data UnitStrideStoreTest = UnitStrideStoreTest
  { storeTestName :: String,
    storeTestMask :: Mask 'C,
    storeTestBlockId :: BlockId,
    storeTestPtr :: Ptr 'C,
    storeTestVector :: Vector 'C,
    storeTestVL :: VL 'C,
    storeTestExpected :: Maybe (Memory 'C)
  }

storeMemoryBlock :: MemoryBlock 'C
storeMemoryBlock = MemoryBlock (bv 40 0x5555555555) (bv 40 0xcccccccccc)

storeMemory :: Memory 'C
storeMemory = Memory (M.fromList [(0, storeMemoryBlock)])
-}

unitStrideLoadStoreTest :: Test
unitStrideLoadStoreTest =
  testGroup
    "UnitStrideLoadStore"
    [ {-testGroup "unitStrideValidAccess" $ do
        UnitStrideValidAccessTest {..} <-
          [ UnitStrideValidAccessTest
              { validAccessTestName = "memory block exact long, start with 0",
                validAccessTestMachineConfig = vconst,
                validAccessTestVectorConfig = vectorConfigEF2M2,
                validAccessTestMask = fullMask vconst 4,
                validAccessTestLenBlock = 32,
                validAccessTestBlockId = 0,
                validAccessTestPtr = Ptr (1 / 2) 0 (bv 8 0) false,
                validAccessTestVLNum = bv 8 8,
                validAccessTestExpected = True
              },
            UnitStrideValidAccessTest
              { validAccessTestName = "Unmatched block ID",
                validAccessTestMachineConfig = vconst,
                validAccessTestVectorConfig = vectorConfigEF2M2,
                validAccessTestMask = fullMask vconst 4,
                validAccessTestLenBlock = 32,
                validAccessTestBlockId = 1,
                validAccessTestPtr = Ptr (1 / 2) 0 (bv 8 0) false,
                validAccessTestVLNum = bv 8 8,
                validAccessTestExpected = False
              },
            UnitStrideValidAccessTest
              { validAccessTestName = "incorrect ptr width",
                validAccessTestMachineConfig = vconst,
                validAccessTestVectorConfig = vectorConfigEF2M2,
                validAccessTestMask = fullMask vconst 4,
                validAccessTestLenBlock = 32,
                validAccessTestBlockId = 0,
                validAccessTestPtr = Ptr 8 0 (bv 8 0) false,
                validAccessTestVLNum = bv 8 8,
                validAccessTestExpected = False
              },
            UnitStrideValidAccessTest
              { validAccessTestName = "incorrect block ID",
                validAccessTestMachineConfig = vconst,
                validAccessTestVectorConfig = vectorConfigEF2M2,
                validAccessTestMask = fullMask vconst 4,
                validAccessTestLenBlock = 32,
                validAccessTestBlockId = 0,
                validAccessTestPtr = Ptr (1 / 2) 1 (bv 8 0) false,
                validAccessTestVLNum = bv 8 8,
                validAccessTestExpected = False
              },
            UnitStrideValidAccessTest
              { validAccessTestName = "memory block not long enough",
                validAccessTestMachineConfig = vconst,
                validAccessTestVectorConfig = vectorConfigEF2M2,
                validAccessTestMask = fullMask vconst 4,
                validAccessTestLenBlock = 30,
                validAccessTestBlockId = 0,
                validAccessTestPtr = Ptr (1 / 2) 0 (bv 8 0) false,
                validAccessTestVLNum = bv 8 8,
                validAccessTestExpected = False
              },
            UnitStrideValidAccessTest
              { validAccessTestName = "memory block longer than needed",
                validAccessTestMachineConfig = vconst,
                validAccessTestVectorConfig = vectorConfigEF2M2,
                validAccessTestMask = fullMask vconst 4,
                validAccessTestLenBlock = 36,
                validAccessTestBlockId = 0,
                validAccessTestPtr = Ptr (1 / 2) 0 (bv 8 2) false,
                validAccessTestVLNum = bv 8 8,
                validAccessTestExpected = True
              },
            UnitStrideValidAccessTest
              { validAccessTestName = "memory block not aligned",
                validAccessTestMachineConfig = vconst,
                validAccessTestVectorConfig = vectorConfigEF2M2,
                validAccessTestMask = fullMask vconst 4,
                validAccessTestLenBlock = 36,
                validAccessTestBlockId = 0,
                validAccessTestPtr = Ptr (1 / 2) 0 (bv 8 1) false,
                validAccessTestVLNum = bv 8 8,
                validAccessTestExpected = False
              },
            UnitStrideValidAccessTest
              { validAccessTestName =
                  "memory block not long enough but excluded by vl",
                validAccessTestMachineConfig = vconst,
                validAccessTestVectorConfig = vectorConfigEF2M2,
                validAccessTestMask = fullMask vconst 4,
                validAccessTestLenBlock = 30,
                validAccessTestBlockId = 0,
                validAccessTestPtr = Ptr (1 / 2) 0 (bv 8 0) false,
                validAccessTestVLNum = bv 8 7,
                validAccessTestExpected = True
              },
            UnitStrideValidAccessTest
              { validAccessTestName =
                  "memory block not long enough but excluded by mask",
                validAccessTestMachineConfig = vconst,
                validAccessTestVectorConfig = vectorConfigEF2M2,
                validAccessTestMask = Mask 4 $ VectorReg (bv 16 0x007f) (bv 16 0),
                validAccessTestLenBlock = 30,
                validAccessTestBlockId = 0,
                validAccessTestPtr = Ptr (1 / 2) 0 (bv 8 0) false,
                validAccessTestVLNum = bv 8 8,
                validAccessTestExpected = True
              },
            UnitStrideValidAccessTest
              { validAccessTestName =
                  "memory block not long enough, mask uninitialized",
                validAccessTestMachineConfig = vconst,
                validAccessTestVectorConfig = vectorConfigEF2M2,
                validAccessTestMask = Mask 4 $ VectorReg (bv 16 0x007f) (bv 16 0x80),
                validAccessTestLenBlock = 30,
                validAccessTestBlockId = 0,
                validAccessTestPtr = Ptr (1 / 2) 0 (bv 8 0) false,
                validAccessTestVLNum = bv 8 8,
                validAccessTestExpected = False
              }
            ]
        return $ testCase validAccessTestName $ do
          let actual =
                unitStrideValidAccess
                  validAccessTestMachineConfig
                  validAccessTestVectorConfig
                  validAccessTestMask
                  validAccessTestLenBlock
                  validAccessTestBlockId
                  validAccessTestPtr
                  validAccessTestVLNum
          case actual of
            Left _ -> False @?= validAccessTestExpected
            Right _ -> True @?= validAccessTestExpected,
      testGroup "unitStrideLoad" $ do
        UnitStrideLoadTest {..} <-
          [ UnitStrideLoadTest
              { loadTestName = "memory block exact long, start with 0",
                loadTestMask = fullMask vconst 4,
                loadTestBlockId = 0,
                loadTestPtr = Ptr (1 / 2) 0 (bv 8 0) false,
                loadTestVL = VL 4 (bv 8 8) nonePolicy false,
                loadTestExpected =
                  Just
                    ( Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x789a) (bv 16 0x3020),
                          VectorReg (bv 16 0x3456) (bv 16 0x3501)
                        ]
                    )
              },
            UnitStrideLoadTest
              { loadTestName = "memory block exact long, start with 2",
                loadTestMask = fullMask vconst 4,
                loadTestBlockId = 0,
                loadTestPtr = Ptr (1 / 2) 0 (bv 8 2) false,
                loadTestVL = VL 4 (bv 8 8) nonePolicy false,
                loadTestExpected =
                  Just
                    ( Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x6789) (bv 16 0x1302),
                          VectorReg (bv 16 0x2345) (bv 16 0x0350)
                        ]
                    )
              },
            UnitStrideLoadTest
              { loadTestName = "memory block exact long, start with 16, vl 2",
                loadTestMask = fullMask vconst 4,
                loadTestBlockId = 0,
                loadTestPtr = Ptr (1 / 2) 0 (bv 8 16) false,
                loadTestVL = VL 4 (bv 8 2) nonePolicy false,
                loadTestExpected =
                  Just
                    ( Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x0012) (bv 16 0xff10),
                          VectorReg (bv 16 0x0000) (bv 16 0xffff)
                        ]
                    )
              },
            UnitStrideLoadTest
              { loadTestName = "memory block exact long, start with 20, vl 0",
                loadTestMask = fullMask vconst 4,
                loadTestBlockId = 0,
                loadTestPtr = Ptr (1 / 2) 0 (bv 8 20) false,
                loadTestVL = VL 4 (bv 8 0) nonePolicy false,
                loadTestExpected =
                  Just
                    ( Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x0000) (bv 16 0xffff),
                          VectorReg (bv 16 0x0000) (bv 16 0xffff)
                        ]
                    )
              },
            UnitStrideLoadTest
              { loadTestName = "memory block exact long, start with 2, masked",
                loadTestMask = Mask 4 $ VectorReg (bv 16 0x0016) (bv 16 0x3),
                loadTestBlockId = 0,
                loadTestPtr = Ptr (1 / 2) 0 (bv 8 2) false,
                loadTestVL = VL 4 (bv 8 6) muPolicy false,
                loadTestExpected =
                  Just
                    ( Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0xc78c) (bv 16 0x03ff),
                          VectorReg (bv 16 0x00c5) (bv 16 0xff00)
                        ]
                    )
              },
            UnitStrideLoadTest
              { loadTestName = "memory block exact long, start with 16, masked",
                loadTestMask = Mask 4 $ VectorReg (bv 16 0x0001) (bv 16 0x02),
                loadTestBlockId = 0,
                loadTestPtr = Ptr (1 / 2) 0 (bv 8 16) false,
                loadTestVL = VL 4 (bv 8 8) muPolicy false,
                loadTestExpected =
                  Just
                    ( Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0xccc2) (bv 16 0x00f0),
                          VectorReg (bv 16 0xcccc) (bv 16 0x0000)
                        ]
                    )
              }
            ]
        return $ testCase loadTestName $ do
          let actual =
                unitStrideLoad
                  (vconstWithMem vconst (M.fromList [(0, 40)]))
                  vectorConfigEF2M2
                  ( Vector
                      vectorConfigEF2M2
                      [ VectorReg (bv 16 0xcccc) (bv 16 0),
                        VectorReg (bv 16 0xcccc) (bv 16 0)
                      ]
                  )
                  loadTestMask
                  memory
                  loadTestBlockId
                  loadTestPtr
                  loadTestVL
          case actual of
            Left _ -> Nothing @?= loadTestExpected
            Right v -> Just v @?= loadTestExpected,
      testGroup "UnitStrideStoreTest" $ do
        UnitStrideStoreTest {..} <-
          [ UnitStrideStoreTest
              { storeTestName = "memory block exact long, start with 0",
                storeTestMask = fullMask vconst 4,
                storeTestBlockId = 0,
                storeTestPtr = Ptr (1 / 2) 0 (bv 8 0) false,
                storeTestVector =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x789a) (bv 16 0x3020),
                      VectorReg (bv 16 0x3456) (bv 16 0x3501)
                    ],
                storeTestVL = VL 4 (bv 8 8) nonePolicy false,
                storeTestExpected =
                  Just $
                    Memory
                      ( M.fromList
                          [ ( 0,
                              MemoryBlock
                                (bv 40 0x553456789a)
                                (bv 40 0xcc35013020)
                            )
                          ]
                      )
              },
            UnitStrideStoreTest
              { storeTestName = "memory block exact long, start with 2",
                storeTestMask = fullMask vconst 4,
                storeTestBlockId = 0,
                storeTestPtr = Ptr (1 / 2) 0 (bv 8 2) false,
                storeTestVector =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x789a) (bv 16 0x3020),
                      VectorReg (bv 16 0x3456) (bv 16 0x3501)
                    ],
                storeTestVL = VL 4 (bv 8 8) nonePolicy false,
                storeTestExpected =
                  Just $
                    Memory
                      ( M.fromList
                          [ ( 0,
                              MemoryBlock
                                (bv 40 0x53456789a5)
                                (bv 40 0xc35013020c)
                            )
                          ]
                      )
              },
            UnitStrideStoreTest
              { storeTestName = "memory block exact long, start with 16, vl 2",
                storeTestMask = fullMask vconst 4,
                storeTestBlockId = 0,
                storeTestPtr = Ptr (1 / 2) 0 (bv 8 16) false,
                storeTestVector =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x789a) (bv 16 0x3020),
                      VectorReg (bv 16 0x3456) (bv 16 0x3501)
                    ],
                storeTestVL = VL 4 (bv 8 2) nonePolicy false,
                storeTestExpected =
                  Just $
                    Memory
                      ( M.fromList
                          [ ( 0,
                              MemoryBlock
                                (bv 40 0x9a55555555)
                                (bv 40 0x20cccccccc)
                            )
                          ]
                      )
              },
            UnitStrideStoreTest
              { storeTestName = "memory block exact long, start with 20, vl 0",
                storeTestMask = fullMask vconst 4,
                storeTestBlockId = 0,
                storeTestPtr = Ptr (1 / 2) 0 (bv 8 20) false,
                storeTestVector =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x789a) (bv 16 0x3020),
                      VectorReg (bv 16 0x3456) (bv 16 0x3501)
                    ],
                storeTestVL = VL 4 (bv 8 0) nonePolicy false,
                storeTestExpected = Just storeMemory
              },
            UnitStrideStoreTest
              { storeTestName = "memory block exact long, start with 2, masked",
                storeTestMask = Mask 4 $ VectorReg (bv 16 0x0016) (bv 16 0x3),
                storeTestBlockId = 0,
                storeTestPtr = Ptr (1 / 2) 0 (bv 8 2) false,
                storeTestVector =
                  Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x789a) (bv 16 0x3020),
                      VectorReg (bv 16 0x3456) (bv 16 0x3501)
                    ],
                storeTestVL = VL 4 (bv 8 8) nonePolicy false,
                storeTestExpected =
                  Just $
                    Memory
                      ( M.fromList
                          [ ( 0,
                              MemoryBlock
                                (bv 40 0x5555658005)
                                (bv 40 0xcccc1c0ffc)
                            )
                          ]
                      )
              }
            ]
        return $ testCase storeTestName $ do
          let actual =
                unitStrideStore
                  vconst
                  vectorConfigEF2M2
                  storeTestVector
                  storeTestMask
                  storeMemory
                  storeTestBlockId
                  storeTestPtr
                  storeTestVL
          case actual of
            Left _ -> Nothing @?= storeTestExpected
            Right v -> Just v @?= storeTestExpected-}
    ]

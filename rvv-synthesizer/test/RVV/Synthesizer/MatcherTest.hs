{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module RVV.Synthesizer.MatcherTest (matcherTest) where

import qualified Data.HashMap.Lazy as M
import Grisette (BV (bv), LogicalOp (false), SomeWordN)
import HieraSynth.Reasoning.Matcher (Matcher (match))
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Memory (Memory (Memory), MemoryBlock (MemoryBlock))
import RVV.Semantics.Value
  ( Mask (Mask),
    Scalar (Scalar),
    Vector (Vector),
    VectorReg (VectorReg),
  )
import RVV.Semantics.VectorConfigConstants (vconst8162, vectorConfigEF2M2, vectorConfigEF4M2)
import RVV.Synthesizer.Matcher
  ( RegListMatcher (RegListMatcher),
    RegMatcher (MatchFull, MatchIgnored, MatchMasked, MatchMem),
    VMatchPair (VMatchPair),
    maskToVMatchPair,
    vectorToVMatchPair,
  )
import RVV.Synthesizer.Value
  ( Value (MaskValue, MemValue, ScalarValue, VectorValue),
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

data ConcreteMatchTest = ConcreteMatchTest
  { concreteTestName :: String,
    concreteMatcher :: RegListMatcher,
    concreteLhs :: [Value 'C],
    concreteRhs :: [Value 'C],
    concreteExpected :: Bool
  }

matcherTest :: Test
matcherTest =
  testGroup
    "Matcher"
    [ testCase "maskToVMatchPair" $ do
        let actual =
              maskToVMatchPair
                ( Mask 2 $
                    VectorReg (bv 16 0x1234 :: SomeWordN) (bv 16 0x5678)
                )
        let expected = VMatchPair (bv 16 0x1234) (bv 16 0x5678)
        actual @?= expected,
      testCase "vectorToVMatchPair" $ do
        let actual =
              vectorToVMatchPair
                ( Vector
                    vectorConfigEF2M2
                    [ VectorReg (bv 16 0x1234 :: SomeWordN) (bv 16 0x5678),
                      VectorReg (bv 16 0x4156 :: SomeWordN) (bv 16 0xab31),
                      VectorReg (bv 16 0xab25 :: SomeWordN) (bv 16 0xbcd1),
                      VectorReg (bv 16 0xda14 :: SomeWordN) (bv 16 0xd311)
                    ]
                )
        let expected =
              VMatchPair (bv 64 0xda14ab2541561234) (bv 64 0xd311bcd1ab315678)
        actual @?= expected,
      testGroup "Concrete" $ do
        ConcreteMatchTest {..} <-
          [ ConcreteMatchTest
              { concreteTestName = "Empty list",
                concreteMatcher = RegListMatcher vconst8162 [],
                concreteLhs = [],
                concreteRhs = [],
                concreteExpected = True
              },
            ConcreteMatchTest
              { concreteTestName = "Unmatch matcher number",
                concreteMatcher = RegListMatcher vconst8162 [MatchFull],
                concreteLhs = [],
                concreteRhs = [],
                concreteExpected = False
              },
            ConcreteMatchTest
              { concreteTestName = "Unmatch lhs rhs number",
                concreteMatcher = RegListMatcher vconst8162 [MatchFull],
                concreteLhs = [ScalarValue $ Scalar (bv 8 0) false],
                concreteRhs = [],
                concreteExpected = False
              },
            ConcreteMatchTest
              { concreteTestName = "Match full same",
                concreteMatcher = RegListMatcher vconst8162 [MatchFull],
                concreteLhs = [ScalarValue $ Scalar (bv 8 10) false],
                concreteRhs = [ScalarValue $ Scalar (bv 8 10) false],
                concreteExpected = True
              },
            ConcreteMatchTest
              { concreteTestName = "Match full not same",
                concreteMatcher = RegListMatcher vconst8162 [MatchFull],
                concreteLhs = [ScalarValue $ Scalar (bv 8 11) false],
                concreteRhs = [ScalarValue $ Scalar (bv 8 10) false],
                concreteExpected = False
              },
            ConcreteMatchTest
              { concreteTestName = "Match ignored same",
                concreteMatcher = RegListMatcher vconst8162 [MatchIgnored],
                concreteLhs = [ScalarValue $ Scalar (bv 8 10) false],
                concreteRhs = [ScalarValue $ Scalar (bv 8 10) false],
                concreteExpected = True
              },
            ConcreteMatchTest
              { concreteTestName = "Match ignored not same",
                concreteMatcher = RegListMatcher vconst8162 [MatchIgnored],
                concreteLhs = [ScalarValue $ Scalar (bv 8 11) false],
                concreteRhs = [ScalarValue $ Scalar (bv 8 10) false],
                concreteExpected = True
              },
            ConcreteMatchTest
              { concreteTestName = "Match ignored different reg type",
                concreteMatcher = RegListMatcher vconst8162 [MatchIgnored],
                concreteLhs = [ScalarValue $ Scalar (bv 8 11) false],
                concreteRhs =
                  [MaskValue $ Mask 2 $ VectorReg (bv 16 11) (bv 16 11)],
                concreteExpected = True
              },
            ConcreteMatchTest
              { concreteTestName = "Match masked vreg group",
                concreteMatcher =
                  RegListMatcher vconst8162 [MatchMasked (bv 32 0xf0f0ff00)],
                concreteLhs =
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x1234) (bv 16 0x5678),
                          VectorReg (bv 16 0x9abc) (bv 16 0xdef0)
                        ]
                  ],
                concreteRhs =
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x1245) (bv 16 0x5689),
                          VectorReg (bv 16 0x99bb) (bv 16 0xddff)
                        ]
                  ],
                concreteExpected = True
              },
            ConcreteMatchTest
              { concreteTestName = "Match masked vreg group same in unmasked",
                concreteMatcher =
                  RegListMatcher vconst8162 [MatchMasked (bv 32 0xf0f0ff00)],
                concreteLhs =
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x1234) (bv 16 0x5678),
                          VectorReg (bv 16 0x9abc) (bv 16 0xdef0)
                        ]
                  ],
                concreteRhs =
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x1234) (bv 16 0x5678),
                          VectorReg (bv 16 0x9abc) (bv 16 0xdef0)
                        ]
                  ],
                concreteExpected = True
              },
            ConcreteMatchTest
              { concreteTestName = "Match masked vreg group different in vtype",
                concreteMatcher =
                  RegListMatcher vconst8162 [MatchMasked (bv 32 0xffffffff)],
                concreteLhs =
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x1234) (bv 16 0x5678),
                          VectorReg (bv 16 0x9abc) (bv 16 0xdef0)
                        ]
                  ],
                concreteRhs =
                  [ VectorValue $
                      Vector
                        vectorConfigEF4M2
                        [ VectorReg (bv 16 0x1234) (bv 16 0x5678),
                          VectorReg (bv 16 0x9abc) (bv 16 0xdef0)
                        ]
                  ],
                concreteExpected = False
              },
            ConcreteMatchTest
              { concreteTestName =
                  "Match masked vreg group different in masked data",
                concreteMatcher =
                  RegListMatcher vconst8162 [MatchMasked (bv 32 0xf0f0ff00)],
                concreteLhs =
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x1334) (bv 16 0x5678),
                          VectorReg (bv 16 0x9abc) (bv 16 0xdef0)
                        ]
                  ],
                concreteRhs =
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x1234) (bv 16 0x5678),
                          VectorReg (bv 16 0x9abc) (bv 16 0xdef0)
                        ]
                  ],
                concreteExpected = False
              },
            ConcreteMatchTest
              { concreteTestName =
                  "Match vreg group different in masked uninitialized bits",
                concreteMatcher =
                  RegListMatcher vconst8162 [MatchMasked (bv 32 0xf0f0ff00)],
                concreteLhs =
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x1234) (bv 16 0x5778),
                          VectorReg (bv 16 0x9abc) (bv 16 0xdef0)
                        ]
                  ],
                concreteRhs =
                  [ VectorValue $
                      Vector
                        vectorConfigEF2M2
                        [ VectorReg (bv 16 0x1234) (bv 16 0x5678),
                          VectorReg (bv 16 0x9abc) (bv 16 0xdef0)
                        ]
                  ],
                concreteExpected = False
              },
            ConcreteMatchTest
              { concreteTestName = "Match masked vreg",
                concreteMatcher =
                  RegListMatcher vconst8162 [MatchMasked (bv 16 0xff00)],
                concreteLhs =
                  [ MaskValue $
                      Mask 2 $
                        VectorReg (bv 16 0x1234) (bv 16 0x5678)
                  ],
                concreteRhs =
                  [ MaskValue $
                      Mask 2 $
                        VectorReg (bv 16 0x1245) (bv 16 0x5689)
                  ],
                concreteExpected = True
              },
            ConcreteMatchTest
              { concreteTestName = "Match masked different ratio",
                concreteMatcher =
                  RegListMatcher vconst8162 [MatchMasked (bv 16 0xff00)],
                concreteLhs =
                  [ MaskValue $
                      Mask 2 $
                        VectorReg (bv 16 0x1234) (bv 16 0x5678)
                  ],
                concreteRhs =
                  [ MaskValue $
                      Mask 4 $
                        VectorReg (bv 16 0x1245) (bv 16 0x5689)
                  ],
                concreteExpected = False
              },
            ConcreteMatchTest
              { concreteTestName = "Match masked vreg same in unmasked",
                concreteMatcher =
                  RegListMatcher vconst8162 [MatchMasked (bv 16 0xff00)],
                concreteLhs =
                  [ MaskValue $
                      Mask 2 $
                        VectorReg (bv 16 0x1234) (bv 16 0x5678)
                  ],
                concreteRhs =
                  [ MaskValue $
                      Mask 2 $
                        VectorReg (bv 16 0x1234) (bv 16 0x5678)
                  ],
                concreteExpected = True
              },
            ConcreteMatchTest
              { concreteTestName =
                  "Match masked vreg different in masked data",
                concreteMatcher =
                  RegListMatcher vconst8162 [MatchMasked (bv 16 0xff00)],
                concreteLhs =
                  [ MaskValue $
                      Mask 2 $
                        VectorReg (bv 16 0x1334) (bv 16 0x5678)
                  ],
                concreteRhs =
                  [ MaskValue $
                      Mask 2 $
                        VectorReg (bv 16 0x1234) (bv 16 0x5678)
                  ],
                concreteExpected = True
              },
            ConcreteMatchTest
              { concreteTestName =
                  "Match masked vreg different in masked uninitialized bits",
                concreteMatcher =
                  RegListMatcher vconst8162 [MatchMasked (bv 16 0xff00)],
                concreteLhs =
                  [ MaskValue $
                      Mask 2 $
                        VectorReg (bv 16 0x1234) (bv 16 0x7678)
                  ],
                concreteRhs =
                  [ MaskValue $
                      Mask 2 $
                        VectorReg (bv 16 0x1234) (bv 16 0x5678)
                  ],
                concreteExpected = True
              },
            ConcreteMatchTest
              { concreteTestName =
                  "Match memory different in block ids",
                concreteMatcher =
                  RegListMatcher vconst8162 [MatchMem (M.fromList [])],
                concreteLhs =
                  [ MemValue $
                      Memory
                        ( M.fromList
                            [(1, MemoryBlock (bv 16 0x1234) (bv 16 0x5678))]
                        )
                  ],
                concreteRhs =
                  [ MemValue $
                      Memory
                        ( M.fromList
                            [(1, MemoryBlock (bv 16 0x1234) (bv 16 0x5678))]
                        )
                  ],
                concreteExpected = False
              },
            ConcreteMatchTest
              { concreteTestName =
                  "Match memory different in block ids 2",
                concreteMatcher =
                  RegListMatcher
                    vconst8162
                    [MatchMem (M.fromList [(1, MatchFull)])],
                concreteLhs = [MemValue $ Memory (M.fromList [])],
                concreteRhs =
                  [ MemValue $
                      Memory
                        ( M.fromList
                            [(1, MemoryBlock (bv 16 0x1234) (bv 16 0x5678))]
                        )
                  ],
                concreteExpected = False
              },
            ConcreteMatchTest
              { concreteTestName =
                  "Match memory full",
                concreteMatcher =
                  RegListMatcher
                    vconst8162
                    [MatchMem (M.fromList [(1, MatchFull)])],
                concreteLhs =
                  [ MemValue $
                      Memory
                        ( M.fromList
                            [(1, MemoryBlock (bv 16 0x1234) (bv 16 0x5678))]
                        )
                  ],
                concreteRhs =
                  [ MemValue $
                      Memory
                        ( M.fromList
                            [(1, MemoryBlock (bv 16 0x1234) (bv 16 0x5678))]
                        )
                  ],
                concreteExpected = True
              },
            ConcreteMatchTest
              { concreteTestName =
                  "Match memory full different",
                concreteMatcher =
                  RegListMatcher
                    vconst8162
                    [MatchMem (M.fromList [(1, MatchFull)])],
                concreteLhs =
                  [ MemValue $
                      Memory
                        ( M.fromList
                            [(1, MemoryBlock (bv 16 0x123) (bv 16 0x56))]
                        )
                  ],
                concreteRhs =
                  [ MemValue $
                      Memory
                        ( M.fromList
                            [(1, MemoryBlock (bv 16 0x1234) (bv 16 0x56))]
                        )
                  ],
                concreteExpected = False
              },
            ConcreteMatchTest
              { concreteTestName =
                  "Match memory ignored",
                concreteMatcher =
                  RegListMatcher
                    vconst8162
                    [MatchMem (M.fromList [(1, MatchIgnored)])],
                concreteLhs =
                  [ MemValue $
                      Memory
                        ( M.fromList
                            [(1, MemoryBlock (bv 16 0x123) (bv 16 0x56))]
                        )
                  ],
                concreteRhs =
                  [ MemValue $
                      Memory
                        ( M.fromList
                            [(1, MemoryBlock (bv 16 0x1234) (bv 16 0x56))]
                        )
                  ],
                concreteExpected = True
              },
            ConcreteMatchTest
              { concreteTestName =
                  "Match memory masked different in unmasked",
                concreteMatcher =
                  RegListMatcher
                    vconst8162
                    [MatchMem (M.fromList [(1, MatchMasked (bv 16 0xff00))])],
                concreteLhs =
                  [ MemValue $
                      Memory
                        ( M.fromList
                            [(1, MemoryBlock (bv 16 0x1222) (bv 16 0x5233))]
                        )
                  ],
                concreteRhs =
                  [ MemValue $
                      Memory
                        ( M.fromList
                            [(1, MemoryBlock (bv 16 0x1234) (bv 16 0x5265))]
                        )
                  ],
                concreteExpected = True
              },
            ConcreteMatchTest
              { concreteTestName =
                  "Match memory masked different in masked data",
                concreteMatcher =
                  RegListMatcher
                    vconst8162
                    [MatchMem (M.fromList [(1, MatchMasked (bv 16 0xff00))])],
                concreteLhs =
                  [ MemValue $
                      Memory
                        ( M.fromList
                            [(1, MemoryBlock (bv 16 0x1134) (bv 16 0x5678))]
                        )
                  ],
                concreteRhs =
                  [ MemValue $
                      Memory
                        ( M.fromList
                            [(1, MemoryBlock (bv 16 0x1234) (bv 16 0x5678))]
                        )
                  ],
                concreteExpected = False
              },
            ConcreteMatchTest
              { concreteTestName =
                  "Match memory masked different in masked uninitialized bits",
                concreteMatcher =
                  RegListMatcher
                    vconst8162
                    [MatchMem (M.fromList [(1, MatchMasked (bv 16 0xff00))])],
                concreteLhs =
                  [ MemValue $
                      Memory
                        ( M.fromList
                            [(1, MemoryBlock (bv 16 0x1234) (bv 16 0x4678))]
                        )
                  ],
                concreteRhs =
                  [ MemValue $
                      Memory
                        ( M.fromList
                            [(1, MemoryBlock (bv 16 0x1234) (bv 16 0x5678))]
                        )
                  ],
                concreteExpected = False
              },
            ConcreteMatchTest
              { concreteTestName =
                  "Match memory empty",
                concreteMatcher =
                  RegListMatcher vconst8162 [MatchMem (M.fromList [])],
                concreteLhs = [MemValue $ Memory (M.fromList [])],
                concreteRhs = [MemValue $ Memory (M.fromList [])],
                concreteExpected = True
              },
            ConcreteMatchTest
              { concreteTestName =
                  "Match memory multiple passed",
                concreteMatcher =
                  RegListMatcher
                    vconst8162
                    [ MatchMem
                        ( M.fromList
                            [ (0, MatchMasked (bv 16 0x00ff)),
                              (1, MatchMasked (bv 16 0xff00))
                            ]
                        )
                    ],
                concreteLhs =
                  [ MemValue $
                      Memory
                        ( M.fromList
                            [ (0, MemoryBlock (bv 16 0x1234) (bv 16 0x5678)),
                              (1, MemoryBlock (bv 16 0x1234) (bv 16 0x5678))
                            ]
                        )
                  ],
                concreteRhs =
                  [ MemValue $
                      Memory
                        ( M.fromList
                            [ (0, MemoryBlock (bv 16 0x1134) (bv 16 0x4578)),
                              (1, MemoryBlock (bv 16 0x1236) (bv 16 0x5689))
                            ]
                        )
                  ],
                concreteExpected = True
              },
            ConcreteMatchTest
              { concreteTestName =
                  "Match memory multiple failed",
                concreteMatcher =
                  RegListMatcher
                    vconst8162
                    [ MatchMem
                        ( M.fromList
                            [ (0, MatchMasked (bv 16 0x00ff)),
                              (1, MatchMasked (bv 16 0xff00))
                            ]
                        )
                    ],
                concreteLhs =
                  [ MemValue $
                      Memory
                        ( M.fromList
                            [ (0, MemoryBlock (bv 16 0x1234) (bv 16 0x5678)),
                              (1, MemoryBlock (bv 16 0x1234) (bv 16 0x5678))
                            ]
                        )
                  ],
                concreteRhs =
                  [ MemValue $
                      Memory
                        ( M.fromList
                            [ (0, MemoryBlock (bv 16 0x1135) (bv 16 0x4578)),
                              (1, MemoryBlock (bv 16 0x1236) (bv 16 0x5689))
                            ]
                        )
                  ],
                concreteExpected = False
              }
            ]
        return $
          testCase concreteTestName $
            match concreteMatcher concreteLhs concreteRhs @?= concreteExpected
    ]

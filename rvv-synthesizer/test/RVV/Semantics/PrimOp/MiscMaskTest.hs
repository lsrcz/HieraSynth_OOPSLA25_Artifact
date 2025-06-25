{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Semantics.PrimOp.MiscMaskTest (miscMaskTest) where

import Grisette (BV (bv))
import Grisette.Unified (EvalModeTag (C))
import RVV.Semantics.Policy (muPolicy, nonePolicy)
import RVV.Semantics.PrimOp.MiscMask
  ( countPopulation,
    findFirstSet,
    setBeforeFirst,
    setIncludingFirst,
    setOnlyFirst,
  )
import RVV.Semantics.Value (Mask (Mask), Scalar (Scalar), VL (VL), VLValue (VLNum), VectorReg (VectorReg))
import RVV.Semantics.VectorConfigConstants (vconst8162)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

miscMaskTest :: Test
miscMaskTest =
  testGroup
    "MiscMask"
    [ testGroup
        "countPopulation"
        [ testCase "full mask full vl" $ do
            let mask = Mask @'C 8 (VectorReg (bv 16 $ -1) (bv 16 0))
            let maskToCount = Mask @'C 8 (VectorReg (bv 16 0x245f) (bv 16 0))
            let vl = VL 8 (VLNum (bv 8 16)) nonePolicy False
            let actual = countPopulation vconst8162 8 mask maskToCount vl
            let expected = Scalar (bv 8 8) False
            actual @?= Right expected,
          testCase "full mask partial vl" $ do
            let mask = Mask @'C 8 (VectorReg (bv 16 $ -1) (bv 16 0))
            let maskToCount = Mask @'C 8 (VectorReg (bv 16 0x245f) (bv 16 0))
            let vl = VL 8 (VLNum (bv 8 11)) nonePolicy False
            let actual = countPopulation vconst8162 8 mask maskToCount vl
            let expected = Scalar (bv 8 7) False
            actual @?= Right expected
            let vl2 = VL 8 (VLNum (bv 8 10)) nonePolicy False
            let actual2 = countPopulation vconst8162 8 mask maskToCount vl2
            let expected2 = Scalar (bv 8 6) False
            actual2 @?= Right expected2,
          testCase "partial mask partial vl" $ do
            let mask = Mask @'C 8 (VectorReg (bv 16 0x57d9) (bv 16 0))
            let maskToCount = Mask @'C 8 (VectorReg (bv 16 0x245f) (bv 16 0))
            let vl = VL 8 (VLNum (bv 8 11)) nonePolicy False
            let actual = countPopulation vconst8162 8 mask maskToCount vl
            let expected = Scalar (bv 8 5) False
            actual @?= Right expected,
          testCase "invalid tail" $ do
            let mask = Mask @'C 8 (VectorReg (bv 16 $ -1) (bv 16 0xf800))
            let maskToCount = Mask @'C 8 (VectorReg (bv 16 0x245f) (bv 16 0xf800))
            let vl = VL 8 (VLNum (bv 8 11)) nonePolicy False
            let actual = countPopulation vconst8162 8 mask maskToCount vl
            let expected = Scalar (bv 8 7) False
            actual @?= Right expected,
          testCase "invalid mask" $ do
            let mask = Mask @'C 8 (VectorReg (bv 16 $ -1) (bv 16 0x0400))
            let maskToCount = Mask @'C 8 (VectorReg (bv 16 0x245f) (bv 16 0))
            let vl = VL 8 (VLNum (bv 8 11)) nonePolicy False
            let actual = countPopulation vconst8162 8 mask maskToCount vl
            let expected = Scalar (bv 8 7) True
            actual @?= Right expected,
          testCase "invalid mask to count" $ do
            let mask = Mask @'C 8 (VectorReg (bv 16 $ -1) (bv 16 0))
            let maskToCount = Mask @'C 8 (VectorReg (bv 16 0x245f) (bv 16 0x0400))
            let vl = VL 8 (VLNum (bv 8 11)) nonePolicy False
            let actual = countPopulation vconst8162 8 mask maskToCount vl
            let expected = Scalar (bv 8 7) True
            actual @?= Right expected,
          testCase "invalid mask to count, but not masked" $ do
            let mask = Mask @'C 8 (VectorReg (bv 16 0xfbff) (bv 16 0))
            let maskToCount = Mask @'C 8 (VectorReg (bv 16 0x245f) (bv 16 0x0400))
            let vl = VL 8 (VLNum (bv 8 11)) nonePolicy False
            let actual = countPopulation vconst8162 8 mask maskToCount vl
            let expected = Scalar (bv 8 6) False
            actual @?= Right expected,
          testCase "invalid mask, but not masked" $ do
            let mask = Mask @'C 8 (VectorReg (bv 16 0x245f) (bv 16 0x0400))
            let maskToCount = Mask @'C 8 (VectorReg (bv 16 0xfbff) (bv 16 0))
            let vl = VL 8 (VLNum (bv 8 11)) nonePolicy False
            let actual = countPopulation vconst8162 8 mask maskToCount vl
            let expected = Scalar (bv 8 6) False
            actual @?= Right expected,
          testCase "both invalid bit" $ do
            let mask = Mask @'C 8 (VectorReg (bv 16 0x245f) (bv 16 0x0400))
            let maskToCount = Mask @'C 8 (VectorReg (bv 16 0xfbff) (bv 16 0x0400))
            let vl = VL 8 (VLNum (bv 8 11)) nonePolicy False
            let actual = countPopulation vconst8162 8 mask maskToCount vl
            let expected = Scalar (bv 8 6) True
            actual @?= Right expected
        ],
      testGroup
        "findFirstSet"
        [ testCase "full mask full vl" $ do
            let mask = Mask @'C 8 (VectorReg (bv 16 $ -1) (bv 16 0))
            let maskToCount = Mask @'C 8 (VectorReg (bv 16 0x2458) (bv 16 0))
            let vl = VL 8 (VLNum (bv 8 16)) nonePolicy False
            let actual = findFirstSet vconst8162 8 mask maskToCount vl
            let expected = Scalar (bv 8 3) False
            actual @?= Right expected,
          testCase "full mask zero vl" $ do
            let mask = Mask @'C 8 (VectorReg (bv 16 $ -1) (bv 16 0))
            let maskToCount = Mask @'C 8 (VectorReg (bv 16 0x2458) (bv 16 0))
            let vl = VL 8 (VLNum (bv 8 0)) nonePolicy False
            let actual = findFirstSet vconst8162 8 mask maskToCount vl
            let expected = Scalar (bv 8 $ -1) False
            actual @?= Right expected,
          testCase "full mask partial vl" $ do
            let mask = Mask @'C 8 (VectorReg (bv 16 $ -1) (bv 16 0))
            let maskToCount = Mask @'C 8 (VectorReg (bv 16 0x2458) (bv 16 0))
            let vl = VL 8 (VLNum (bv 8 11)) nonePolicy False
            let actual = findFirstSet vconst8162 8 mask maskToCount vl
            let expected = Scalar (bv 8 3) False
            actual @?= Right expected,
          testCase "partial mask partial vl" $ do
            let mask = Mask @'C 8 (VectorReg (bv 16 0x57d9) (bv 16 0))
            let maskToCount = Mask @'C 8 (VectorReg (bv 16 0x2458) (bv 16 0))
            let vl = VL 8 (VLNum (bv 8 11)) nonePolicy False
            let actual = findFirstSet vconst8162 8 mask maskToCount vl
            let expected = Scalar (bv 8 3) False
            actual @?= Right expected,
          testCase "partial mask partial vl/minus one" $ do
            let mask = Mask @'C 8 (VectorReg (bv 16 0xf8a7) (bv 16 0))
            let maskToCount = Mask @'C 8 (VectorReg (bv 16 0x2c58) (bv 16 0))
            let vl = VL 8 (VLNum (bv 8 11)) nonePolicy False
            let actual = findFirstSet vconst8162 8 mask maskToCount vl
            let expected = Scalar (bv 8 $ -1) False
            actual @?= Right expected,
          testCase "invalid tail" $ do
            let mask = Mask @'C 8 (VectorReg (bv 16 $ -1) (bv 16 0xf800))
            let maskToCount = Mask @'C 8 (VectorReg (bv 16 0x2458) (bv 16 0xf800))
            let vl = VL 8 (VLNum (bv 8 11)) nonePolicy False
            let actual = findFirstSet vconst8162 8 mask maskToCount vl
            let expected = Scalar (bv 8 3) False
            actual @?= Right expected,
          testCase "invalid mask" $ do
            let mask = Mask @'C 8 (VectorReg (bv 16 $ -1) (bv 16 0x0400))
            let maskToCount = Mask @'C 8 (VectorReg (bv 16 0x2458) (bv 16 0))
            let vl = VL 8 (VLNum (bv 8 11)) nonePolicy False
            let actual = findFirstSet vconst8162 8 mask maskToCount vl
            let expected = Scalar (bv 8 3) True
            actual @?= Right expected,
          testCase "invalid mask to count" $ do
            let mask = Mask @'C 8 (VectorReg (bv 16 $ -1) (bv 16 0))
            let maskToCount = Mask @'C 8 (VectorReg (bv 16 0x2458) (bv 16 0x0400))
            let vl = VL 8 (VLNum (bv 8 11)) nonePolicy False
            let actual = findFirstSet vconst8162 8 mask maskToCount vl
            let expected = Scalar (bv 8 3) True
            actual @?= Right expected,
          testCase "invalid mask to count, but not masked" $ do
            let mask = Mask @'C 8 (VectorReg (bv 16 0xfbff) (bv 16 0))
            let maskToCount = Mask @'C 8 (VectorReg (bv 16 0x2458) (bv 16 0x0400))
            let vl = VL 8 (VLNum (bv 8 11)) nonePolicy False
            let actual = findFirstSet vconst8162 8 mask maskToCount vl
            let expected = Scalar (bv 8 3) False
            actual @?= Right expected,
          testCase "invalid mask, but not masked" $ do
            let mask = Mask @'C 8 (VectorReg (bv 16 0x245f) (bv 16 0x0400))
            let maskToCount = Mask @'C 8 (VectorReg (bv 16 0xfbf8) (bv 16 0))
            let vl = VL 8 (VLNum (bv 8 11)) nonePolicy False
            let actual = findFirstSet vconst8162 8 mask maskToCount vl
            let expected = Scalar (bv 8 3) False
            actual @?= Right expected,
          testCase "both invalid bit" $ do
            let mask = Mask @'C 8 (VectorReg (bv 16 0x245f) (bv 16 0x0400))
            let maskToCount = Mask @'C 8 (VectorReg (bv 16 0xfbf8) (bv 16 0x0400))
            let vl = VL 8 (VLNum (bv 8 11)) nonePolicy False
            let actual = findFirstSet vconst8162 8 mask maskToCount vl
            let expected = Scalar (bv 8 3) True
            actual @?= Right expected
        ],
      testGroup
        "setBeforeFirst/setIncludingFirst/setOnlyFirst"
        [ testCase "basic" $ do
            let mask = Mask @'C 8 (VectorReg (bv 16 0x1cf2) (bv 16 0))
            let maskToSet = Mask @'C 8 (VectorReg (bv 16 0x5789) (bv 16 0))
            let vl = VL 8 (VLNum (bv 8 11)) muPolicy False
            setBeforeFirst vconst8162 8 maskToSet mask maskToSet vl
              @?= Right (Mask 8 (VectorReg (bv 16 0x037b) (bv 16 0xf800)))
            setIncludingFirst vconst8162 8 maskToSet mask maskToSet vl
              @?= Right (Mask 8 (VectorReg (bv 16 0x03fb) (bv 16 0xf800)))
            setOnlyFirst vconst8162 8 maskToSet mask maskToSet vl
              @?= Right (Mask 8 (VectorReg (bv 16 0x0389) (bv 16 0xf800))),
          testCase "no bit" $ do
            let mask = Mask @'C 8 (VectorReg (bv 16 0x1832) (bv 16 0))
            let maskToSet = Mask @'C 8 (VectorReg (bv 16 0x5789) (bv 16 0))
            let vl = VL 8 (VLNum (bv 8 11)) muPolicy False
            setBeforeFirst vconst8162 8 maskToSet mask maskToSet vl
              @?= Right (Mask 8 (VectorReg (bv 16 0x07bb) (bv 16 0xf800)))
            setIncludingFirst vconst8162 8 maskToSet mask maskToSet vl
              @?= Right (Mask 8 (VectorReg (bv 16 0x07bb) (bv 16 0xf800)))
            setOnlyFirst vconst8162 8 maskToSet mask maskToSet vl
              @?= Right (Mask 8 (VectorReg (bv 16 0x0789) (bv 16 0xf800)))
        ]
    ]

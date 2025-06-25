module RVV.Semantics.BitSizeUtilTest (bitSizeUtilTest) where

import RVV.Semantics.BitSizeUtil (isPower2, log2, power2)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

bitSizeUtilTest :: Test
bitSizeUtilTest =
  testGroup
    "BitSizeUtil"
    [ testGroup "isPower2" $ do
        (x, expected) <-
          [ (0, False),
            (1, True),
            (2, True),
            (3, False),
            (4, True),
            (5, False),
            (31, False),
            (32, True),
            (33, False),
            (1023, False),
            (1024, True),
            (1025, False)
            ]
        return $ testCase (show x) $ isPower2 x @?= expected,
      testGroup "log2" $ do
        (n, expected) <-
          [ (1, 0),
            (2, 1),
            (3, 1),
            (4, 2),
            (5, 2),
            (31, 4),
            (32, 5),
            (33, 5),
            (1023, 9),
            (1024, 10),
            (1025, 10)
            ]
        return $ testCase (show n) $ log2 n @?= expected,
      testGroup "power2" $ do
        (n, expected) <- [(0, 1), (1, 2), (2, 4), (3, 8), (4, 16), (5, 32)]
        return $ testCase (show n) $ power2 n @?= expected
    ]

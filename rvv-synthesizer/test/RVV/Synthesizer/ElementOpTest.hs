{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RVV.Synthesizer.ElementOpTest (elementOpTest) where

import Data.Bits (Bits (xor, (.&.), (.|.)))
import Grisette
  ( BV (bv, bvSelect, bvSext, bvZext),
    DivOr (divOr, remOr),
    ITEOp (symIte),
    LogicalOp (false, symImplies, symNot, true, (.&&), (.||)),
    PPrint (pformat),
    SignConversion (toSigned, toUnsigned),
    SolvingFailure (Unsat),
    SomeSymWordN,
    SymBool,
    SymEq ((./=), (.==)),
    SymOrd ((.<), (.<=), (.>), (.>=)),
    SymShift (symShift, symShiftNegated),
    solve,
    ssymBV,
    z3,
  )
import HieraSynth.Util.Pretty (renderDoc)
import RVV.Semantics.Element
  ( MaskElement (MaskElement, maskElementData, maskElementUninitialized),
    VectorElement (VectorElement),
    maskElementIsInitialized,
  )
import RVV.Synthesizer.Parameter.FixedPointRoundingMode
  ( FixedPointRoundingMode (FixedRDN, FixedRNE, FixedRNU, FixedROD),
    interpretFixedPointClip,
  )
import RVV.Synthesizer.Parameter.IntCompareOpCode
  ( IntCompareOpCode
      ( MSEq,
        MSGe,
        MSGeu,
        MSGt,
        MSGtu,
        MSLe,
        MSLeu,
        MSLt,
        MSLtu,
        MSNe
      ),
    interpretIntCompareOpCode,
  )
import RVV.Synthesizer.Parameter.MaskLogicalOpCode
  ( MaskLogicalOpCode (MAnd, MAndn, MNand, MNor, MOr, MOrn, MXnor, MXor),
    interpretMaskLogicalOpCode,
  )
import RVV.Synthesizer.Parameter.SingleWidthIntBinaryOpCode
  ( SingleWidthIntBinaryOpCode
      ( Add,
        And,
        Div,
        Divu,
        Max,
        Maxu,
        Min,
        Minu,
        Mul,
        Mulh,
        Mulhsu,
        Mulhu,
        Or,
        RSub,
        Rem,
        Remu,
        SAdd,
        SAddu,
        SSub,
        SSubu,
        Sll,
        Slt,
        Sltu,
        Sra,
        Srl,
        Sub,
        Xor
      ),
    interpretSingleWidthBinaryOpCode,
  )
import RVV.Synthesizer.Parameter.SingleWidthMulAddOpCode
  ( SingleWidthMulAddOpCode (MAcc, MAdd, NMSac, NMSub),
    interpretSingleWidthMulAddOpCode,
  )
import RVV.Synthesizer.Parameter.WideningIntBinaryOpCode
  ( WideningIntBinaryOpCode (WAdd, WAddu, WMul, WMulsu, WMulu, WSub, WSubu),
    interpretWideningIntBinaryOpCode,
    wideningIntBinaryOpCodeWidenLhs,
    wideningIntBinaryOpCodeWidenRhs,
  )
import RVV.Synthesizer.Parameter.WideningMulAddOpCode
  ( WideningMulAddOpCode (WMAcc, WMAccsu, WMAccu, WMAccus),
    wideningMulAddOpCodeWidenLhs,
    wideningMulAddOpCodeWidenRhs,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import TestUtil.SymbolicAssertion ((.@?=))

d16 :: SomeSymWordN
d16 = ssymBV 16 "d16"

d32 :: SomeSymWordN
d32 = ssymBV 32 "d32"

a :: SomeSymWordN
a = ssymBV 16 "a"

b :: SomeSymWordN
b = ssymBV 16 "b"

abool :: SymBool
abool = "a"

bbool :: SymBool
bbool = "b"

data BinOpFuncTest op = BinOpFuncTest
  { binOpFuncTestName :: String,
    binOpFuncTestOp :: op,
    binOpFuncTestLhs :: SomeSymWordN,
    binOpFuncTestRhs :: SomeSymWordN,
    binOpFuncTestExpected :: SomeSymWordN
  }

data MulAddOpFuncTest op = MulAddOpFuncTest
  { mulAddOpFuncTestName :: String,
    mulAddOpFuncTestOp :: op,
    mulAddOpFuncTestDest :: SomeSymWordN,
    mulAddOpFuncTestLhs :: SomeSymWordN,
    mulAddOpFuncTestRhs :: SomeSymWordN,
    mulAddOpFuncTestExpected :: SomeSymWordN
  }

data CompareOpFuncTest op = CompareOpFuncTest
  { compareOpFuncTestName :: String,
    compareOpFuncTestOp :: op,
    compareOpFuncTestLhs :: SomeSymWordN,
    compareOpFuncTestRhs :: SomeSymWordN,
    compareOpFuncTestExpected :: SymBool
  }

data MaskLogicalOpFuncTest op = MaskLogicalOpFuncTest
  { interpretMaskLogicalOpCodeTestName :: String,
    interpretMaskLogicalOpCodeTestOp :: op,
    interpretMaskLogicalOpCodeTestExpectedFunc :: SymBool -> SymBool -> SymBool
  }

verify :: SymBool -> IO ()
verify b = do
  r <- solve z3 $ symNot b
  case r of
    Left Unsat -> return ()
    Left e -> fail $ show e
    Right m -> fail $ "Verify failed: got model " <> show m

elementOpTest :: Test
elementOpTest =
  testGroup
    "ElementOp"
    [ testGroup "interpretSingleWidthBinaryOpCode" $ do
        BinOpFuncTest {..} <-
          [ BinOpFuncTest
              { binOpFuncTestName = "Add",
                binOpFuncTestOp = Add,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected = a + b
              },
            BinOpFuncTest
              { binOpFuncTestName = "Sub",
                binOpFuncTestOp = Sub,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected = a - b
              },
            BinOpFuncTest
              { binOpFuncTestName = "RSub",
                binOpFuncTestOp = RSub,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected = b - a
              },
            BinOpFuncTest
              { binOpFuncTestName = "And",
                binOpFuncTestOp = And,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected = a .&. b
              },
            BinOpFuncTest
              { binOpFuncTestName = "Or",
                binOpFuncTestOp = Or,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected = a .|. b
              },
            BinOpFuncTest
              { binOpFuncTestName = "Xor",
                binOpFuncTestOp = Xor,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected = a `xor` b
              },
            BinOpFuncTest
              { binOpFuncTestName = "Mul",
                binOpFuncTestOp = Mul,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected = a * b
              },
            BinOpFuncTest
              { binOpFuncTestName = "Mulh",
                binOpFuncTestOp = Mulh,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected =
                  bvSelect 16 16 $ bvSext 32 a * bvSext 32 b
              },
            BinOpFuncTest
              { binOpFuncTestName = "Mulhu",
                binOpFuncTestOp = Mulhu,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected =
                  bvSelect 16 16 $ bvZext 32 a * bvZext 32 b
              },
            BinOpFuncTest
              { binOpFuncTestName = "Mulhsu",
                binOpFuncTestOp = Mulhsu,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected =
                  bvSelect 16 16 $ bvSext 32 a * bvZext 32 b
              },
            BinOpFuncTest
              { binOpFuncTestName = "Sll",
                binOpFuncTestOp = Sll,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected =
                  a `symShift` (b .&. bv 16 0xf)
              },
            BinOpFuncTest
              { binOpFuncTestName = "Srl",
                binOpFuncTestOp = Srl,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected = symShiftNegated a (b .&. bv 16 0xf)
              },
            BinOpFuncTest
              { binOpFuncTestName = "Sra",
                binOpFuncTestOp = Sra,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected =
                  toUnsigned $
                    symShiftNegated (toSigned a) (toSigned b .&. bv 16 0xf)
              },
            BinOpFuncTest
              { binOpFuncTestName = "Max",
                binOpFuncTestOp = Max,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected =
                  symIte (toSigned a .< toSigned b) b a
              },
            BinOpFuncTest
              { binOpFuncTestName = "Maxu",
                binOpFuncTestOp = Maxu,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected = symIte (a .< b) b a
              },
            BinOpFuncTest
              { binOpFuncTestName = "Min",
                binOpFuncTestOp = Min,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected = symIte (toSigned a .< toSigned b) a b
              },
            BinOpFuncTest
              { binOpFuncTestName = "Minu",
                binOpFuncTestOp = Minu,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected = symIte (a .< b) a b
              },
            BinOpFuncTest
              { binOpFuncTestName = "Div",
                binOpFuncTestOp = Div,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected =
                  symIte
                    (a .== bv 16 0x8000 .&& b .== bv 16 (-1))
                    (bv 16 0x8000)
                    $ symIte (b .== bv 16 0) (bv 16 $ -1)
                    $ toUnsigned
                    $ divOr (bv 16 0xbeef) (toSigned a) (toSigned b)
              },
            BinOpFuncTest
              { binOpFuncTestName = "Divu",
                binOpFuncTestOp = Divu,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected =
                  symIte (b .== bv 16 0) (bv 16 $ -1) $
                    divOr (bv 16 0xbeef) a b
              },
            BinOpFuncTest
              { binOpFuncTestName = "Rem",
                binOpFuncTestOp = Rem,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected =
                  symIte
                    (a .== bv 16 0x8000 .&& b .== bv 16 (-1))
                    (bv 16 0)
                    $ symIte (b .== bv 16 0) a
                    $ toUnsigned
                    $ remOr (bv 16 0xbeef) (toSigned a) (toSigned b)
              },
            BinOpFuncTest
              { binOpFuncTestName = "Remu",
                binOpFuncTestOp = Remu,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected =
                  symIte (b .== bv 16 0) a $ remOr (bv 16 0xbeef) a b
              },
            BinOpFuncTest
              { binOpFuncTestName = "Slt",
                binOpFuncTestOp = Slt,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected =
                  symIte (toSigned a .< toSigned b) (bv 16 1) (bv 16 0)
              },
            BinOpFuncTest
              { binOpFuncTestName = "Sltu",
                binOpFuncTestOp = Sltu,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected = symIte (a .< b) (bv 16 1) (bv 16 0)
              },
            BinOpFuncTest
              { binOpFuncTestName = "SAdd",
                binOpFuncTestOp = SAdd,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected =
                  let ai = toSigned a
                      bi = toSigned b
                   in symIte
                        (ai .< 0 .&& bi .< 0 .&& ai + bi .>= 0)
                        (bv 16 0x8000)
                        $ symIte
                          (ai .> 0 .&& bi .> 0 .&& ai + bi .<= 0)
                          (bv 16 0x7fff)
                        $ toUnsigned
                        $ ai + bi
              },
            BinOpFuncTest
              { binOpFuncTestName = "SAddu",
                binOpFuncTestOp = SAddu,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected =
                  symIte
                    (bvZext 17 a + bvZext 17 b .>= bv 17 0x10000)
                    (bv 16 0xffff)
                    (a + b)
              },
            BinOpFuncTest
              { binOpFuncTestName = "SSub",
                binOpFuncTestOp = SSub,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected =
                  let ai = toSigned a
                      bi = toSigned b
                   in symIte
                        (ai .< 0 .&& bi .> 0 .&& ai - bi .>= 0)
                        (bv 16 0x8000)
                        $ symIte
                          (ai .>= 0 .&& bi .< 0 .&& ai - bi .< 0)
                          (bv 16 0x7fff)
                        $ toUnsigned
                        $ ai - bi
              },
            BinOpFuncTest
              { binOpFuncTestName = "SSubu",
                binOpFuncTestOp = SSubu,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected = symIte (a .< b) (bv 16 0) (a - b)
              }
            ]
        return $ testCase binOpFuncTestName $ do
          let VectorElement actual _ =
                interpretSingleWidthBinaryOpCode
                  binOpFuncTestOp
                  (VectorElement binOpFuncTestLhs (0 * binOpFuncTestLhs))
                  (VectorElement binOpFuncTestRhs (0 * binOpFuncTestLhs))
          actual .@?= binOpFuncTestExpected,
      testGroup "wideningBinOpFunc" $ do
        BinOpFuncTest {..} <-
          [ BinOpFuncTest
              { binOpFuncTestName = "WAdd",
                binOpFuncTestOp = WAdd,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected = bvSext 32 a + bvSext 32 b
              },
            BinOpFuncTest
              { binOpFuncTestName = "WSub",
                binOpFuncTestOp = WSub,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected = bvSext 32 a - bvSext 32 b
              },
            BinOpFuncTest
              { binOpFuncTestName = "WAddu",
                binOpFuncTestOp = WAddu,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected = bvZext 32 a + bvZext 32 b
              },
            BinOpFuncTest
              { binOpFuncTestName = "WSubu",
                binOpFuncTestOp = WSubu,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected = bvZext 32 a - bvZext 32 b
              },
            BinOpFuncTest
              { binOpFuncTestName = "WMul",
                binOpFuncTestOp = WMul,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected = bvSext 32 a * bvSext 32 b
              },
            BinOpFuncTest
              { binOpFuncTestName = "WMulu",
                binOpFuncTestOp = WMulu,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected = bvZext 32 a * bvZext 32 b
              },
            BinOpFuncTest
              { binOpFuncTestName = "WMulsu",
                binOpFuncTestOp = WMulsu,
                binOpFuncTestLhs = a,
                binOpFuncTestRhs = b,
                binOpFuncTestExpected = bvSext 32 a * bvZext 32 b
              }
            ]
        return $ testCase binOpFuncTestName $ do
          let VectorElement actual _ =
                interpretWideningIntBinaryOpCode
                  binOpFuncTestOp
                  ( wideningIntBinaryOpCodeWidenLhs
                      binOpFuncTestOp
                      (VectorElement binOpFuncTestLhs (0 * binOpFuncTestLhs))
                  )
                  ( wideningIntBinaryOpCodeWidenRhs
                      binOpFuncTestOp
                      (VectorElement binOpFuncTestRhs (0 * binOpFuncTestRhs))
                  )
          actual .@?= binOpFuncTestExpected,
      testGroup "singleWidthMulAdd" $ do
        MulAddOpFuncTest {..} <-
          [ MulAddOpFuncTest
              { mulAddOpFuncTestName = "MAcc",
                mulAddOpFuncTestOp = MAcc,
                mulAddOpFuncTestDest = d16,
                mulAddOpFuncTestLhs = a,
                mulAddOpFuncTestRhs = b,
                mulAddOpFuncTestExpected = d16 + a * b
              },
            MulAddOpFuncTest
              { mulAddOpFuncTestName = "NMSac",
                mulAddOpFuncTestOp = NMSac,
                mulAddOpFuncTestDest = d16,
                mulAddOpFuncTestLhs = a,
                mulAddOpFuncTestRhs = b,
                mulAddOpFuncTestExpected = d16 - a * b
              },
            MulAddOpFuncTest
              { mulAddOpFuncTestName = "MAdd",
                mulAddOpFuncTestOp = MAdd,
                mulAddOpFuncTestDest = d16,
                mulAddOpFuncTestLhs = a,
                mulAddOpFuncTestRhs = b,
                mulAddOpFuncTestExpected = a * d16 + b
              },
            MulAddOpFuncTest
              { mulAddOpFuncTestName = "NMSub",
                mulAddOpFuncTestOp = NMSub,
                mulAddOpFuncTestDest = d16,
                mulAddOpFuncTestLhs = a,
                mulAddOpFuncTestRhs = b,
                mulAddOpFuncTestExpected = b - d16 * a
              }
            ]
        return $ testCase mulAddOpFuncTestName $ do
          let actual =
                interpretSingleWidthMulAddOpCode
                  mulAddOpFuncTestOp
                  mulAddOpFuncTestDest
                  mulAddOpFuncTestLhs
                  mulAddOpFuncTestRhs
          actual .@?= mulAddOpFuncTestExpected,
      testGroup "wideningMulAdd" $ do
        MulAddOpFuncTest {..} <-
          [ MulAddOpFuncTest
              { mulAddOpFuncTestName = "WMAcc",
                mulAddOpFuncTestOp = WMAcc,
                mulAddOpFuncTestDest = d32,
                mulAddOpFuncTestLhs = a,
                mulAddOpFuncTestRhs = b,
                mulAddOpFuncTestExpected =
                  d32 + bvSext 32 a * bvSext 32 b
              },
            MulAddOpFuncTest
              { mulAddOpFuncTestName = "WMAccu",
                mulAddOpFuncTestOp = WMAccu,
                mulAddOpFuncTestDest = d32,
                mulAddOpFuncTestLhs = a,
                mulAddOpFuncTestRhs = b,
                mulAddOpFuncTestExpected =
                  d32 + bvZext 32 a * bvZext 32 b
              },
            MulAddOpFuncTest
              { mulAddOpFuncTestName = "WMAccus",
                mulAddOpFuncTestOp = WMAccus,
                mulAddOpFuncTestDest = d32,
                mulAddOpFuncTestLhs = a,
                mulAddOpFuncTestRhs = b,
                mulAddOpFuncTestExpected =
                  d32 + bvZext 32 a * bvSext 32 b
              },
            MulAddOpFuncTest
              { mulAddOpFuncTestName = "WMAccsu",
                mulAddOpFuncTestOp = WMAccsu,
                mulAddOpFuncTestDest = d32,
                mulAddOpFuncTestLhs = a,
                mulAddOpFuncTestRhs = b,
                mulAddOpFuncTestExpected =
                  d32 + bvSext 32 a * bvZext 32 b
              }
            ]
        return $ testCase mulAddOpFuncTestName $ do
          let actual =
                mulAddOpFuncTestDest
                  + wideningMulAddOpCodeWidenLhs
                    mulAddOpFuncTestOp
                    mulAddOpFuncTestLhs
                    * wideningMulAddOpCodeWidenRhs
                      mulAddOpFuncTestOp
                      mulAddOpFuncTestRhs
          actual .@?= mulAddOpFuncTestExpected,
      testGroup "CompareOp" $ do
        CompareOpFuncTest {..} <-
          [ CompareOpFuncTest
              { compareOpFuncTestName = "MSEq",
                compareOpFuncTestOp = MSEq,
                compareOpFuncTestLhs = a,
                compareOpFuncTestRhs = b,
                compareOpFuncTestExpected = a .== b
              },
            CompareOpFuncTest
              { compareOpFuncTestName = "MSNe",
                compareOpFuncTestOp = MSNe,
                compareOpFuncTestLhs = a,
                compareOpFuncTestRhs = b,
                compareOpFuncTestExpected = a ./= b
              },
            CompareOpFuncTest
              { compareOpFuncTestName = "MSLt",
                compareOpFuncTestOp = MSLt,
                compareOpFuncTestLhs = a,
                compareOpFuncTestRhs = b,
                compareOpFuncTestExpected = toSigned a .< toSigned b
              },
            CompareOpFuncTest
              { compareOpFuncTestName = "MSLtu",
                compareOpFuncTestOp = MSLtu,
                compareOpFuncTestLhs = a,
                compareOpFuncTestRhs = b,
                compareOpFuncTestExpected = a .< b
              },
            CompareOpFuncTest
              { compareOpFuncTestName = "MSLe",
                compareOpFuncTestOp = MSLe,
                compareOpFuncTestLhs = a,
                compareOpFuncTestRhs = b,
                compareOpFuncTestExpected = toSigned a .<= toSigned b
              },
            CompareOpFuncTest
              { compareOpFuncTestName = "MSLeu",
                compareOpFuncTestOp = MSLeu,
                compareOpFuncTestLhs = a,
                compareOpFuncTestRhs = b,
                compareOpFuncTestExpected = a .<= b
              },
            CompareOpFuncTest
              { compareOpFuncTestName = "MSGt",
                compareOpFuncTestOp = MSGt,
                compareOpFuncTestLhs = a,
                compareOpFuncTestRhs = b,
                compareOpFuncTestExpected = toSigned a .> toSigned b
              },
            CompareOpFuncTest
              { compareOpFuncTestName = "MSGtu",
                compareOpFuncTestOp = MSGtu,
                compareOpFuncTestLhs = a,
                compareOpFuncTestRhs = b,
                compareOpFuncTestExpected = a .> b
              },
            CompareOpFuncTest
              { compareOpFuncTestName = "MSGe",
                compareOpFuncTestOp = MSGe,
                compareOpFuncTestLhs = a,
                compareOpFuncTestRhs = b,
                compareOpFuncTestExpected = toSigned a .>= toSigned b
              }
            ]
        return $ testCase compareOpFuncTestName $ do
          let actual =
                interpretIntCompareOpCode
                  compareOpFuncTestOp
                  compareOpFuncTestLhs
                  compareOpFuncTestRhs
          actual .@?= compareOpFuncTestExpected,
      testGroup "MaskLogicalOp" $ do
        MaskLogicalOpFuncTest {..} <-
          [ MaskLogicalOpFuncTest
              { interpretMaskLogicalOpCodeTestName = "MAnd",
                interpretMaskLogicalOpCodeTestOp = MAnd,
                interpretMaskLogicalOpCodeTestExpectedFunc = (.&&)
              },
            MaskLogicalOpFuncTest
              { interpretMaskLogicalOpCodeTestName = "MNand",
                interpretMaskLogicalOpCodeTestOp = MNand,
                interpretMaskLogicalOpCodeTestExpectedFunc =
                  \abool bbool -> symNot $ abool .&& bbool
              },
            MaskLogicalOpFuncTest
              { interpretMaskLogicalOpCodeTestName = "MAndn",
                interpretMaskLogicalOpCodeTestOp = MAndn,
                interpretMaskLogicalOpCodeTestExpectedFunc =
                  \abool bbool -> abool .&& symNot bbool
              },
            MaskLogicalOpFuncTest
              { interpretMaskLogicalOpCodeTestName = "MXor",
                interpretMaskLogicalOpCodeTestOp = MXor,
                interpretMaskLogicalOpCodeTestExpectedFunc = (./=)
              },
            MaskLogicalOpFuncTest
              { interpretMaskLogicalOpCodeTestName = "MOr",
                interpretMaskLogicalOpCodeTestOp = MOr,
                interpretMaskLogicalOpCodeTestExpectedFunc = (.||)
              },
            MaskLogicalOpFuncTest
              { interpretMaskLogicalOpCodeTestName = "MNor",
                interpretMaskLogicalOpCodeTestOp = MNor,
                interpretMaskLogicalOpCodeTestExpectedFunc =
                  \abool bbool -> symNot $ abool .|| bbool
              },
            MaskLogicalOpFuncTest
              { interpretMaskLogicalOpCodeTestName = "MOrn",
                interpretMaskLogicalOpCodeTestOp = MOrn,
                interpretMaskLogicalOpCodeTestExpectedFunc =
                  \abool bbool -> abool .|| symNot bbool
              },
            MaskLogicalOpFuncTest
              { interpretMaskLogicalOpCodeTestName = "MXnor",
                interpretMaskLogicalOpCodeTestOp = MXnor,
                interpretMaskLogicalOpCodeTestExpectedFunc = (.==)
              }
            ]
        return $ testCase interpretMaskLogicalOpCodeTestName $ do
          let auninit = "auninit" :: SymBool
          let buninit = "buninit" :: SymBool
          let actual =
                interpretMaskLogicalOpCode
                  interpretMaskLogicalOpCodeTestOp
                  (MaskElement abool auninit)
                  (MaskElement bbool buninit)
          maskElementData actual .@?= interpretMaskLogicalOpCodeTestExpectedFunc abool bbool
          verify $
            (symNot auninit .&& symNot buninit)
              `symImplies` maskElementIsInitialized actual
          verify $
            symImplies
              (auninit .&& symNot buninit)
              ( maskElementIsInitialized actual
                  .== ( interpretMaskLogicalOpCodeTestExpectedFunc true bbool
                          .== interpretMaskLogicalOpCodeTestExpectedFunc false bbool
                      )
              )
          verify $
            symImplies
              (symNot auninit .&& buninit)
              ( maskElementIsInitialized actual
                  .== ( interpretMaskLogicalOpCodeTestExpectedFunc abool true
                          .== interpretMaskLogicalOpCodeTestExpectedFunc abool false
                      )
              )
          verify $
            symImplies
              (auninit .&& buninit)
              (maskElementUninitialized actual),
      testGroup
        "vnclipRoundingOpFunc"
        [ testGroup "RNU" $ do
            (value, expected) <-
              [ (bv 16 0x0800, bv 8 0x02),
                (bv 16 0x0900, bv 8 0x02),
                (bv 16 0x0a00, bv 8 0x03),
                (bv 16 0x0b00, bv 8 0x03),
                (bv 16 0x0c00, bv 8 0x03),
                (bv 16 0x0d00, bv 8 0x03),
                (bv 16 0x0e00, bv 8 0x04),
                (bv 16 0x0f00, bv 8 0x04),
                (bv 16 0x1000, bv 8 0x04)
                ]
            return $ testCase (show value) $ do
              let actual =
                    interpretFixedPointClip
                      FixedRNU
                      True
                      (VectorElement value (bv 16 0))
                      (VectorElement (bv 8 10) (bv 8 0))
              actual @?= VectorElement expected (bv 8 0),
          testGroup "RNE" $ do
            (value, expected) <-
              [ (bv 16 0x0800, bv 8 0x02),
                (bv 16 0x0900, bv 8 0x02),
                (bv 16 0x0a00, bv 8 0x02),
                (bv 16 0x0b00, bv 8 0x03),
                (bv 16 0x0c00, bv 8 0x03),
                (bv 16 0x0d00, bv 8 0x03),
                (bv 16 0x0e00, bv 8 0x04),
                (bv 16 0x0f00, bv 8 0x04),
                (bv 16 0x1000, bv 8 0x04)
                ]
            return $ testCase (show value) $ do
              let actual =
                    interpretFixedPointClip
                      FixedRNE
                      True
                      (VectorElement value (bv 16 0))
                      (VectorElement (bv 8 10) (bv 8 0))
              actual @?= VectorElement expected (bv 8 0),
          testGroup "RDN" $ do
            (value, expected) <-
              [ (bv 16 0x0800, bv 8 0x02),
                (bv 16 0x0900, bv 8 0x02),
                (bv 16 0x0a00, bv 8 0x02),
                (bv 16 0x0b00, bv 8 0x02),
                (bv 16 0x0c00, bv 8 0x03),
                (bv 16 0x0d00, bv 8 0x03),
                (bv 16 0x0e00, bv 8 0x03),
                (bv 16 0x0f00, bv 8 0x03),
                (bv 16 0x1000, bv 8 0x04)
                ]
            return $ testCase (show value) $ do
              let actual =
                    interpretFixedPointClip
                      FixedRDN
                      True
                      (VectorElement value (bv 16 0))
                      (VectorElement (bv 8 10) (bv 8 0))
              actual @?= VectorElement expected (bv 8 0),
          testGroup "ROD" $ do
            (value, expected) <-
              [ (bv 16 0x0800, bv 8 0x02),
                (bv 16 0x0900, bv 8 0x03),
                (bv 16 0x0a00, bv 8 0x03),
                (bv 16 0x0b00, bv 8 0x03),
                (bv 16 0x0c00, bv 8 0x03),
                (bv 16 0x0d00, bv 8 0x03),
                (bv 16 0x0e00, bv 8 0x03),
                (bv 16 0x0f00, bv 8 0x03),
                (bv 16 0x1000, bv 8 0x04)
                ]
            return $ testCase (show value) $ do
              let actual =
                    interpretFixedPointClip
                      FixedROD
                      True
                      (VectorElement value (bv 16 0))
                      (VectorElement (bv 8 10) (bv 8 0))
              actual @?= VectorElement expected (bv 8 0),
          testGroup "Signedness" $ do
            (signed, expected) <- [(True, bv 8 0xe0), (False, bv 8 0x20)]
            return $ testCase (show signed) $ do
              let actual =
                    interpretFixedPointClip
                      FixedRNU
                      signed
                      (VectorElement (bv 16 0x8000) (bv 16 0))
                      (VectorElement (bv 8 10) (bv 8 0))
              actual @?= VectorElement expected (bv 8 0),
          testGroup "Invalidity" $ do
            (name, signed :: Bool, lhs, rhs, expected) <-
              [ ( "invalid lhs",
                  True,
                  VectorElement (bv 16 0x8000) (bv 16 0x0001),
                  VectorElement (bv 8 10) (bv 8 0),
                  VectorElement (bv 8 0xe0) (bv 8 0xff)
                ),
                ( "invalid rhs",
                  True,
                  VectorElement (bv 16 0x8000) (bv 16 0x0000),
                  VectorElement (bv 8 10) (bv 8 0x04),
                  VectorElement (bv 8 0xe0) (bv 8 0xff)
                ),
                ( "unrelated invalid rhs",
                  True,
                  VectorElement (bv 16 0x8000) (bv 16 0x0000),
                  VectorElement (bv 8 10) (bv 8 0x10),
                  VectorElement (bv 8 0xe0) (bv 8 0x00)
                )
                ]
            return $ testCase name $ do
              let actual = interpretFixedPointClip FixedRNU signed lhs rhs
              actual @?= expected
        ],
      testGroup
        "PPrint"
        [ testGroup "BinOp" $ do
            (op, txt) <-
              [ (Add, "add"),
                (Sub, "sub"),
                (And, "and"),
                (Or, "or"),
                (Xor, "xor"),
                (Mul, "mul"),
                (Mulh, "mulh"),
                (Mulhu, "mulhu"),
                (Mulhsu, "mulhsu"),
                (Sll, "sll"),
                (Srl, "srl"),
                (Sra, "sra"),
                (Min, "min"),
                (Minu, "minu"),
                (Max, "max"),
                (Maxu, "maxu"),
                (Div, "div"),
                (Divu, "divu"),
                (Rem, "rem"),
                (Remu, "remu"),
                (Slt, "slt"),
                (Sltu, "sltu")
                ]
            return $ testCase (show op) $ renderDoc 80 (pformat op) @?= txt,
          testGroup "BinOp" $ do
            (op, txt) <-
              [ (WAddu, "waddu"),
                (WSubu, "wsubu"),
                (WAdd, "wadd"),
                (WSub, "wsub"),
                (WMul, "wmul"),
                (WMulu, "wmulu"),
                (WMulsu, "wmulsu")
                ]
            return $ testCase (show op) $ renderDoc 80 (pformat op) @?= txt,
          testGroup "SingleWidthMulAddOp" $ do
            (op, txt) <-
              [ (MAdd, "madd"),
                (MAcc, "macc"),
                (NMSac, "nmsac"),
                (NMSub, "nmsub")
                ]
            return $ testCase (show op) $ renderDoc 80 (pformat op) @?= txt,
          testGroup "WideningMulAddOp" $ do
            (op, txt) <-
              [ (WMAcc, "wmacc"),
                (WMAccu, "wmaccu"),
                (WMAccsu, "wmaccsu"),
                (WMAccus, "wmaccus")
                ]
            return $ testCase (show op) $ renderDoc 80 (pformat op) @?= txt,
          testGroup "CompareOp" $ do
            (op, txt) <-
              [ (MSEq, "mseq"),
                (MSNe, "msne"),
                (MSLt, "mslt"),
                (MSLtu, "msltu"),
                (MSLe, "msle"),
                (MSLeu, "msleu"),
                (MSGt, "msgt"),
                (MSGtu, "msgtu"),
                (MSGe, "msge"),
                (MSGeu, "msgeu")
                ]
            return $ testCase (show op) $ renderDoc 80 (pformat op) @?= txt,
          testGroup "MaskLogicalOp" $ do
            (op, txt) <-
              [ (MAnd, "mand"),
                (MNand, "mnand"),
                (MAndn, "mandn"),
                (MXor, "mxor"),
                (MOr, "mor"),
                (MNor, "mnor"),
                (MOrn, "morn"),
                (MXnor, "mxnor")
                ]
            return $ testCase (show op) $ renderDoc 80 (pformat op) @?= txt
        ]
    ]

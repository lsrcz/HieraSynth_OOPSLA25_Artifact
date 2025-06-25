{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module OptimalSketch (getOptimalSketch) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Grisette (WordN)
import HieraSynth.Program.Choice.ChoiceTree (ChoiceTree (Leaf))
import HieraSynth.Program.Choice.ComponentBag (ComponentBag (ComponentBag))
import HieraSynth.Program.SymbolTable (SymbolTable (SymbolTable))
import RVV.Synthesizer.Op (SketchSpec (CompSpec), SketchSpecTable)
import RVV.Synthesizer.Operator.Common.ImmSpec
  ( ImmSpec
      ( ArbitraryImmSpec,
        ConstImmSpec
      ),
  )
import RVV.Synthesizer.Operator.Common.RHSSpec (SketchRHSSpec (SketchImmRHS, SketchScalarRHS))
import RVV.Synthesizer.Operator.ScalarOperator (sketchScalarBin, sketchScalarUnary)
import RVV.Synthesizer.Operator.Scalar (sketchScalarLongImm)
import RVV.Synthesizer.Parameter.SingleWidthIntBinaryOpCode
  ( SingleWidthIntBinaryOpCode
      ( Add,
        And,
        Andn,
        CZeroEqz,
        CZeroNez,
        Div,
        Max,
        Maxu,
        Mulh,
        Or,
        Sll,
        Sltu,
        Sra,
        Sub,
        Xnor,
        Xor
      ),
  )
import RVV.Synthesizer.Parameter.SingleWidthIntUnaryOpCode
  ( SingleWidthIntUnaryOpCode (CPop, Clz, Neg, Seqz),
  )
import RVV.Synthesizer.Type (ValueType (ScalarType))

-- Optimal sketches based on the synthesis results

-- p01: Extract the rightmost bit that is set (x & (-x))
sketch01 :: Bool -> SketchSpecTable (WordN 8)
sketch01 _ =
  SymbolTable
    [ ( "p01",
        CompSpec $
          ComponentBag
            [ScalarType (1 / 2)]
            [ (Leaf [sketchScalarUnary [Neg] (1 / 2)], 1),
              (Leaf [sketchScalarBin [Andn] (1 / 2) SketchScalarRHS], 1)
            ]
            [ScalarType (1 / 2)]
      )
    ]

-- p02: Turn off the rightmost 1-bit
sketch02 :: Bool -> SketchSpecTable (WordN 8)
sketch02 fixedImm =
  SymbolTable
    [ ( "p02",
        CompSpec $
          ComponentBag
            [ScalarType (1 / 2)]
            [ (Leaf [sketchScalarBin [Add] (1 / 2) (SketchImmRHS (if fixedImm then ConstImmSpec 1 else ArbitraryImmSpec))], 1),
              (Leaf [sketchScalarBin [And] (1 / 2) SketchScalarRHS], 1)
            ]
            [ScalarType (1 / 2)]
      )
    ]

-- p03: Turn off the rightmost 1-bit using two's complement
sketch03 :: Bool -> SketchSpecTable (WordN 8)
sketch03 _ =
  SymbolTable
    [ ( "p03",
        CompSpec $
          ComponentBag
            [ScalarType (1 / 2)]
            [ (Leaf [sketchScalarUnary [Neg] (1 / 2)], 1),
              (Leaf [sketchScalarBin [And] (1 / 2) SketchScalarRHS], 1)
            ]
            [ScalarType (1 / 2)]
      )
    ]

-- p04: Create a mask with bits set between the rightmost 0 and 1 bits
sketch04 :: Bool -> SketchSpecTable (WordN 8)
sketch04 fixedImm =
  SymbolTable
    [ ( "p04",
        CompSpec $
          ComponentBag
            [ScalarType (1 / 2)]
            [ (Leaf [sketchScalarBin [Add] (1 / 2) (SketchImmRHS (if fixedImm then ConstImmSpec 0xffffffff else ArbitraryImmSpec))], 1),
              (Leaf [sketchScalarBin [Xor] (1 / 2) SketchScalarRHS], 1)
            ]
            [ScalarType (1 / 2)]
      )
    ]

-- p05: Set all bits to the right of the rightmost 1-bit
sketch05 :: Bool -> SketchSpecTable (WordN 8)
sketch05 fixedImm =
  SymbolTable
    [ ( "p05",
        CompSpec $
          ComponentBag
            [ScalarType (1 / 2)]
            [ (Leaf [sketchScalarBin [Add] (1 / 2) (SketchImmRHS (if fixedImm then ConstImmSpec 0xffffffff else ArbitraryImmSpec))], 1),
              (Leaf [sketchScalarBin [Or] (1 / 2) SketchScalarRHS], 1)
            ]
            [ScalarType (1 / 2)]
      )
    ]

-- p06: Set all bits to the right of the rightmost 0-bit
sketch06 :: Bool -> SketchSpecTable (WordN 8)
sketch06 fixedImm =
  SymbolTable
    [ ( "p06",
        CompSpec $
          ComponentBag
            [ScalarType (1 / 2)]
            [ (Leaf [sketchScalarBin [Add] (1 / 2) (SketchImmRHS (if fixedImm then ConstImmSpec 1 else ArbitraryImmSpec))], 1),
              (Leaf [sketchScalarBin [Or] (1 / 2) SketchScalarRHS], 1)
            ]
            [ScalarType (1 / 2)]
      )
    ]

-- p07: Create a mask with a single bit set in the position of the rightmost 0-bit
sketch07 :: Bool -> SketchSpecTable (WordN 8)
sketch07 fixedImm =
  SymbolTable
    [ ( "p07",
        CompSpec $
          ComponentBag
            [ScalarType (1 / 2)]
            [ (Leaf [sketchScalarBin [Add] (1 / 2) (SketchImmRHS (if fixedImm then ConstImmSpec 1 else ArbitraryImmSpec))], 1),
              (Leaf [sketchScalarBin [Andn] (1 / 2) SketchScalarRHS], 1)
            ]
            [ScalarType (1 / 2)]
      )
    ]

-- p08: Create a mask with a single bit set in the position of the first 0-bit from the right
sketch08 :: Bool -> SketchSpecTable (WordN 8)
sketch08 fixedImm =
  SymbolTable
    [ ( "p08",
        CompSpec $
          ComponentBag
            [ScalarType (1 / 2)]
            [ (Leaf [sketchScalarBin [Add] (1 / 2) (SketchImmRHS (if fixedImm then ConstImmSpec 0xffffffff else ArbitraryImmSpec))], 1),
              (Leaf [sketchScalarBin [Andn] (1 / 2) SketchScalarRHS], 1)
            ]
            [ScalarType (1 / 2)]
      )
    ]

-- p09: Absolute value of a signed integer
sketch09 :: Bool -> SketchSpecTable (WordN 8)
sketch09 _ =
  SymbolTable
    [ ( "p09",
        CompSpec $
          ComponentBag
            [ScalarType (1 / 2)]
            [ (Leaf [sketchScalarUnary [Neg] (1 / 2)], 1),
              (Leaf [sketchScalarBin [Max] (1 / 2) SketchScalarRHS], 1)
            ]
            [ScalarType (1 / 2)]
      )
    ]

-- p10: Test if two numbers have the same sign
sketch10 :: Bool -> SketchSpecTable (WordN 8)
sketch10 _ =
  SymbolTable
    [ ( "p10",
        CompSpec $
          ComponentBag
            [ScalarType (1 / 2), ScalarType (1 / 2)]
            [ (Leaf [sketchScalarUnary [Clz] (1 / 2)], 2),
              (Leaf [sketchScalarBin [Sub] (1 / 2) SketchScalarRHS], 1),
              (Leaf [sketchScalarUnary [Seqz] (1 / 2)], 1)
            ]
            [ScalarType (1 / 2)]
      )
    ]

-- p11: Test if a value is greater than all ones that may be present in another value
sketch11 :: Bool -> SketchSpecTable (WordN 8)
sketch11 _ =
  SymbolTable
    [ ( "p11",
        CompSpec $
          ComponentBag
            [ScalarType (1 / 2), ScalarType (1 / 2)]
            [ (Leaf [sketchScalarBin [Andn] (1 / 2) SketchScalarRHS], 1),
              (Leaf [sketchScalarBin [Sltu] (1 / 2) SketchScalarRHS], 1)
            ]
            [ScalarType (1 / 2)]
      )
    ]

-- p12: Test if a value is less than or equal to all ones that may be present in another value
sketch12 :: Bool -> SketchSpecTable (WordN 8)
sketch12 _ =
  SymbolTable
    [ ( "p12",
        CompSpec $
          ComponentBag
            [ScalarType (1 / 2), ScalarType (1 / 2)]
            [ (Leaf [sketchScalarBin [Andn] (1 / 2) SketchScalarRHS], 1),
              (Leaf [sketchScalarBin [Sltu] (1 / 2) SketchScalarRHS], 1),
              (Leaf [sketchScalarUnary [Seqz] (1 / 2)], 1)
            ]
            [ScalarType (1 / 2)]
      )
    ]

-- p13: Extract the sign of a number
sketch13 :: Bool -> SketchSpecTable (WordN 8)
sketch13 fixedImm =
  SymbolTable
    [ ( "p13",
        CompSpec $
          ComponentBag
            [ScalarType (1 / 2)]
            [ (Leaf [sketchScalarLongImm (1 / 2) (if fixedImm then ConstImmSpec 0xffffffff else ArbitraryImmSpec)], 1),
              (Leaf [sketchScalarBin [CZeroEqz] (1 / 2) SketchScalarRHS], 1)
            ]
            [ScalarType (1 / 2)]
      )
    ]

-- p14: Calculate the average of two integers without overflow
sketch14 :: Bool -> SketchSpecTable (WordN 8)
sketch14 fixedImm =
  SymbolTable
    [ ( "p14",
        CompSpec $
          ComponentBag
            [ScalarType (1 / 2), ScalarType (1 / 2)]
            [ (Leaf [sketchScalarBin [Xor] (1 / 2) SketchScalarRHS], 1),
              (Leaf [sketchScalarBin [Sra] (1 / 2) (SketchImmRHS (if fixedImm then ConstImmSpec 1 else ArbitraryImmSpec))], 1),
              (Leaf [sketchScalarBin [And] (1 / 2) SketchScalarRHS], 1),
              (Leaf [sketchScalarBin [Add] (1 / 2) SketchScalarRHS], 1)
            ]
            [ScalarType (1 / 2)]
      )
    ]

-- p15: Calculate the high median of two integers
sketch15 :: Bool -> SketchSpecTable (WordN 8)
sketch15 fixedImm =
  SymbolTable
    [ ( "p15",
        CompSpec $
          ComponentBag
            [ScalarType (1 / 2), ScalarType (1 / 2)]
            [ (Leaf [sketchScalarBin [Xor] (1 / 2) SketchScalarRHS], 1),
              (Leaf [sketchScalarBin [Sra] (1 / 2) (SketchImmRHS (if fixedImm then ConstImmSpec 1 else ArbitraryImmSpec))], 1),
              (Leaf [sketchScalarBin [Or] (1 / 2) SketchScalarRHS], 1),
              (Leaf [sketchScalarBin [Sub] (1 / 2) SketchScalarRHS], 1)
            ]
            [ScalarType (1 / 2)]
      )
    ]

-- p16: Compute the maximum of two integers
sketch16 :: Bool -> SketchSpecTable (WordN 8)
sketch16 _ =
  SymbolTable
    [ ( "p16",
        CompSpec $
          ComponentBag
            [ScalarType (1 / 2), ScalarType (1 / 2)]
            [ (Leaf [sketchScalarBin [Maxu] (1 / 2) SketchScalarRHS], 1)
            ]
            [ScalarType (1 / 2)]
      )
    ]

-- p17: Clear all but the lowest bit in a word
sketch17 :: Bool -> SketchSpecTable (WordN 8)
sketch17 _ =
  SymbolTable
    [ ( "p17",
        CompSpec $
          ComponentBag
            [ScalarType (1 / 2)]
            [ (Leaf [sketchScalarUnary [Neg] (1 / 2)], 1),
              (Leaf [sketchScalarBin [Xor] (1 / 2) SketchScalarRHS], 1),
              (Leaf [sketchScalarBin [Add] (1 / 2) SketchScalarRHS], 1),
              (Leaf [sketchScalarBin [Andn] (1 / 2) SketchScalarRHS], 1)
            ]
            [ScalarType (1 / 2)]
      )
    ]

-- p18: Find if a given number is a power of 2
sketch18 :: Bool -> SketchSpecTable (WordN 8)
sketch18 _ =
  SymbolTable
    [ ( "p18",
        CompSpec $
          ComponentBag
            [ScalarType (1 / 2)]
            [ (Leaf [sketchScalarUnary [CPop] (1 / 2)], 1),
              (Leaf [sketchScalarBin [Add] (1 / 2) (SketchImmRHS (ConstImmSpec 0xffffffff))], 1),
              (Leaf [sketchScalarUnary [Seqz] (1 / 2)], 1)
            ]
            [ScalarType (1 / 2)]
      )
    ]

-- p19: Bit reversal within each field specified by a mask
sketch19 :: Bool -> SketchSpecTable (WordN 8)
sketch19 _ =
  SymbolTable
    [ ( "p19",
        CompSpec $
          ComponentBag
            [ScalarType (1 / 2), ScalarType (1 / 2), ScalarType (1 / 2)]
            [ (Leaf [sketchScalarBin [Sra] (1 / 2) SketchScalarRHS], 1),
              (Leaf [sketchScalarBin [Xnor] (1 / 2) SketchScalarRHS], 3),
              (Leaf [sketchScalarBin [Andn] (1 / 2) SketchScalarRHS], 1),
              (Leaf [sketchScalarBin [Sll] (1 / 2) SketchScalarRHS], 1)
            ]
            [ScalarType (1 / 2)]
      )
    ]

-- p20: Find next higher number with same number of set bits
sketch20 :: Bool -> SketchSpecTable (WordN 8)
sketch20 fixedImm =
  SymbolTable
    [ ( "p20",
        CompSpec $
          ComponentBag
            [ScalarType (1 / 2)]
            [ (Leaf [sketchScalarBin [Add] (1 / 2) (SketchImmRHS (if fixedImm then ConstImmSpec 0xffffffff else ArbitraryImmSpec))], 1),
              (Leaf [sketchScalarBin [Andn] (1 / 2) SketchScalarRHS], 1),
              (Leaf [sketchScalarBin [Div] (1 / 2) SketchScalarRHS], 1),
              (Leaf [sketchScalarBin [Sra] (1 / 2) (SketchImmRHS (if fixedImm then ConstImmSpec 2 else ArbitraryImmSpec))], 1),
              (Leaf [sketchScalarBin [Add] (1 / 2) SketchScalarRHS], 1),
              (Leaf [sketchScalarBin [Or] (1 / 2) SketchScalarRHS], 1)
            ]
            [ScalarType (1 / 2)]
      )
    ]

-- p21: Implements a multiplexer (ternary conditional) based on x matching constants
sketch21 :: Bool -> SketchSpecTable (WordN 8)
sketch21 _ =
  SymbolTable
    [ ( "p21",
        CompSpec $
          ComponentBag
            [ScalarType (1 / 2), ScalarType (1 / 2), ScalarType (1 / 2), ScalarType (1 / 2)]
            [ (Leaf [sketchScalarBin [Xor] (1 / 2) SketchScalarRHS], 5),
              (Leaf [sketchScalarBin [CZeroEqz] (1 / 2) SketchScalarRHS], 1),
              (Leaf [sketchScalarBin [CZeroNez] (1 / 2) SketchScalarRHS], 1)
            ]
            [ScalarType (1 / 2)]
      )
    ]

-- p22: Population count (count bits) with specific bit manipulation pattern
sketch22 :: Bool -> SketchSpecTable (WordN 8)
sketch22 fixedImm =
  SymbolTable
    [ ( "p22",
        CompSpec $
          ComponentBag
            [ScalarType (1 / 2)]
            [ (Leaf [sketchScalarUnary [CPop] (1 / 2)], 1),
              (Leaf [sketchScalarLongImm (1 / 2) (if fixedImm then ConstImmSpec 1 else ArbitraryImmSpec)], 1),
              (Leaf [sketchScalarBin [And] (1 / 2) SketchScalarRHS], 1)
            ]
            [ScalarType (1 / 2)]
      )
    ]

-- p23: Population count (number of 1 bits) using a more standard approach
sketch23 :: Bool -> SketchSpecTable (WordN 8)
sketch23 _ =
  SymbolTable
    [ ( "p23",
        CompSpec $
          ComponentBag
            [ScalarType (1 / 2)]
            [ (Leaf [sketchScalarUnary [CPop] (1 / 2)], 1)
            ]
            [ScalarType (1 / 2)]
      )
    ]

-- p24: Round up to next power of 2
sketch24 :: Bool -> SketchSpecTable (WordN 8)
sketch24 fixedImm =
  SymbolTable
    [ ( "p24",
        CompSpec $
          ComponentBag
            [ScalarType (1 / 2)]
            [ (Leaf [sketchScalarUnary [Clz] (1 / 2)], 1),
              (Leaf [sketchScalarLongImm (1 / 2) (if fixedImm then ConstImmSpec 0x7fffffff else ArbitraryImmSpec)], 1),
              (Leaf [sketchScalarBin [Sra] (1 / 2) SketchScalarRHS], 1),
              (Leaf [sketchScalarBin [Add] (1 / 2) SketchScalarRHS], 1),
              (Leaf [sketchScalarBin [Andn] (1 / 2) SketchScalarRHS], 1)
            ]
            [ScalarType (1 / 2)]
      )
    ]

-- p25: Long multiplication of two 32-bit numbers with 64-bit result
sketch25 :: Bool -> SketchSpecTable (WordN 8)
sketch25 _ =
  SymbolTable
    [ ( "p25",
        CompSpec $
          ComponentBag
            [ScalarType (1 / 2), ScalarType (1 / 2)]
            [ (Leaf [sketchScalarBin [Mulh] (1 / 2) SketchScalarRHS], 1)
            ]
            [ScalarType (1 / 2)]
      )
    ]

sketches :: HM.HashMap T.Text (Bool -> SketchSpecTable (WordN 8))
sketches =
  HM.fromList
    [ ("p01", sketch01),
      ("p02", sketch02),
      ("p03", sketch03),
      ("p04", sketch04),
      ("p05", sketch05),
      ("p06", sketch06),
      ("p07", sketch07),
      ("p08", sketch08),
      ("p09", sketch09),
      ("p10", sketch10),
      ("p11", sketch11),
      ("p12", sketch12),
      ("p13", sketch13),
      ("p14", sketch14),
      ("p15", sketch15),
      ("p16", sketch16),
      ("p17", sketch17),
      ("p18", sketch18),
      ("p19", sketch19),
      ("p20", sketch20),
      ("p21", sketch21),
      ("p22", sketch22),
      ("p23", sketch23),
      ("p24", sketch24),
      ("p25", sketch25)
    ]

-- Map problem names to their corresponding sketch functions
getOptimalSketch :: Bool -> T.Text -> SketchSpecTable (WordN 8)
getOptimalSketch fixedImm problem = (sketches HM.! problem) fixedImm

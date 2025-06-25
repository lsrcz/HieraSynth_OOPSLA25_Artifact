{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module StdLibSketch (getStdLibSketch) where

import qualified Data.Text as T
import Grisette
  ( WordN,
  )
import HieraSynth.Program.Choice.ChoiceTree
  ( ChoiceTree (Leaf),
  )
import HieraSynth.Program.Choice.ComponentBag
  ( ComponentBag (ComponentBag),
  )
import HieraSynth.Program.SymbolTable (SymbolTable (SymbolTable))
import RVV.Synthesizer.Op
  ( SketchOp,
    SketchSpec (CompSpec),
    SketchSpecTable,
  )
import RVV.Synthesizer.Operator.Common.ImmSpec
  ( ImmSpec
      ( ArbitraryImmSpec,
        ConstImmSpec
      ),
  )
import RVV.Synthesizer.Operator.Common.RHSSpec (SketchRHSSpec (SketchScalarRHS))
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
        Xor
      ),
  )
import RVV.Synthesizer.Parameter.SingleWidthIntUnaryOpCode
  ( SingleWidthIntUnaryOpCode (CPop, Clz, Neg, Not, Seqz),
  )
import RVV.Synthesizer.Type (ValueType (ScalarType))

getStdLibSketch :: Bool -> T.Text -> SketchSpecTable (WordN 8)
getStdLibSketch fixedImm name
  | name `elem` ["p07", "p08"] =
      sketchStandardLibrary fixedImm 1 [sketchScalarBin [Andn] (1 / 2) SketchScalarRHS] name
  | name == "p09" =
      sketchStandardLibrary fixedImm 1 [sketchScalarBin [Max] (1 / 2) SketchScalarRHS] name
  | name `elem` ["p11", "p12"] =
      sketchStandardLibrary fixedImm 2 [sketchScalarBin [Andn] (1 / 2) SketchScalarRHS] name
  | name == "p13" =
      sketchStandardLibrary
        fixedImm
        1
        [ sketchScalarLongImm (1 / 2) (if fixedImm then ConstImmSpec (-1) else ArbitraryImmSpec),
          sketchScalarBin [CZeroEqz] (1 / 2) SketchScalarRHS
        ]
        name
  | name == "p16" =
      sketchStandardLibrary fixedImm 2 [sketchScalarBin [Maxu] (1 / 2) SketchScalarRHS] name
  | name == "p19" =
      sketchStandardLibrary
        fixedImm
        3
        [ sketchScalarBin [Sll] (1 / 2) SketchScalarRHS,
          sketchScalarBin [Xor] (1 / 2) SketchScalarRHS,
          sketchScalarBin [Xor] (1 / 2) SketchScalarRHS
        ]
        name
  | name == "p20" =
      sketchStandardLibrary
        fixedImm
        1
        [ sketchScalarBin [Div] (1 / 2) SketchScalarRHS,
          sketchScalarLongImm (1 / 2) (if fixedImm then ConstImmSpec 2 else ArbitraryImmSpec)
        ]
        name
  | name == "p21" =
      sketchStandardLibrary
        fixedImm
        4
        [ sketchScalarBin [CZeroNez] (1 / 2) SketchScalarRHS,
          sketchScalarBin [CZeroNez] (1 / 2) SketchScalarRHS,
          sketchScalarBin [Xor] (1 / 2) SketchScalarRHS,
          sketchScalarBin [Xor] (1 / 2) SketchScalarRHS,
          sketchScalarBin [Xor] (1 / 2) SketchScalarRHS,
          sketchScalarBin [Xor] (1 / 2) SketchScalarRHS
        ]
        name
  | name == "p22" =
      sketchStandardLibrary
        fixedImm
        1
        [sketchScalarUnary [CPop] (1 / 2)]
        name
  | name == "p23" =
      sketchStandardLibrary
        fixedImm
        1
        [sketchScalarUnary [CPop] (1 / 2)]
        name
  | name == "p24" =
      sketchStandardLibrary
        fixedImm
        1
        [ sketchScalarLongImm (1 / 2) (if fixedImm then ConstImmSpec 0x7fffffff else ArbitraryImmSpec),
          sketchScalarUnary [Clz] (1 / 2)
        ]
        name
  | name == "p25" =
      sketchStandardLibrary
        fixedImm
        2
        [ sketchScalarBin [Mulh] (1 / 2) SketchScalarRHS
        ]
        name
  | name `elem` ["p10", "p14", "p15"] =
      sketchStandardLibrary fixedImm 2 [] name
  | otherwise = sketchStandardLibrary fixedImm 1 [] name

sketchStandardLibrary :: Bool -> Int -> [SketchOp] -> T.Text -> SketchSpecTable (WordN 8)
sketchStandardLibrary fixedImm argNum ops symbol =
  SymbolTable
    [ ( symbol,
        CompSpec $
          ComponentBag
            (replicate argNum $ ScalarType (1 / 2))
            ( fmap (\op -> (Leaf [op], 1)) $
                ops
                  ++ [ sketchScalarUnary [Not] (1 / 2),
                       sketchScalarUnary [Seqz] (1 / 2),
                       sketchScalarUnary [Neg] (1 / 2),
                       sketchScalarBin [Add] (1 / 2) SketchScalarRHS,
                       sketchScalarBin [Sub] (1 / 2) SketchScalarRHS,
                       sketchScalarLongImm (1 / 2) (if fixedImm then ConstImmSpec 1 else ArbitraryImmSpec),
                       sketchScalarLongImm (1 / 2) (if fixedImm then ConstImmSpec 31 else ArbitraryImmSpec),
                       sketchScalarBin [Sra] (1 / 2) SketchScalarRHS,
                       sketchScalarBin [And] (1 / 2) SketchScalarRHS,
                       sketchScalarBin [Sltu] (1 / 2) SketchScalarRHS,
                       sketchScalarBin [Xor] (1 / 2) SketchScalarRHS,
                       sketchScalarBin [Or] (1 / 2) SketchScalarRHS
                     ]
            )
            [ScalarType (1 / 2)]
      )
    ]

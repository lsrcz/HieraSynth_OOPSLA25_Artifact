{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module RVV.Synthesizer.Operator.Common.ImmSpec (ImmSpec (..)) where

import Data.Bits (FiniteBits (finiteBitSize))
import Grisette
  ( GenSym (fresh),
    GenSymSimple (simpleFresh),
    PPrint (pformat),
    SimpleListSpec (SimpleListSpec),
    WordN,
    chooseFresh,
  )
import HieraSynth.Util.Pretty (parenCommaList)
import Grisette.Unified (EvalModeTag (C, S))
import RVV.Semantics.Imm
  ( Imm (ArbitraryImm, BoundedImm, ConstImm, NumBitsBoundedImm, ReplicatingImm, TabularImm),
  )
import RVV.Synthesizer.SketchGen.ToFastSketch (ToFastSketch (toFastSketch))
import RVV.Util.Derive (deriveNoSymEval)

data ImmSpec
  = ConstImmSpec (WordN 64)
  | TabularImmSpec [(Int, WordN 64)] (WordN 64)
  | ReplicatingImmSpec Int
  | ArbitraryImmSpec
  | BoundedImmSpec Bool (WordN 64) (WordN 64)
  | NumBitsBoundedImmSpec Bool

deriveNoSymEval [''ImmSpec]

instance ToFastSketch (Imm 'C) ImmSpec where
  toFastSketch (ConstImm v) = ConstImmSpec v
  toFastSketch (ArbitraryImm _) = ArbitraryImmSpec
  toFastSketch (TabularImm l d) = TabularImmSpec l d
  toFastSketch (ReplicatingImm imm) =
    ReplicatingImmSpec $ finiteBitSize imm
  toFastSketch (BoundedImm signed lower upper _) =
    BoundedImmSpec signed lower upper
  toFastSketch (NumBitsBoundedImm allowNegative _) =
    NumBitsBoundedImmSpec allowNegative

instance GenSymSimple ImmSpec (Imm 'S) where
  simpleFresh (ConstImmSpec v) = return $ ConstImm v
  simpleFresh (TabularImmSpec l d) = return $ TabularImm l d
  simpleFresh (ReplicatingImmSpec bs) = do
    v <- simpleFresh bs
    return $ ReplicatingImm v
  simpleFresh ArbitraryImmSpec = simpleFresh ()
  simpleFresh (BoundedImmSpec signed lower upper) = do
    v <- simpleFresh (SimpleListSpec 7 ())
    return $ BoundedImm signed lower upper v
  simpleFresh (NumBitsBoundedImmSpec allowNegative) = do
    v <- simpleFresh (SimpleListSpec 7 ())
    return $ NumBitsBoundedImm allowNegative v

instance GenSym ImmSpec (Imm 'S)

instance GenSym [ImmSpec] (Imm 'S) where
  fresh [] = simpleFresh ArbitraryImmSpec
  fresh lst = do
    imm <- traverse simpleFresh lst
    chooseFresh imm

instance PPrint ImmSpec where
  pformat (ConstImmSpec v) = "const(" <> pformat v <> ")"
  pformat (TabularImmSpec l d) =
    "tabular"
      <> parenCommaList
        ( map (\(a, b) -> pformat a <> ": " <> pformat b) l
            ++ ["other: " <> pformat d]
        )
  pformat (ReplicatingImmSpec v) = "rep(" <> pformat v <> ")"
  pformat ArbitraryImmSpec = "arbitrary"
  pformat (BoundedImmSpec signed lower upper) =
    "bounded("
      <> (if signed then "signed" else "unsigned")
      <> ","
      <> pformat lower
      <> ","
      <> pformat upper
      <> ")"
  pformat (NumBitsBoundedImmSpec allowNegative) =
    "num_bits_bounded("
      <> (if allowNegative then "allow_negative" else "no_negative")
      <> ")"

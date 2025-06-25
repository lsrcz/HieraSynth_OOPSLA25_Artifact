{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Semantics.Imm
  ( Imm (..),
    getImm,
  )
where

import Data.Bits (FiniteBits (finiteBitSize))
import Data.Foldable (Foldable (foldl'))
import Grisette
  ( BV (bvConcat, bvSelect),
    GenSym,
    GenSymSimple (simpleFresh),
    LogicalOp ((.&&)),
    PPrint (pformat),
    SignConversion (toSigned),
    SimpleListSpec (SimpleListSpec),
    SomeBV (SomeBV),
    ToSym (toSym),
    WordN,
    mrgReturn,
  )
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import HieraSynth.Util.Pretty (encloseList)
import Grisette.Unified
  ( EvalModeTag (C, S),
    GetSomeWordN,
    GetWordN,
    UnifiedSymEq,
    (.<=),
  )
import RVV.Semantics.BitSizeUtil (log2)
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Util.Context (assert)
import RVV.Util.Derive (deriveFullExcept, firstModeDeriveConfig)

getImmFromList :: (SemConstraint mode m) => Int -> [GetWordN mode 64] -> m (GetSomeWordN mode)
getImmFromList i l
  | i `elem` [1, 2, 4, 8, 16, 32, 64] && length l > log2 i =
      mrgReturn $ SomeBV (l !! log2 i)
getImmFromList _ _ = error "Invalid imm size"

data Imm mode
  = ConstImm (WordN 64)
  | ArbitraryImm [GetWordN mode 64]
  | TabularImm [(Int, WordN 64)] (WordN 64)
  | BoundedImm
      { _signed :: Bool,
        _lowerBoundInclusive :: WordN 64,
        _upperBoundInclusive :: WordN 64,
        _value :: [GetWordN mode 64]
      }
  | NumBitsBoundedImm
      { _allowNegative :: Bool,
        _value :: [GetWordN mode 64]
      }
  | ReplicatingImm (GetSomeWordN mode)

deriveFullExcept firstModeDeriveConfig [''Imm] [''UnifiedSymEq]

getImm ::
  forall mode m.
  (SemConstraint mode m) =>
  Int ->
  Imm mode ->
  m (GetSomeWordN mode)
getImm v _ | v <= 0 || v > 64 = mrgThrowError "Invalid imm size"
getImm n (ConstImm v) =
  mrgReturn $ bvSelect 0 n (SomeBV (fromIntegral v :: GetWordN mode 64))
getImm v (ArbitraryImm f) = do
  x <- getImmFromList v f
  mrgReturn $ bvSelect 0 v x
getImm v (TabularImm l d) = do
  mrgReturn $
    toSym $
      SomeBV $
        foldl' (\acc (a, b) -> if a == v then b else acc) d l
getImm v (BoundedImm signed l u f) = do
  x <- getImmFromList v f
  let somel = toSym $ SomeBV l :: GetSomeWordN mode
      someu = toSym $ SomeBV u :: GetSomeWordN mode
  assert @m @mode "Imm value out of bounds" $
    if signed
      then toSigned somel .<= toSigned x .&& toSigned x .<= toSigned someu
      else somel .<= x .&& x .<= someu
  mrgReturn $ bvSelect 0 v x
getImm v (NumBitsBoundedImm allowNegative f) = do
  let upperBound = fromIntegral v :: WordN 64
      lowerBound = if allowNegative then -upperBound else 0
  getImm v (BoundedImm allowNegative lowerBound upperBound f)
getImm bs (ReplicatingImm v)
  | bs <= 0 = mrgThrowError "Invalid imm size"
  | finiteBitSize v > bs = mrgReturn $ bvSelect 0 bs v
  | finiteBitSize v == bs = mrgReturn v
  | bs `mod` finiteBitSize v == 0 =
      mrgReturn $ foldl1 bvConcat $ replicate (bs `div` finiteBitSize v) v
  | otherwise = mrgThrowError "Invalid imm size"

instance PPrint (Imm 'C) where
  pformat (ConstImm v) = pformat v
  pformat (ArbitraryImm l) = pformat l
  pformat (TabularImm l d) =
    encloseList
      "<"
      ">"
      ","
      ( fmap
          (\(bw, i) -> pformat bw <> ":" <> pformat (bvSelect 0 bw $ SomeBV i))
          l
          ++ ["other:" <> pformat d]
      )
  pformat (BoundedImm s l u lst) =
    encloseList
      "<"
      ">"
      ","
      [ pformat lst,
        "bound is signed: " <> pformat s,
        "lower: " <> pformat l,
        "upper: " <> pformat u
      ]
  pformat (NumBitsBoundedImm allowNegative lst) =
    encloseList
      "<"
      ">"
      ","
      [ pformat lst,
        "allow negative: " <> pformat allowNegative
      ]
  pformat (ReplicatingImm v) = "rep(" <> pformat v <> ")"

instance GenSym () (Imm 'S)

instance GenSymSimple () (Imm 'S) where
  simpleFresh _ = ArbitraryImm <$> simpleFresh (SimpleListSpec 7 ())

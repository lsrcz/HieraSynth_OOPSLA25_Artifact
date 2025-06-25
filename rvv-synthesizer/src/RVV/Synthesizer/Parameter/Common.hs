{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.Parameter.Common
  ( combineArithInvalid,
    combineAnyInvalid,
    anyInvalid,
    shiftBits,
    opCodeParser,
    oneOfOpCodeParser,
    checkUnsupportedOpCode,
    bvSextDouble,
    bvZextDouble,
    elemSextDouble,
    elemZextDouble,
    bvTruncHalf,
    elemTruncHalf,
  )
where

import Control.Applicative (asum)
import Data.Bits (Bits (shiftL, (.&.)), FiniteBits (finiteBitSize))
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as T
import Grisette (BV (bv, bvSelect, bvSext, bvZext), LogicalOp ((.&&), (.||)), Mergeable, SizedBV, SomeBV, false)
import HieraSynth.Util.Parser (CharParser)
import HieraSynth.Util.Show (showAsText)
import Grisette.Unified (GetData, GetSomeWordN, extractData, symBitBlast, symFromBits, symIte, (.==))
import RVV.EvalMode (EvalMode)
import RVV.Semantics.BitSizeUtil (log2)
import RVV.Semantics.Element (VectorElement (VectorElement))
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Util.Context (assert)
import Text.Megaparsec (Stream (chunkToTokens, tokensToChunk))
import Text.Megaparsec.Char (string)

combineArithInvalid ::
  forall mode.
  (EvalMode mode) =>
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  GetSomeWordN mode
combineArithInvalid l r =
  let lbits = symBitBlast @mode l
      rbits = symBitBlast @mode r
   in symFromBits @mode $
        drop 1 $
          scanl (\a (b1, b2) -> a .|| b1 .|| b2) false $
            zip lbits rbits

combineAnyInvalid ::
  forall mode.
  (EvalMode mode) =>
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  GetSomeWordN mode
combineAnyInvalid l r =
  symIte @mode
    ((l .== 0) .&& (r .== 0))
    (bv (finiteBitSize l) 0)
    (bv (finiteBitSize l) $ -1)

anyInvalid :: forall mode. (EvalMode mode) => GetSomeWordN mode -> GetSomeWordN mode
anyInvalid v =
  symIte @mode (v .== 0) (bv (finiteBitSize v) 0) (bv (finiteBitSize v) $ -1)

shiftBits ::
  (SizedBV bv, FiniteBits (SomeBV bv), Num (SomeBV bv), Show (SomeBV bv)) =>
  SomeBV bv ->
  SomeBV bv
shiftBits n = n .&. mask
  where
    bitLog2 = log2 $ finiteBitSize n
    one = bv (finiteBitSize n) 1
    mask = (one `shiftL` bitLog2) - one

{-# INLINE opCodeParser #-}
opCodeParser ::
  forall e s m op.
  (CharParser e s m) =>
  [(s, op)] ->
  s ->
  s ->
  m op
opCodeParser names prefix postfix =
  asum $
    (\(k, v) -> string (augment k) >> return v)
      <$> names
  where
    toTokens = chunkToTokens (Proxy @s)
    augment k =
      tokensToChunk (Proxy @s) $
        toTokens prefix ++ toTokens k ++ toTokens postfix

oneOfOpCodeParser ::
  (CharParser e s m) =>
  [(m op, a)] ->
  m (op, a)
oneOfOpCodeParser l = asum $ fmap (\(op, a) -> (,a) <$> op) l

checkUnsupportedOpCode ::
  (SemConstraint mode ctx, Eq opcode, Mergeable opcode, Show opcode) =>
  T.Text ->
  GetData mode opcode ->
  [opcode] ->
  ctx ()
checkUnsupportedOpCode name op ops = do
  op <- extractData op
  assert
    ("Unsupported operation for " <> name <> ": " <> showAsText op)
    (op `elem` ops)

bvSextDouble :: (SizedBV bv, FiniteBits (SomeBV bv)) => SomeBV bv -> SomeBV bv
bvSextDouble v = bvSext (2 * finiteBitSize v) v

bvZextDouble :: (SizedBV bv, FiniteBits (SomeBV bv)) => SomeBV bv -> SomeBV bv
bvZextDouble v = bvZext (2 * finiteBitSize v) v

elemSextDouble :: (EvalMode mode) => VectorElement mode -> VectorElement mode
elemSextDouble (VectorElement v vi) =
  VectorElement (bvSextDouble v) (bvSextDouble vi)

elemZextDouble :: (EvalMode mode) => VectorElement mode -> VectorElement mode
elemZextDouble (VectorElement v vi) =
  VectorElement (bvZextDouble v) (bvZextDouble vi)

bvTruncHalf :: (SizedBV bv, FiniteBits (SomeBV bv)) => SomeBV bv -> SomeBV bv
bvTruncHalf v = bvSelect 0 (finiteBitSize v `div` 2) v

elemTruncHalf :: (EvalMode mode) => VectorElement mode -> VectorElement mode
elemTruncHalf (VectorElement v vi) = VectorElement (bvTruncHalf v) (bvTruncHalf vi)

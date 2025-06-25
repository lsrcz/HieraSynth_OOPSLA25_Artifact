{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.Matcher
  ( VMatchPair (..),
    maskToVMatchPair,
    vectorToVMatchPair,
    RegMatcher (..),
    RegListMatcher (..),
  )
where

import Control.DeepSeq (NFData)
import Data.Bits (Bits (xor, (.&.)), FiniteBits (finiteBitSize))
import Data.Bytes.Serial (Serial)
import qualified Data.HashMap.Lazy as M
import GHC.Generics (Generic)
import Grisette
  ( BV (bv, bvConcat, bvSelect),
    Default (Default),
    EvalSym (evalSym),
    LogicalOp ((.&&), (.||)),
    Mergeable (rootStrategy),
    MergingStrategy (NoStrategy),
    PPrint,
    SomeWordN,
    SymBool,
    ToSym (toSym),
  )
import HieraSynth.Reasoning.Matcher (Matcher (match))
import Grisette.Unified
  ( EvalModeTag (C, S),
    GetBool,
    GetSomeWordN,
    (.==),
  )
import RVV.EvalMode (EvalMode)
import RVV.Semantics.MachineConfig
  ( MachineConfig (MachineConfig, scalableConfig),
    MachineScalableConfig (machineVectorLength),
  )
import RVV.Semantics.Memory (Memory (Memory), MemoryBlock (MemoryBlock))
import RVV.Semantics.Value
  ( Mask (Mask, maskMultiplier),
    Vector (Vector, vectorConfig),
    VectorReg (VectorReg),
  )
import RVV.Semantics.VectorConfig
  ( VectorConfig (lengthMul),
    maskNumValidElements,
    vectorNumValidBits,
  )
import RVV.Synthesizer.Value (Value (MaskValue, MemValue, VectorValue))

data RegMatcher
  = MatchMasked SomeWordN
  | MatchFull
  | MatchIgnored
  | MatchMem (M.HashMap Int RegMatcher)
  deriving (Eq, Generic)
  deriving anyclass (NFData, Serial)
  deriving (PPrint) via (Default RegMatcher)

data RegListMatcher = RegListMatcher MachineConfig [RegMatcher]
  deriving (Eq, Generic)
  deriving anyclass (NFData, Serial)
  deriving (PPrint) via (Default RegListMatcher)

instance Mergeable RegListMatcher where
  rootStrategy = NoStrategy

instance EvalSym RegListMatcher where
  evalSym _ _ = id

matchBV ::
  (EvalMode mode) =>
  SomeWordN ->
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  GetBool mode
matchBV maskBV lhs rhs
  | finiteBitSize maskBV /= finiteBitSize lhs =
      error "Mask bitwidth does not match lhs bit width"
  | finiteBitSize maskBV /= finiteBitSize rhs =
      error "Mask bitwidth does not match rhs bit width"
  | otherwise =
      (toSym maskBV .&. (lhs `xor` rhs))
        .== bv (finiteBitSize maskBV) 0

data VMatchPair mode = VMatchPair
  { matchData :: GetSomeWordN mode,
    matchUninitialized :: GetSomeWordN mode
  }

deriving instance (EvalMode mode) => Show (VMatchPair mode)

deriving instance (EvalMode mode) => Eq (VMatchPair mode)

maskToVMatchPair :: Mask mode -> VMatchPair mode
maskToVMatchPair (Mask _ (VectorReg d i)) = VMatchPair d i

memoryBlockToVMatchPair :: MemoryBlock mode -> VMatchPair mode
memoryBlockToVMatchPair (MemoryBlock d i) = VMatchPair d i

vectorToVMatchPair :: (EvalMode mode) => Vector mode -> VMatchPair mode
vectorToVMatchPair (Vector _ regs) =
  VMatchPair
    { matchData = foldl1 (flip bvConcat) $ map (\(VectorReg d _) -> d) regs,
      matchUninitialized =
        foldl1 (flip bvConcat) $ map (\(VectorReg _ i) -> i) regs
    }

matchVMatchPair ::
  (EvalMode mode) =>
  MachineConfig ->
  SomeWordN ->
  VMatchPair mode ->
  VMatchPair mode ->
  GetBool mode
matchVMatchPair
  MachineConfig {..}
  mask
  (VMatchPair actualData actualUninitialized)
  (VMatchPair expectedData expectedUninitialized) =
    matchBV (toSym mask) actualData expectedData
      .&& matchBV (toSym mask) actualUninitialized expectedUninitialized

normalizeVectorReg :: (EvalMode mode) => Int -> VectorReg mode -> VectorReg mode
normalizeVectorReg bits (VectorReg dt invalid) =
  let vlen = finiteBitSize dt
   in if vlen > bits
        then
          VectorReg
            (bvConcat (bv (vlen - bits) 0) (bvSelect 0 bits dt))
            (bvConcat (bv (vlen - bits) (-1)) (bvSelect 0 bits invalid))
        else VectorReg dt invalid

normalizeInvalidParts ::
  (EvalMode mode) =>
  MachineConfig ->
  Value mode ->
  Value mode
normalizeInvalidParts vconst (VectorValue (Vector vtype [vectorReg]))
  | lengthMul vtype < 1 =
      let bits = vectorNumValidBits vconst vtype
       in VectorValue (Vector vtype [normalizeVectorReg bits vectorReg])
normalizeInvalidParts vconst v@(MaskValue (Mask maskMul (VectorReg dt invalid))) =
  let bits = maskNumValidElements vconst maskMul
      vlen = machineVectorLength $ scalableConfig vconst
   in if bits == vlen
        then v
        else MaskValue $ Mask maskMul $ normalizeVectorReg bits (VectorReg dt invalid)
normalizeInvalidParts _ v = v

matchValue ::
  forall mode.
  (EvalMode mode) =>
  MachineConfig ->
  RegMatcher ->
  Value mode ->
  Value mode ->
  GetBool mode
matchValue vconst matcher actual expected =
  matchValue'
    vconst
    matcher
    (normalizeInvalidParts vconst actual)
    (normalizeInvalidParts vconst expected)

matchValue' ::
  forall mode.
  (EvalMode mode) =>
  MachineConfig ->
  RegMatcher ->
  Value mode ->
  Value mode ->
  GetBool mode
matchValue'
  vconst
  (MatchMasked mask)
  (VectorValue actual)
  (VectorValue expected) =
    toSym (vectorConfig actual == vectorConfig expected)
      .&& matchVMatchPair
        vconst
        mask
        (vectorToVMatchPair actual)
        (vectorToVMatchPair expected)
matchValue'
  vconst
  (MatchMasked mask)
  (MaskValue actual)
  (MaskValue expected) =
    toSym (maskMultiplier actual == maskMultiplier expected)
      .&& matchVMatchPair
        vconst
        mask
        (maskToVMatchPair actual)
        (maskToVMatchPair expected)
matchValue' _ (MatchMasked {}) _ _ = toSym False
matchValue'
  vconst
  (MatchMem mem)
  (MemValue (Memory actual))
  (MemValue (Memory expected)) =
    toSym sameIdx
      .&& M.foldlWithKey'
        ( \acc k matcher ->
            acc .&& matchMemBlock matcher (actual M.! k) (expected M.! k)
        )
        (toSym True)
        mem
    where
      sameIdx = M.keysSet actual == M.keysSet expected && M.keysSet actual == M.keysSet mem
      matchMemBlock ::
        RegMatcher -> MemoryBlock mode -> MemoryBlock mode -> GetBool mode
      matchMemBlock MatchFull (MemoryBlock ld li) (MemoryBlock rd ri) =
        ld .== rd .&& li .== ri
      matchMemBlock MatchIgnored _ _ = toSym True
      matchMemBlock (MatchMasked mask) l r =
        matchVMatchPair
          vconst
          mask
          (memoryBlockToVMatchPair l)
          (memoryBlockToVMatchPair r)
      matchMemBlock _ _ _ = toSym False
matchValue' _ (MatchMem {}) _ _ = toSym False
matchValue' _ MatchFull actual expected = actual .== expected
matchValue' _ MatchIgnored _ _ = toSym True

matchList ::
  forall mode.
  (EvalMode mode) =>
  RegListMatcher ->
  [Value mode] ->
  [Value mode] ->
  GetBool mode
matchList (RegListMatcher vconst regMatchers) actual expected =
  (length regMatchers .== length expected)
    .&& (length actual .== length expected)
    .&& ( toSym (null regMatchers)
            .|| foldl1
              (.&&)
              (zipWith3 (matchValue vconst) regMatchers actual expected)
        )

instance Matcher RegListMatcher Bool (Value 'C) where
  match = matchList

instance Matcher RegListMatcher SymBool (Value 'S) where
  match = matchList

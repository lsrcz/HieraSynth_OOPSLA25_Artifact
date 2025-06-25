{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module RVV.Semantics.Multiplier
  ( WidthMul (..),
    MaskMul (..),
    LengthMul (..),
  )
where

import Control.DeepSeq (NFData)
import Data.Bytes.Serial (Serial)
import Data.Hashable (Hashable)
import Data.Ratio (Ratio)
import Grisette (EvalSym, Mergeable, PPrint (pformat), SymEq, deriveWith)
import RVV.Synthesizer.Specification.ScaleLMul (ScaleLMul (scaleLMul))
import RVV.Util.Derive (noModeDeriveConfig)
import RVV.Util.Pretty (prettyRatio, showRatio)

newtype WidthMul
  = WidthMul {getWidthMul :: Ratio Int}
  deriving newtype
    ( Num,
      Eq,
      Ord,
      NFData,
      Hashable,
      EvalSym,
      Mergeable,
      SymEq,
      Fractional
    )

instance Show WidthMul where
  show (WidthMul r) = showRatio r

instance PPrint WidthMul where
  pformat (WidthMul r) = prettyRatio r

-- | Number of bits in the mask = mmul * vlen / xlen
-- i.e., the base on a real machine is the number of bits in the mask for
-- vuint64_t (smallest supported mask type).
--
-- For more bits in the mask, we will have a mmul > 1.
--
-- vbool4_t corresponds to mmul=16
newtype MaskMul
  = MaskMul {getMaskMul :: Ratio Int}
  deriving newtype
    ( Num,
      Eq,
      Ord,
      NFData,
      Hashable,
      EvalSym,
      Mergeable,
      SymEq,
      Fractional
    )

instance Show MaskMul where
  show (MaskMul r) = showRatio r

instance PPrint MaskMul where
  pformat (MaskMul r) = prettyRatio r

newtype LengthMul
  = LengthMul {getLengthMul :: Ratio Int}
  deriving newtype
    ( Num,
      Eq,
      Ord,
      NFData,
      Hashable,
      EvalSym,
      Mergeable,
      SymEq,
      Fractional
    )

instance Show LengthMul where
  show (LengthMul r) = showRatio r

instance PPrint LengthMul where
  pformat (LengthMul r) = prettyRatio r

deriveWith
  noModeDeriveConfig
  [''WidthMul, ''MaskMul, ''LengthMul]
  [''Serial]

instance ScaleLMul MaskMul where
  scaleLMul ratio mmul = mmul * MaskMul ratio

instance ScaleLMul LengthMul where
  scaleLMul ratio vlmul = vlmul * LengthMul ratio

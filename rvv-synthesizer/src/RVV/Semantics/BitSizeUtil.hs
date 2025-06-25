module RVV.Semantics.BitSizeUtil
  ( isPower2,
    log2,
    power2,
  )
where

import Data.Bits
  ( Bits (shiftL, (.&.)),
    FiniteBits (countLeadingZeros, finiteBitSize),
  )

isPower2 :: Int -> Bool
isPower2 x = x /= 0 && x .&. (x - 1) == 0

log2 :: Int -> Int
log2 n = finiteBitSize n - 1 - countLeadingZeros n

power2 :: Int -> Int
power2 n = 1 `shiftL` n

{-# LANGUAGE OverloadedStrings #-}

module RVV.Util.Pretty (showRatio, prettyRatio) where

import Data.Ratio (Ratio, denominator, numerator)
import Grisette (Doc, PPrint (pformat))

showRatio :: Ratio Int -> String
showRatio r =
  let n = numerator r
      d = denominator r
   in if d == 1
        then show n
        else if n == 1 then "f" ++ show d else show r

prettyRatio :: Ratio Int -> Doc ann
prettyRatio = pformat . showRatio

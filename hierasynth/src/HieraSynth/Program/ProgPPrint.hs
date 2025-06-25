{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module HieraSynth.Program.ProgPPrint
  ( ProgPPrint (..),
  )
where

import qualified Data.Text as T
import HieraSynth.Util.Pretty (Doc)

class ProgPPrint prog where
  pformatProg :: T.Text -> prog -> Either (Doc ann) (Doc ann)

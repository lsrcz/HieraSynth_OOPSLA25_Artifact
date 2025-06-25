{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module HieraSynth.Operator.OpParser (OpParser (..)) where

import HieraSynth.Util.Parser (CharParser)

class OpParser op where
  opParser :: (CharParser e s m) => m op

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module HieraSynth.Type.TypeParser (TypeParser (..)) where

import HieraSynth.Util.Parser (CharParser)

class TypeParser op where
  typeParser :: (CharParser e s m) => m op

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module RVV.App.FileFormat (FileFormat (..), fileFormatParser) where

import Control.Applicative (Alternative ((<|>)))
import HieraSynth.Util.Parser (CharParser, symbol)

data FileFormat = LLVM | SIR

instance Show FileFormat where
  show LLVM = "llvm"
  show SIR = "sir"

fileFormatParser :: (CharParser e s m) => m FileFormat
fileFormatParser =
  (symbol "llvm" >> return LLVM)
    <|> (symbol "sir" >> return SIR)

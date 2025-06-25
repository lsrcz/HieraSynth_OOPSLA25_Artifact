{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module HieraSynth.Program.ProgParser
  ( ProgParser (..),
    progTableParser,
  )
where

import Control.Applicative (Alternative (many))
import qualified Data.Text as T
import HieraSynth.Program.SymbolTable (SymbolTable (SymbolTable))
import HieraSynth.Util.Parser (CharParser)

class ProgParser prog where
  progParser :: (CharParser e s m) => m (T.Text, prog)

progTableParser :: (CharParser e s m, ProgParser prog) => m (SymbolTable prog)
progTableParser = do
  progList <- many progParser
  return $ SymbolTable progList

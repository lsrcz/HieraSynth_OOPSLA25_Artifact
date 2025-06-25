{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module RVV.App.IRSpecScalingMethod
  ( IRSpecScalingMethod (..),
    irSpecScalingMethodParser,
  )
where

import Control.Applicative ((<|>))
import Grisette (PPrint (pformat), viaShow)
import HieraSynth.Util.Parser (CharParser, symbol)

data IRSpecScalingMethod = NoScale | ZextScale | SextScale

instance Show IRSpecScalingMethod where
  show NoScale = "no"
  show ZextScale = "zext"
  show SextScale = "sext"

instance PPrint IRSpecScalingMethod where
  pformat = viaShow

irSpecScalingMethodParser :: (CharParser e s m) => m IRSpecScalingMethod
irSpecScalingMethodParser =
  (symbol "no" >> return NoScale)
    <|> (symbol "zext" >> return ZextScale)
    <|> (symbol "sext" >> return SextScale)

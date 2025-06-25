{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RVV.App.MachineConfigSpec
  ( MachineConfigSpec (..),
    toMachineConfig,
    vconstSpecListParser,
  )
where

import Control.Applicative ((<|>))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Ratio (Ratio, denominator, numerator, (%))
import Grisette (PPrint (pformat), viaShow)
import HieraSynth.Util.Parser (CharParser, symbol)
import RVV.Parser.ArgParser (ratioParser)
import RVV.Semantics.BitSizeUtil (isPower2)
import RVV.Semantics.MachineConfig
  ( AllowPartialVL,
    MachineBaseConfig
      ( MachineBaseConfig,
        machineMemoryBlockIds,
        machinePointerUnit,
        machineScalarImmLength,
        machineScalarLength,
        machineVectorImmLength
      ),
    MachineConfig (MachineConfig),
    MachineScalableConfig (MachineScalableConfig),
  )
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Char.Lexer (decimal)

data MachineConfigSpec = MachineConfigSpec {vconstFraction :: Ratio Int, machineVectorLength :: Int}

toMachineConfig :: Bool -> AllowPartialVL -> MachineConfigSpec -> MachineConfig
toMachineConfig useVLMask allowPartialVL MachineConfigSpec {..}
  | vconstFraction > 1 =
      error "Fraction must be less than or equal to 1"
  | not (numerator vconstFraction == 1 && isPower2 (denominator vconstFraction)) =
      error "Fraction must be a power of 2"
  | otherwise =
      MachineConfig
        ( MachineBaseConfig
            { machineScalarLength = 64 `div` denominator vconstFraction,
              machineMemoryBlockIds = HS.empty,
              machinePointerUnit = 8 `div` ptrDividend vconstFraction,
              machineScalarImmLength = 64 `div` denominator vconstFraction,
              machineVectorImmLength = 64 `div` denominator vconstFraction
            }
        )
        (MachineScalableConfig machineVectorLength HM.empty)
        useVLMask
        allowPartialVL
  where
    ptrDividend v = denominator (max v (1 % 8))

instance Show MachineConfigSpec where
  show MachineConfigSpec {..} =
    "vlen" <> show machineVectorLength <> "f" <> show vconstFraction

instance PPrint MachineConfigSpec where
  pformat = viaShow

vconstSpecParser :: (CharParser e s m) => m MachineConfigSpec
vconstSpecParser = do
  string "vlen"
  vlen <- decimal
  fraction <- ratioParser <|> return 1
  return $ MachineConfigSpec fraction vlen

vconstSpecListParser :: (CharParser e s m) => m [[MachineConfigSpec]]
vconstSpecListParser = (vconstSpecParser `sepBy` symbol ",") `sepBy` symbol ";"

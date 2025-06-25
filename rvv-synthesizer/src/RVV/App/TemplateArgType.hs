{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module RVV.App.TemplateArgType
  ( TemplateArgType (..),
    toClangTemplateArgTxt,
    toClangTemplateArgListTxt,
    templateArgTypeListParser,
    toValueType,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Data.Ratio ((%))
import qualified Data.Text as T
import Grisette (PPrint (pformat), viaShow)
import HieraSynth.Util.Parser (CharParser, symbol)
import HieraSynth.Util.Show (showAsText)
import RVV.Parser.ArgParser (lengthMulParser)
import RVV.Semantics.Multiplier (LengthMul, MaskMul (MaskMul), WidthMul (WidthMul))
import RVV.Semantics.VectorConfig (VectorConfig (VectorConfig))
import RVV.Synthesizer.Type (ValueType (MaskType, ScalarType, VectorType))
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Char.Lexer (decimal)

data TemplateArgType
  = VectorTemplateType
      { _signed :: Bool,
        _bitWidth :: Int,
        _LMul :: LengthMul
      }
  | ScalarTemplateType {_signed :: Bool, _bitWidth :: Int}
  | MaskTemplateType {_ratio :: Int}

instance Show TemplateArgType where
  show (VectorTemplateType signed bitWidth lmul) =
    (if signed then "vint" else "vuint")
      <> show bitWidth
      <> "m"
      <> show lmul
      <> "_t"
  show (ScalarTemplateType signed bitWidth) =
    (if signed then "int" else "uint") <> show bitWidth <> "_t"
  show (MaskTemplateType ratio) = "vbool" <> show ratio <> "_t"

instance PPrint TemplateArgType where
  pformat = viaShow

toClangTemplateArgTxt :: TemplateArgType -> T.Text
toClangTemplateArgTxt (VectorTemplateType signed bitWidth lmul) =
  "__rvv_"
    <> (if signed then "int" else "uint")
    <> showAsText bitWidth
    <> "m"
    <> T.pack (show lmul)
    <> "_t"
toClangTemplateArgTxt (ScalarTemplateType signed bitWidth) =
  (if signed then "" else "unsigned ")
    <> case bitWidth of
      8 -> "char"
      16 -> "short"
      32 -> "int"
      64 -> "long"
      _ -> error "Invalid bit width"
toClangTemplateArgTxt (MaskTemplateType ratio) =
  "__rvv_bool" <> showAsText ratio <> "_t"

toClangTemplateArgListTxt :: Maybe [TemplateArgType] -> T.Text
toClangTemplateArgListTxt (Just lst) =
  "<" <> (T.intercalate ", " . fmap toClangTemplateArgTxt $ lst) <> ">"
toClangTemplateArgListTxt Nothing = ""

signParser :: (CharParser e s m) => m Bool
signParser = (string "uint" >> return False) <|> (string "int" >> return True)

vectorTemplateArgTypeParser :: (CharParser e s m) => m TemplateArgType
vectorTemplateArgTypeParser = do
  string "v"
  sign <- signParser
  bw <- decimal
  string "m"
  ratio <- lengthMulParser
  VectorTemplateType sign bw ratio <$ string "_t"

scalarTemplateArgTypeParser :: (CharParser e s m) => m TemplateArgType
scalarTemplateArgTypeParser = do
  sign <- signParser
  bw <- decimal
  ScalarTemplateType sign bw <$ string "_t"

maskTemplateArgTypeParser :: (CharParser e s m) => m TemplateArgType
maskTemplateArgTypeParser = do
  symbol "vbool"
  ratio <- decimal
  MaskTemplateType ratio <$ string "_t"

-- TODO: add ScalarTemplateType and MaskTemplateType parsers
templateArgTypeParser :: (CharParser e s m) => m TemplateArgType
templateArgTypeParser =
  maskTemplateArgTypeParser
    <|> vectorTemplateArgTypeParser
    <|> scalarTemplateArgTypeParser

templateArgTypeListParser :: (CharParser e s m) => m [TemplateArgType]
templateArgTypeListParser = templateArgTypeParser `sepBy` symbol ","

toValueType :: TemplateArgType -> ValueType
toValueType (VectorTemplateType _ bitWidth logLMul) =
  VectorType $ VectorConfig (WidthMul $ bitWidth % 64) logLMul
toValueType (ScalarTemplateType _ bitWidth) =
  ScalarType (WidthMul $ bitWidth % 64)
toValueType (MaskTemplateType ratio) =
  MaskType (MaskMul $ 64 % ratio)

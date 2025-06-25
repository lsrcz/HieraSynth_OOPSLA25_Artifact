{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module RVV.Synthesizer.Specification.ScaleConstants
  ( ImmSelector (..),
    NewImm (..),
    scaleConTable,
    scaledFullMatchSpecFromRefProg,
    selectorAndNewImmMapParsers,
  )
where

import qualified Data.HashMap.Strict as HM
import Data.Ratio (Ratio, (%))
import qualified Data.Text as T
import Grisette (WordN)
import HieraSynth.Combinator.Embed (type (:<:) (inj, prj))
import qualified HieraSynth.Program.Concrete as Concrete
import HieraSynth.Program.SymbolTable (SymbolTable (SymbolTable))
import HieraSynth.Util.Parser (CharParser)
import Grisette.Unified (EvalModeTag (C))
import Options.Applicative (Alternative (some))
import RVV.Semantics.Imm (Imm)
import RVV.Semantics.MachineConfig (MachineBaseConfig, MachineConfig (baseConfig))
import RVV.Semantics.PrimOp.Util (SemConstraint)
import RVV.Synthesizer.DefaultSynthType (DefaultConProg)
import RVV.Synthesizer.Matcher (RegListMatcher)
import RVV.Synthesizer.Operator.Scalar (Scalar (ScalarLongImm))
import RVV.Synthesizer.Specification.RefProg (fullMatchSpecFromRefProg)
import RVV.Synthesizer.Specification.Scale (baseConfigScaleRatio)
import RVV.Synthesizer.Value (Value)
import Text.Megaparsec (sepBy, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, string)
import qualified Text.Megaparsec.Char.Lexer as L

atPosParser :: (CharParser e s m) => m ImmSelector
atPosParser = char '@' >> fmap AtPos L.decimal

eqPosParser :: (CharParser e s m) => m ImmSelector
eqPosParser = char '~' >> fmap EqValue L.decimal

immSelectorParser :: (CharParser e s m) => m ImmSelector
immSelectorParser = atPosParser <|> eqPosParser

data ImmSelector = AtPos Int | EqValue (WordN 64) deriving (Show)

immMapParser :: (CharParser e s m) => m (Int, WordN 64)
immMapParser = do
  fraction <- L.decimal
  string ":"
  value <- L.decimal
  pure (fraction, value)

newImmParser :: (CharParser e s m) => m NewImm
newImmParser = NewImm <$> immMapParser `sepBy` char ','

selectorAndNewImmParser :: (CharParser e s m) => m (ImmSelector, NewImm)
selectorAndNewImmParser = do
  newImm <- newImmParser
  selector <- immSelectorParser
  pure (selector, newImm)

-- | "s:[1:2@2|3:4~3]|g:[1:2,3:4@2]" means
--
-- [ ( "s",
--     [ (AtPos 2, NewImm [(1, 0x0000000000000002)]),
--       (EqValue 0x0000000000000003, NewImm [(3, 0x0000000000000004)])
--     ]
--   ),
--   ("g", [(AtPos 2, NewImm [(1, 0x0000000000000002), (3, 0x0000000000000004)])])
--  ]
selectorAndNewImmsParser :: (CharParser e s m) => m [(ImmSelector, NewImm)]
selectorAndNewImmsParser = do
  char '['
  ret <- selectorAndNewImmParser `sepBy` char '|'
  char ']'
  return ret

nameToSelectorAndNewImmsParser :: (CharParser e s m) => m (T.Text, [(ImmSelector, NewImm)])
nameToSelectorAndNewImmsParser = do
  name <- some (char '_' <|> alphaNumChar)
  char ':'
  selectorsAndNewImm <- selectorAndNewImmsParser
  return (T.pack name, selectorsAndNewImm)

selectorAndNewImmMapParsers ::
  (CharParser e s m) =>
  m (HM.HashMap T.Text [(ImmSelector, NewImm)])
selectorAndNewImmMapParsers = do
  ret <- nameToSelectorAndNewImmsParser `sepBy` char '|'
  return $ HM.fromList ret

newtype NewImm = NewImm [(Int, WordN 64)] deriving (Show)

scaleImm :: Int -> NewImm -> Imm 'C -> Imm 'C
scaleImm 0 _ imm = imm
-- scaleImm fraction (NewImm l) (ArbitraryImm (TabularFun [] _)) =
--   go l
--   where
--     go [] = error "scaleImm: no matching vconst fraction"
--     go ((pos, v) : rest)
--       | pos == fraction = ArbitraryImm (TabularFun [] v)
--       | otherwise = go rest
scaleImm _ _ _ = error "scaleImm: not implemented" -- can only scale ArbitraryImm"

scaleDefaultConProgImm :: Int -> ImmSelector -> NewImm -> DefaultConProg -> DefaultConProg
scaleDefaultConProgImm fraction selector newImm (Concrete.Prog args stmts res) =
  Concrete.Prog args (go selector stmts) res
  where
    go AtPos {} [] =
      error "scaleDefaultConProgImm: no matching vconst fraction for AtPos"
    go EqValue {} [] = []
    go
      (AtPos n)
      ( stmt@( Concrete.Stmt
                 (prj -> Just (ScalarLongImm xmul oldImm))
                 stmtArgs
                 stmtRes
               )
          : sts
        ) =
        if n == 0
          then
            Concrete.Stmt
              ( inj (ScalarLongImm xmul (scaleImm fraction newImm oldImm))
              )
              stmtArgs
              stmtRes
              : sts
          else stmt : go (AtPos (n - 1)) sts
    go (AtPos n) (stmt : sts) = stmt : go (AtPos n) sts
    -- go
    --   (EqValue v)
    --   ( stmt@( Concrete.Stmt
    --              (prj -> Just (ScalarLong xmul oldImm))
    --              stmtArgs
    --              stmtRes
    --            )
    --       : sts
    --     ) = case oldImm of
    --     ArbitraryImm (TabularFun [] oldV)
    --       | oldV == v ->
    --           Concrete.Stmt
    --             (inj (ScalarLong xmul (scaleImm fraction newImm oldImm)))
    --             stmtArgs
    --             stmtRes
    --             : sts
    --     _ -> stmt : go (EqValue v) sts
    go (EqValue v) (stmt : sts) = stmt : go (EqValue v) sts

scaleConTable ::
  Int ->
  HM.HashMap T.Text [(ImmSelector, NewImm)] ->
  SymbolTable DefaultConProg ->
  SymbolTable DefaultConProg
scaleConTable fraction selectorsAndNewImm (SymbolTable table) =
  SymbolTable $ go table
  where
    go [] = []
    go ((name, prog) : rest) =
      case HM.lookup name selectorsAndNewImm of
        Just selectorsAndNewImm ->
          ( name,
            foldl
              ( \prog (selector, newImm) ->
                  scaleDefaultConProgImm fraction selector newImm prog
              )
              prog
              selectorsAndNewImm
          )
            : go rest
        Nothing -> (name, prog) : go rest

ratioToSelectorScalingSpec :: Ratio Int -> Int
ratioToSelectorScalingSpec ratio =
  if
    | ratio == 1 % 1 -> 0
    | ratio == 1 % 2 -> 1
    | ratio == 1 % 4 -> 2
    | ratio == 1 % 8 -> 3
    | ratio == 1 % 16 -> 4
    | ratio == 1 % 32 -> 5
    | otherwise ->
        error $ "ratioToSelectorScalingSpec: unsupported ratio: " <> show ratio

scaledFullMatchSpecFromRefProg ::
  (SemConstraint mode ctx) =>
  MachineBaseConfig ->
  HM.HashMap T.Text [(ImmSelector, NewImm)] ->
  SymbolTable DefaultConProg ->
  T.Text ->
  MachineConfig ->
  [Value mode] ->
  ctx ([Value mode], RegListMatcher)
scaledFullMatchSpecFromRefProg originalBase selectorsAndNewImm prog key vconst input =
  let Right ratio = baseConfigScaleRatio False (baseConfig vconst) originalBase
      scalingSpec = ratioToSelectorScalingSpec ratio
      scaledTable = scaleConTable scalingSpec selectorsAndNewImm prog
   in fullMatchSpecFromRefProg scaledTable key vconst input

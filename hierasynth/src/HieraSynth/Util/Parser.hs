{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module HieraSynth.Util.Parser
  ( CharParser,
    lexeme,
    symbol,
    identifier,
    comma,
    colon,
    leftBracket,
    rightBracket,
    leftParen,
    rightParen,
    leftBrace,
    rightBrace,
    leftAngle,
    rightAngle,
    betweenBrackets,
    betweenParens,
    betweenBraces,
    betweenAngles,
    commaSep,
    bracketCommaSep,
    bracketCommaSepOrSingleton,
    parenCommaSep,
    parenCommaSepOrSingleton,
    braceCommaSep,
    braceCommaSepOrSingleton,
    angleCommaSep,
    angleCommaSepOrSingleton,
    named,
    maybeNamed,
    commaSep2,
    commaSep3,
    commaSep4,
    commaSep5,
    bracketCommaSep2,
    bracketCommaSep3,
    bracketCommaSep4,
    bracketCommaSep5,
    parenCommaSep2,
    parenCommaSep3,
    parenCommaSep4,
    parenCommaSep5,
    braceCommaSep2,
    braceCommaSep3,
    braceCommaSep4,
    braceCommaSep5,
    angleCommaSep2,
    angleCommaSep3,
    angleCommaSep4,
    angleCommaSep5,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Applicative.Combinators (between)
import Data.String (IsString)
import qualified Data.Text as T
import Text.Megaparsec (MonadParsec (try), Stream (Token, Tokens), sepBy, some)
import Text.Megaparsec.Char (alphaNumChar, char, space1)
import qualified Text.Megaparsec.Char.Lexer as L

sc :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

type CharParser e s m =
  ( MonadParsec e s m,
    Token s ~ Char,
    IsString (Tokens s),
    Tokens s ~ s,
    MonadFail m
  )

lexeme :: (CharParser e s m) => m a -> m a
lexeme = L.lexeme sc

symbol :: (CharParser e s m) => s -> m s
symbol = L.symbol sc

identifier :: forall e s m. (CharParser e s m) => m T.Text
identifier =
  (lexeme . try) (T.pack <$> some identChar)
  where
    identChar = alphaNumChar <|> char '_' <|> char '.'

comma ::
  (CharParser e s m) => m s
comma = symbol ","

colon ::
  (CharParser e s m) => m s
colon = symbol ":"

leftBracket ::
  (CharParser e s m) => m s
leftBracket = symbol "["

rightBracket ::
  (CharParser e s m) => m s
rightBracket = symbol "]"

leftParen ::
  (CharParser e s m) => m s
leftParen = symbol "("

rightParen ::
  (CharParser e s m) => m s
rightParen = symbol ")"

leftBrace ::
  (CharParser e s m) => m s
leftBrace = symbol "{"

rightBrace ::
  (CharParser e s m) => m s
rightBrace = symbol "}"

leftAngle ::
  (CharParser e s m) => m s
leftAngle = symbol "<"

rightAngle ::
  (CharParser e s m) => m s
rightAngle = symbol ">"

betweenBrackets ::
  (CharParser e s m) =>
  m a -> m a
betweenBrackets = between leftBracket rightBracket

betweenParens ::
  (CharParser e s m) =>
  m a -> m a
betweenParens = between leftParen rightParen

betweenBraces ::
  (CharParser e s m) =>
  m a -> m a
betweenBraces = between leftBrace rightBrace

betweenAngles ::
  (CharParser e s m) =>
  m a -> m a
betweenAngles = between leftAngle rightAngle

commaSep ::
  (CharParser e s m) =>
  m a -> m [a]
commaSep p = p `sepBy` comma

bracketCommaSep ::
  (CharParser e s m) =>
  m a -> m [a]
bracketCommaSep = betweenBrackets . commaSep

bracketCommaSepOrSingleton ::
  (CharParser e s m) =>
  m a -> m [a]
bracketCommaSepOrSingleton p = ((: []) <$> p) <|> bracketCommaSep p

parenCommaSep ::
  (CharParser e s m) =>
  m a -> m [a]
parenCommaSep = betweenParens . commaSep

parenCommaSepOrSingleton ::
  (CharParser e s m) =>
  m a -> m [a]
parenCommaSepOrSingleton p = ((: []) <$> p) <|> parenCommaSep p

braceCommaSep ::
  (CharParser e s m) =>
  m a -> m [a]
braceCommaSep = betweenBraces . commaSep

braceCommaSepOrSingleton ::
  (CharParser e s m) =>
  m a -> m [a]
braceCommaSepOrSingleton p = ((: []) <$> p) <|> braceCommaSep p

angleCommaSep ::
  (CharParser e s m) =>
  m a -> m [a]
angleCommaSep = betweenAngles . commaSep

angleCommaSepOrSingleton ::
  (CharParser e s m) =>
  m a -> m [a]
angleCommaSepOrSingleton p = ((: []) <$> p) <|> angleCommaSep p

named ::
  (CharParser e s m) =>
  s -> m a -> m a
named name p = symbol name >> symbol "=" >> p

maybeNamed ::
  (CharParser e s m) =>
  Maybe s -> m a -> m a
maybeNamed (Just name) = named name
maybeNamed Nothing = id

commaSep2 ::
  (CharParser e s m) =>
  m a -> m b -> m (a, b)
commaSep2 p q = do
  a <- p
  comma
  b <- q
  return (a, b)

commaSep3 ::
  (CharParser e s m) =>
  m a -> m b -> m c -> m (a, b, c)
commaSep3 p q r = do
  a <- p
  comma
  b <- q
  comma
  c <- r
  return (a, b, c)

commaSep4 ::
  (CharParser e s m) =>
  m a -> m b -> m c -> m d -> m (a, b, c, d)
commaSep4 p q r s = do
  a <- p
  comma
  b <- q
  comma
  c <- r
  comma
  d <- s
  return (a, b, c, d)

commaSep5 ::
  (CharParser err s m) =>
  m a -> m b -> m c -> m d -> m e -> m (a, b, c, d, e)
commaSep5 p q r s t = do
  a <- p
  comma
  b <- q
  comma
  c <- r
  comma
  d <- s
  comma
  e <- t
  return (a, b, c, d, e)

bracketCommaSep2 ::
  (CharParser e s m) =>
  m a -> m b -> m (a, b)
bracketCommaSep2 p q = betweenBrackets (commaSep2 p q)

bracketCommaSep3 ::
  (CharParser e s m) =>
  m a -> m b -> m c -> m (a, b, c)
bracketCommaSep3 p q r = betweenBrackets (commaSep3 p q r)

bracketCommaSep4 ::
  (CharParser e s m) =>
  m a -> m b -> m c -> m d -> m (a, b, c, d)
bracketCommaSep4 p q r s = betweenBrackets (commaSep4 p q r s)

bracketCommaSep5 ::
  (CharParser err s m) =>
  m a -> m b -> m c -> m d -> m e -> m (a, b, c, d, e)
bracketCommaSep5 p q r s t = betweenBrackets (commaSep5 p q r s t)

parenCommaSep2 ::
  (CharParser e s m) =>
  m a -> m b -> m (a, b)
parenCommaSep2 p q = betweenParens (commaSep2 p q)

parenCommaSep3 ::
  (CharParser e s m) =>
  m a -> m b -> m c -> m (a, b, c)
parenCommaSep3 p q r = betweenParens (commaSep3 p q r)

parenCommaSep4 ::
  (CharParser e s m) =>
  m a -> m b -> m c -> m d -> m (a, b, c, d)
parenCommaSep4 p q r s = betweenParens (commaSep4 p q r s)

parenCommaSep5 ::
  (CharParser err s m) =>
  m a -> m b -> m c -> m d -> m e -> m (a, b, c, d, e)
parenCommaSep5 p q r s t = betweenParens (commaSep5 p q r s t)

braceCommaSep2 ::
  (CharParser e s m) =>
  m a -> m b -> m (a, b)
braceCommaSep2 p q = betweenBraces (commaSep2 p q)

braceCommaSep3 ::
  (CharParser e s m) =>
  m a -> m b -> m c -> m (a, b, c)
braceCommaSep3 p q r = betweenBraces (commaSep3 p q r)

braceCommaSep4 ::
  (CharParser e s m) =>
  m a -> m b -> m c -> m d -> m (a, b, c, d)
braceCommaSep4 p q r s = betweenBraces (commaSep4 p q r s)

braceCommaSep5 ::
  (CharParser err s m) =>
  m a -> m b -> m c -> m d -> m e -> m (a, b, c, d, e)
braceCommaSep5 p q r s t = betweenBraces (commaSep5 p q r s t)

angleCommaSep2 ::
  (CharParser e s m) =>
  m a -> m b -> m (a, b)
angleCommaSep2 p q = betweenAngles (commaSep2 p q)

angleCommaSep3 ::
  (CharParser e s m) =>
  m a -> m b -> m c -> m (a, b, c)
angleCommaSep3 p q r = betweenAngles (commaSep3 p q r)

angleCommaSep4 ::
  (CharParser e s m) =>
  m a -> m b -> m c -> m d -> m (a, b, c, d)
angleCommaSep4 p q r s = betweenAngles (commaSep4 p q r s)

angleCommaSep5 ::
  (CharParser err s m) =>
  m a -> m b -> m c -> m d -> m e -> m (a, b, c, d, e)
angleCommaSep5 p q r s t = betweenAngles (commaSep5 p q r s t)

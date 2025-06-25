{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module HieraSynth.TypeSignature
  ( TypeSignature (..),
    typeSignatureParser,
  )
where

import Data.List ((\\))
import Grisette
  ( PPrint (pformatPrec),
    PPrint1 (liftPFormatPrec),
    allClasses01,
    derive,
    pformatPrec1,
    pprintClasses,
    (<+>),
  )
import HieraSynth.Type.TypeParser (TypeParser (typeParser))
import HieraSynth.Util.Parser
  ( CharParser,
    parenCommaSepOrSingleton,
    symbol,
  )
import HieraSynth.Util.Pretty (encloseListIfNotSingle)

data TypeSignature ty = TypeSignature {argTypes :: [ty], resTypes :: [ty]}

derive [''TypeSignature] (allClasses01 \\ pprintClasses)

instance (PPrint ty) => PPrint (TypeSignature ty) where
  pformatPrec = pformatPrec1

instance PPrint1 TypeSignature where
  liftPFormatPrec ppt _ n (TypeSignature argTypes resTypes) =
    encloseListIfNotSingle "(" ")" "," (ppt n <$> argTypes)
      <+> "->"
      <+> encloseListIfNotSingle "(" ")" "," (ppt n <$> resTypes)

typeSignatureParser :: (CharParser e s m, TypeParser ty) => m (TypeSignature ty)
typeSignatureParser = do
  argTypes <- parenCommaSepOrSingleton typeParser
  symbol "->"
  resTypes <- parenCommaSepOrSingleton typeParser
  return $ TypeSignature argTypes resTypes

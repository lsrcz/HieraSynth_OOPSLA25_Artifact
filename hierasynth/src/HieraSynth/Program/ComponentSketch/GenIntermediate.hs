{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module HieraSynth.Program.ComponentSketch.GenIntermediate
  ( GenIntermediate (..),
    Intermediates (..),
    genIntermediates,
    genOpIntermediates,
  )
where

import GHC.Generics (Generic)
import Grisette
  ( GenSym (fresh),
    Mergeable,
    allClasses01,
    derive,
    liftUnion,
  )
import Grisette.Lib.Data.Traversable (mrgTraverse)
import HieraSynth.Context (MonadAngelicContext)
import HieraSynth.Operator.OpSemantics (DefaultSem)
import HieraSynth.TypeSignature
  ( TypeSignature (argTypes, resTypes),
  )

class (Mergeable val) => GenIntermediate sem ty val where
  genIntermediate :: (MonadAngelicContext ctx) => sem -> ty -> ctx val

genIntermediates ::
  (GenIntermediate sem ty val, MonadAngelicContext ctx) =>
  sem ->
  [ty] ->
  ctx [val]
genIntermediates sem = mrgTraverse (genIntermediate sem)

data Intermediates val = Intermediates
  { argIntermediates :: [val],
    resIntermediates :: [val]
  }
  deriving (Generic)

derive [''Intermediates] allClasses01

genOpIntermediates ::
  forall semObj ty val ctx p.
  (GenIntermediate semObj ty val, MonadAngelicContext ctx) =>
  p ty ->
  semObj ->
  TypeSignature ty ->
  ctx (Intermediates val)
genOpIntermediates _ sem signature = do
  arg <- genIntermediates sem $ argTypes signature
  res <- genIntermediates sem $ resTypes signature
  return $ Intermediates arg res

instance (GenSym ty a) => GenIntermediate DefaultSem ty a where
  genIntermediate _ ty = fresh ty >>= liftUnion

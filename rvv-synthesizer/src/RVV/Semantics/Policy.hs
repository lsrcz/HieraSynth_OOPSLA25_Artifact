{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Semantics.Policy
  ( TailPolicy (..),
    ta,
    tu,
    MaskPolicy (..),
    ma,
    mu,
    Policy (..),
    tumuPolicy,
    tuPolicy,
    muPolicy,
    nonePolicy,
    symPolicy,
  )
where

import Grisette
  ( GenSym (fresh),
    GenSymSimple (simpleFresh),
    PPrint (pformat),
    SimpleMergeable,
    SymBool,
    chooseFresh,
    deriveWith,
    makeNamedUnifiedCtor,
    mrgIf,
  )
import Grisette.Unified
  ( EvalModeTag (C, S),
    GetData,
  )
import RVV.EvalMode (EvalMode)
import RVV.Util.Derive (deriveFull, firstModeDeriveConfig, noModeDeriveConfig)

data TailPolicy = TailAgnostic | TailUndisturbed

instance Monoid TailPolicy where
  mempty = TailAgnostic

instance Semigroup TailPolicy where
  TailAgnostic <> x = x
  x <> _ = x

deriveFull noModeDeriveConfig [''TailPolicy]
makeNamedUnifiedCtor [''EvalMode] ["ta", "tu"] ''TailPolicy

instance PPrint TailPolicy where
  pformat TailAgnostic = "ta"
  pformat TailUndisturbed = "tu"

instance GenSym () TailPolicy where
  fresh _ = chooseFresh [TailAgnostic, TailUndisturbed]

data MaskPolicy = MaskAgnostic | MaskUndisturbed

instance Monoid MaskPolicy where
  mempty = MaskAgnostic

instance Semigroup MaskPolicy where
  MaskAgnostic <> x = x
  x <> _ = x

deriveFull noModeDeriveConfig [''MaskPolicy]
makeNamedUnifiedCtor [''EvalMode] ["ma", "mu"] ''MaskPolicy

instance PPrint MaskPolicy where
  pformat MaskAgnostic = "ma"
  pformat MaskUndisturbed = "mu"

instance GenSym () MaskPolicy where
  fresh _ = chooseFresh [MaskAgnostic, MaskUndisturbed]

data Policy mode = Policy
  { tailPolicy :: GetData mode TailPolicy,
    maskPolicy :: GetData mode MaskPolicy
  }

instance Monoid (Policy 'C) where
  mempty = Policy mempty mempty

instance Semigroup (Policy 'C) where
  Policy t1 m1 <> Policy t2 m2 = Policy (t1 <> t2) (m1 <> m2)

deriveFull firstModeDeriveConfig [''Policy]
deriveWith firstModeDeriveConfig [''Policy] [''SimpleMergeable]

tumuPolicy :: forall mode. (EvalMode mode) => Policy mode
tumuPolicy = Policy {tailPolicy = tu, maskPolicy = mu}

tuPolicy :: forall mode. (EvalMode mode) => Policy mode
tuPolicy = Policy {tailPolicy = tu, maskPolicy = ma}

muPolicy :: forall mode. (EvalMode mode) => Policy mode
muPolicy = Policy {tailPolicy = ta, maskPolicy = mu}

nonePolicy :: forall mode. (EvalMode mode) => Policy mode
nonePolicy = Policy {tailPolicy = ta, maskPolicy = ma}

instance GenSym () (Policy 'S)

instance GenSymSimple () (Policy 'S) where
  simpleFresh _ = do
    t <- simpleFresh ()
    m <- simpleFresh ()
    return $ Policy t m

instance (EvalMode mode) => PPrint (Policy mode) where
  pformat Policy {..} = pformat tailPolicy <> pformat maskPolicy

symPolicy :: SymBool -> SymBool -> Policy 'S
symPolicy tailPolicy maskPolicy =
  Policy (mrgIf tailPolicy ta tu) (mrgIf maskPolicy ma mu)

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.Parameter.Destination (Destination (..), ud, pd) where

import Grisette
  ( PPrint (pformat),
    makeNamedUnifiedCtor,
  )
import RVV.EvalMode (EvalMode)
import RVV.Util.Derive (deriveFull, noModeDeriveConfig)

data Destination = UseUndefinedDest | UseProvidedDest

deriveFull noModeDeriveConfig [''Destination]
makeNamedUnifiedCtor [''EvalMode] ["ud", "pd"] ''Destination

instance PPrint Destination where
  pformat UseUndefinedDest = "ud"
  pformat UseProvidedDest = "pd"

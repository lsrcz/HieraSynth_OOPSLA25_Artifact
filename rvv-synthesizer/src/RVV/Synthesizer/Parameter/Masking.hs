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

module RVV.Synthesizer.Parameter.Masking (Masking (..), fm, pm) where

import Grisette
  ( PPrint (pformat),
    makeNamedUnifiedCtor,
  )
import RVV.EvalMode (EvalMode)
import RVV.Util.Derive (deriveFull, noModeDeriveConfig)

data Masking = UseFullMask | UseProvidedMask

deriveFull noModeDeriveConfig [''Masking]
makeNamedUnifiedCtor [''EvalMode] ["fm", "pm"] ''Masking

instance PPrint Masking where
  pformat UseFullMask = "fm"
  pformat UseProvidedMask = "pm"

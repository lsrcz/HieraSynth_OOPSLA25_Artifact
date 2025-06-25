{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

module EvalMode
  ( EvalMode,
    MonadEvalMode,
    deriveWithEvalMode,
    MonadEvalContext,
  )
where

import Grisette
  ( DeriveConfig (evalModeConfig),
    EvalModeConfig (EvalModeConstraints),
    deriveWith,
  )
import HieraSynth.Context (MonadContext)
import Grisette.Unified
  ( TheoryToUnify (UBool, UInteger),
    genEvalMode,
  )
import Language.Haskell.TH (DecsQ, Name)

genEvalMode "EvalMode" [UBool, UInteger]

deriveWithEvalMode :: [Name] -> [Name] -> DecsQ
deriveWithEvalMode =
  deriveWith
    ( mempty
        { evalModeConfig = [(0, EvalModeConstraints [''EvalMode])]
        }
    )

type MonadEvalContext mode ctx = (MonadContext ctx, MonadEvalMode mode ctx)

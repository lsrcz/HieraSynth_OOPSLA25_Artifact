{-# LANGUAGE TemplateHaskellQuotes #-}

module RVV.Util.Derive
  ( deriveFull,
    deriveFullExcept,
    deriveBasic,
    noModeDeriveConfig,
    firstModeDeriveConfig,
    deriveNoSymEval,
    deriveNoSymEvalNoHash,
  )
where

import Control.DeepSeq (NFData)
import Data.Bytes.Serial (Serial)
import Data.Hashable (Hashable)
import qualified Data.Set as S
import Grisette
  ( DeriveConfig (evalModeConfig),
    EvalModeConfig (EvalModeConstraints),
    EvalSym,
    Mergeable,
    SymEq,
    ToCon,
    ToSym,
    deriveWith,
  )
import Grisette.Unified (UnifiedSymEq)
import Language.Haskell.TH (DecsQ, Name)
import RVV.EvalMode (EvalMode)

noModeDeriveConfig :: DeriveConfig
noModeDeriveConfig = mempty

firstModeDeriveConfig :: DeriveConfig
firstModeDeriveConfig =
  mempty {evalModeConfig = [(0, EvalModeConstraints [''EvalMode])]}

deriveFull :: DeriveConfig -> [Name] -> DecsQ
deriveFull config ns = deriveFullExcept config ns []

deriveFullExcept :: DeriveConfig -> [Name] -> [Name] -> DecsQ
deriveFullExcept config ns l =
  deriveWith
    config
    ns
    ( S.toList $
        S.fromList
          [ ''Show,
            ''Eq,
            ''SymEq,
            ''Mergeable,
            ''Hashable,
            ''NFData,
            ''EvalSym,
            ''ToSym,
            ''ToCon,
            ''Serial,
            ''UnifiedSymEq
          ]
          `S.difference` S.fromList l
    )

deriveNoSymEvalNoHash :: [Name] -> DecsQ
deriveNoSymEvalNoHash ns =
  deriveWith
    noModeDeriveConfig
    ns
    [ ''Show,
      ''Eq,
      ''NFData,
      ''Serial
    ]

deriveNoSymEval :: [Name] -> DecsQ
deriveNoSymEval ns =
  deriveWith
    noModeDeriveConfig
    ns
    [ ''Show,
      ''Eq,
      ''Hashable,
      ''NFData,
      ''Serial
    ]

deriveBasic :: DeriveConfig -> [Name] -> DecsQ
deriveBasic config ns =
  deriveWith
    config
    ns
    [ ''Show,
      ''Eq,
      ''Mergeable
    ]

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Semantics.MachineConfig
  ( MachineConfig (..),
    AllowPartialVL (..),
    MachineScalableConfig (..),
    MachineBaseConfig (..),
  )
where

import Control.DeepSeq (NFData)
import Data.Bytes.Serial (Serial)
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import Data.Hashable (Hashable)
import Grisette (PPrint (pformat), deriveWith, viaShow)
import RVV.Util.Derive (noModeDeriveConfig)

data AllowPartialVL = AllowPartialVL | DisallowPartialVL

instance Show AllowPartialVL where
  show AllowPartialVL = "allow"
  show DisallowPartialVL = "disallow"

instance PPrint AllowPartialVL where
  pformat = viaShow

deriveWith
  noModeDeriveConfig
  [''AllowPartialVL]
  [''Eq, ''NFData, ''Serial, ''Hashable]

data MachineConfig = MachineConfig
  { baseConfig :: MachineBaseConfig,
    scalableConfig :: MachineScalableConfig,
    useVLMask :: Bool,
    allowPartialVL :: AllowPartialVL
  }

data MachineScalableConfig = MachineScalableConfig
  { machineVectorLength :: Int,
    machineMemoryBlockLengths :: M.HashMap Int Int
  }

data MachineBaseConfig = MachineBaseConfig
  { machineScalarLength :: Int,
    machinePointerUnit :: Int,
    machineScalarImmLength :: Int,
    machineMemoryBlockIds :: S.HashSet Int,
    machineVectorImmLength :: Int
  }

deriveWith
  noModeDeriveConfig
  [''MachineScalableConfig]
  [''Eq, ''Show, ''NFData, ''Serial, ''Hashable, ''PPrint]

deriveWith
  noModeDeriveConfig
  [''MachineBaseConfig]
  [''Eq, ''Show, ''NFData, ''Serial, ''Hashable, ''PPrint]

deriveWith
  noModeDeriveConfig
  [''MachineConfig]
  [''Eq, ''Show, ''NFData, ''Serial, ''Hashable, ''PPrint]

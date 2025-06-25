{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.Operator.Common.PPrint
  ( bracketList,
    formatMaskMulPolicy,
    formatXmulDelegatedMaskMul,
    formatVTypeDest,
    formatVTypeDestMask,
    formatVTypeImmDest,
    formatXmulDelegatedMaskMulImm,
    formatVTypeImmDestMask,
    formatVTypeMask,
    formatVTypeImmMask,
    pformatChoices,
    formatMaskMulPolicies,
    formatVTypeDests,
    formatVTypeDestsMasks,
    formatVTypeMasks,
    formatVTypeImmsDests,
    formatVTypeImmsMasks,
    formatVTypeImmsDestsMasks,
    formatDelegatedXMulMaskMul,
    formatDelegatedXMulMaskMulImm,
    withName,
    PPrintArgList (..),
    choices,
  )
where

import Data.Functor.Identity (Identity (Identity))
import Data.String (IsString (fromString))
import Grisette (Doc, PPrint (pformat))
import HieraSynth.Util.Pretty (encloseList, encloseListIfNotSingle)
import Grisette.Unified (EvalModeTag (C, S))
import RVV.Semantics.Imm (Imm)
import RVV.Semantics.Multiplier (MaskMul, WidthMul)
import RVV.Semantics.Policy (Policy)
import RVV.Semantics.VectorConfig (VectorConfig)
import RVV.Synthesizer.Operator.Common.ImmSpec (ImmSpec)
import RVV.Synthesizer.Parameter.Destination (Destination)
import RVV.Synthesizer.Parameter.Masking (Masking)

bracketList :: [Doc ann] -> Doc ann
bracketList = encloseList "[" "]" ","

formatMaskMulPolicy :: MaskMul -> Policy 'C -> Doc ann
formatMaskMulPolicy maskMul policy =
  bracketList
    [ "mmul=" <> pformat maskMul,
      pformat policy
    ]

formatXmulDelegatedMaskMul :: WidthMul -> MaskMul -> Doc ann
formatXmulDelegatedMaskMul delegatedXMul maskMul =
  bracketList
    [ "delegated_xmul=" <> pformat delegatedXMul,
      "mmul=" <> pformat maskMul
    ]

formatVTypeDest :: VectorConfig -> Identity Destination -> Doc ann
formatVTypeDest vtype (Identity dest) =
  bracketList [pformat vtype, pformat dest]

formatVTypeDestMask ::
  Maybe (Doc ann) ->
  VectorConfig ->
  Identity Destination ->
  Identity Masking ->
  Doc ann
formatVTypeDestMask vtypeComment vtype (Identity dest) (Identity mask) =
  bracketList
    [ maybe "" (<> "=") vtypeComment <> pformat vtype,
      pformat dest,
      pformat mask
    ]

formatVTypeImmDest ::
  VectorConfig -> Doc ann -> Imm 'C -> Identity Destination -> Doc ann
formatVTypeImmDest vtype imm_name imm (Identity dest) =
  bracketList
    [ pformat vtype,
      imm_name <> "=" <> pformat imm,
      pformat dest
    ]

formatXmulDelegatedMaskMulImm ::
  WidthMul -> MaskMul -> Doc ann -> Imm 'C -> Doc ann
formatXmulDelegatedMaskMulImm delegatedXMul maskMul imm_name imm =
  bracketList
    [ "delegated_xmul=" <> pformat delegatedXMul,
      "mmul=" <> pformat maskMul,
      imm_name <> "=" <> pformat imm
    ]

formatVTypeImmDestMask ::
  Maybe (Doc ann) ->
  VectorConfig ->
  Doc ann ->
  Imm 'C ->
  Identity Destination ->
  Identity Masking ->
  Doc ann
formatVTypeImmDestMask vtypeComment vtype imm_name imm (Identity dest) (Identity mask) =
  bracketList
    [ maybe "" (<> "=") vtypeComment <> pformat vtype,
      imm_name <> "=" <> pformat imm,
      pformat dest,
      pformat mask
    ]

formatVTypeMask :: Maybe (Doc ann) -> VectorConfig -> Identity Masking -> Doc ann
formatVTypeMask vtypeComment vtype (Identity mask) =
  bracketList
    [ maybe "" (<> "=") vtypeComment <> pformat vtype,
      pformat mask
    ]

formatVTypeImmMask ::
  Maybe (Doc ann) ->
  VectorConfig ->
  Doc ann ->
  Identity (Imm 'C) ->
  Identity Masking ->
  Doc ann
formatVTypeImmMask vtypeComment vtype imm_name (Identity imm) (Identity mask) =
  bracketList
    [ maybe "" (<> "=") vtypeComment <> pformat vtype,
      imm_name <> "=" <> pformat imm,
      pformat mask
    ]

pformatChoices :: (PPrint a) => [a] -> Doc ann
pformatChoices choices =
  encloseListIfNotSingle "<" ">" "," $ map pformat choices

formatMaskMulPolicies :: MaskMul -> [Policy 'S] -> Doc ann
formatMaskMulPolicies maskMul policies =
  bracketList
    [ "maskMul=" <> pformat maskMul,
      pformatChoices policies
    ]

formatVTypeDests ::
  VectorConfig -> [Destination] -> Doc ann
formatVTypeDests vtype dest =
  bracketList [pformat vtype, pformatChoices dest]

formatVTypeDestsMasks ::
  VectorConfig -> [Destination] -> [Masking] -> Doc ann
formatVTypeDestsMasks vtype dest mask =
  bracketList [pformat vtype, pformatChoices dest, pformatChoices mask]

formatVTypeMasks :: VectorConfig -> [Masking] -> Doc ann
formatVTypeMasks vtype mask = bracketList [pformat vtype, pformatChoices mask]

formatVTypeImmsDests ::
  VectorConfig -> Doc ann -> ImmSpec -> [Destination] -> Doc ann
formatVTypeImmsDests vtype imm_name imm dest =
  bracketList
    [ pformat vtype,
      imm_name <> "=" <> pformat imm,
      pformatChoices dest
    ]

formatVTypeImmsMasks ::
  VectorConfig -> Doc ann -> ImmSpec -> [Masking] -> Doc ann
formatVTypeImmsMasks vtype imm_name imm mask =
  bracketList
    [ pformat vtype,
      imm_name <> "=" <> pformat imm,
      pformatChoices mask
    ]

formatVTypeImmsDestsMasks ::
  VectorConfig -> Doc ann -> ImmSpec -> [Destination] -> [Masking] -> Doc ann
formatVTypeImmsDestsMasks vtype imm_name imm dest mask =
  bracketList
    [ pformat vtype,
      imm_name <> "=" <> pformat imm,
      pformatChoices dest,
      pformatChoices mask
    ]

formatDelegatedXMulMaskMul :: WidthMul -> MaskMul -> Doc ann
formatDelegatedXMulMaskMul delegatedXMul maskMul =
  bracketList
    [ "delegated_xmul=" <> pformat delegatedXMul,
      "maskMul=" <> pformat maskMul
    ]

formatDelegatedXMulMaskMulImm ::
  WidthMul -> MaskMul -> Doc ann -> ImmSpec -> Doc ann
formatDelegatedXMulMaskMulImm delegatedXMul maskMul imm_name imm =
  bracketList
    [ "delegated_xmul=" <> pformat delegatedXMul,
      "maskMul=" <> pformat maskMul,
      imm_name <> "=" <> pformat imm
    ]

data Named a = Named String a deriving (Show)

instance (PPrint a) => PPrint (Named a) where
  pformat (Named name a) = fromString name <> "=" <> pformat a

withName :: String -> a -> Named a
withName = Named

newtype Choices a = Choices [a] deriving (Show)

instance (PPrint a) => PPrint (Choices a) where
  pformat (Choices choices) = pformatChoices choices

choices :: [a] -> Choices a
choices = Choices

class PPrintArgList a where
  pformatArgList :: a -> Doc ann

instance {-# OVERLAPPABLE #-} (PPrint a) => PPrintArgList a where
  pformatArgList a = bracketList [pformat a]

instance {-# OVERLAPPING #-} (PPrint a, PPrint b) => PPrintArgList (a, b) where
  pformatArgList (a, b) = bracketList [pformat a, pformat b]

instance
  {-# OVERLAPPING #-}
  (PPrint a, PPrint b, PPrint c) =>
  PPrintArgList (a, b, c)
  where
  pformatArgList (a, b, c) = bracketList [pformat a, pformat b, pformat c]

instance
  {-# OVERLAPPING #-}
  (PPrint a, PPrint b, PPrint c, PPrint d) =>
  PPrintArgList (a, b, c, d)
  where
  pformatArgList (a, b, c, d) =
    bracketList [pformat a, pformat b, pformat c, pformat d]

instance
  {-# OVERLAPPING #-}
  (PPrint a, PPrint b, PPrint c, PPrint d, PPrint e) =>
  PPrintArgList (a, b, c, d, e)
  where
  pformatArgList (a, b, c, d, e) =
    bracketList [pformat a, pformat b, pformat c, pformat d, pformat e]

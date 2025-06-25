{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module RVV.Synthesizer.Parameter.MaskLogicalOpCode
  ( MaskLogicalOpCode (..),
    mkMAnd,
    mkMNand,
    mkMAndn,
    mkMXor,
    mkMOr,
    mkMNor,
    mkMOrn,
    mkMXnor,
    interpretMaskLogicalOpCode,
    commutativeMaskLogicalOpCode,
    maskLogicalOpCodeParser,
    interpretMaskLogicalOpCodeUninitialized,
    interpretMaskLogicalOpCodeSemantics,
  )
where

import Control.Monad.Identity (Identity (Identity))
import Data.Bits (Bits (complement, xor, (.&.), (.|.)))
import Data.String (IsString)
import Grisette
  ( LogicalOp (symNot, (.&&), (.||)),
    PPrint (pformat),
    makePrefixedUnifiedCtor,
  )
import HieraSynth.Util.Parser (CharParser)
import Grisette.Unified (GetSomeWordN, (./=), (.==))
import RVV.EvalMode (EvalMode)
import RVV.Semantics.Element (MaskElement (MaskElement))
import RVV.Synthesizer.Parameter.Common (opCodeParser)
import RVV.Util.Derive (deriveFull, noModeDeriveConfig)

data MaskLogicalOpCode
  = MAnd
  | MNand
  | MAndn
  | MXor
  | MOr
  | MNor
  | MOrn
  | MXnor

commutativeMaskLogicalOpCode :: [MaskLogicalOpCode]
commutativeMaskLogicalOpCode =
  [MAnd, MNand, MXor, MOr, MNor, MXnor]

deriveFull noModeDeriveConfig [''MaskLogicalOpCode]
makePrefixedUnifiedCtor [''EvalMode] "mk" ''MaskLogicalOpCode

instance PPrint MaskLogicalOpCode where
  pformat MAnd = "mand"
  pformat MNand = "mnand"
  pformat MAndn = "mandn"
  pformat MXor = "mxor"
  pformat MOr = "mor"
  pformat MNor = "mnor"
  pformat MOrn = "morn"
  pformat MXnor = "mxnor"

interpretMaskLogicalOpCodeUninitialized ::
  (EvalMode mode) =>
  MaskLogicalOpCode ->
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  GetSomeWordN mode
interpretMaskLogicalOpCodeUninitialized MAnd ad au bd bu =
  (ad .&. bu) .|. (bd .&. au) .|. (au .&. bu)
interpretMaskLogicalOpCodeUninitialized MNand ad au bd bu =
  (ad .&. bu) .|. (bd .&. au) .|. (au .&. bu)
interpretMaskLogicalOpCodeUninitialized MAndn ad au bd bu =
  (ad .&. bu) .|. (complement bd .&. au) .|. (au .&. bu)
interpretMaskLogicalOpCodeUninitialized MXor _ au _ bu = au .|. bu
interpretMaskLogicalOpCodeUninitialized MOr ad au bd bu =
  (complement ad .&. bu) .|. (complement bd .&. au) .|. (au .&. bu)
interpretMaskLogicalOpCodeUninitialized MNor ad au bd bu =
  (complement ad .&. bu) .|. (complement bd .&. au) .|. (au .&. bu)
interpretMaskLogicalOpCodeUninitialized MOrn ad au bd bu =
  (complement ad .&. bu) .|. (bd .&. au) .|. (au .&. bu)
interpretMaskLogicalOpCodeUninitialized MXnor _ au _ bu = au .|. bu

interpretMaskLogicalOpCodeSemantics ::
  (EvalMode mode) =>
  MaskLogicalOpCode ->
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  GetSomeWordN mode
interpretMaskLogicalOpCodeSemantics MAnd ad bd = ad .&. bd
interpretMaskLogicalOpCodeSemantics MNand ad bd = complement $ ad .&. bd
interpretMaskLogicalOpCodeSemantics MAndn ad bd = ad .&. complement bd
interpretMaskLogicalOpCodeSemantics MXor ad bd = ad `xor` bd
interpretMaskLogicalOpCodeSemantics MOr ad bd = ad .|. bd
interpretMaskLogicalOpCodeSemantics MNor ad bd = complement $ ad .|. bd
interpretMaskLogicalOpCodeSemantics MOrn ad bd = ad .|. complement bd
interpretMaskLogicalOpCodeSemantics MXnor ad bd = complement $ ad `xor` bd

interpretMaskLogicalOpCode ::
  (EvalMode mode) =>
  MaskLogicalOpCode ->
  MaskElement mode ->
  MaskElement mode ->
  MaskElement mode
interpretMaskLogicalOpCode MAnd (MaskElement ad au) (MaskElement bd bu) =
  MaskElement (ad .&& bd) ((ad .&& bu) .|| (bd .&& au) .|| (au .&& bu))
interpretMaskLogicalOpCode MNand (MaskElement ad au) (MaskElement bd bu) =
  MaskElement (symNot $ ad .&& bd) ((ad .&& bu) .|| (bd .&& au) .|| (au .&& bu))
interpretMaskLogicalOpCode MAndn (MaskElement ad au) (MaskElement bd bu) =
  MaskElement
    (ad .&& symNot bd)
    ((ad .&& bu) .|| (symNot bd .&& au) .|| (au .&& bu))
interpretMaskLogicalOpCode MXor (MaskElement ad au) (MaskElement bd bu) =
  MaskElement (ad ./= bd) (au .|| bu)
interpretMaskLogicalOpCode MOr (MaskElement ad au) (MaskElement bd bu) =
  MaskElement
    (ad .|| bd)
    ((symNot ad .&& bu) .|| (symNot bd .&& au) .|| (au .&& bu))
interpretMaskLogicalOpCode MNor (MaskElement ad au) (MaskElement bd bu) =
  MaskElement
    (symNot $ ad .|| bd)
    ((symNot ad .&& bu) .|| (symNot bd .&& au) .|| (au .&& bu))
interpretMaskLogicalOpCode MOrn (MaskElement ad au) (MaskElement bd bu) =
  MaskElement
    (ad .|| symNot bd)
    ((symNot ad .&& bu) .|| (bd .&& au) .|| (au .&& bu))
interpretMaskLogicalOpCode MXnor (MaskElement ad au) (MaskElement bd bu) =
  MaskElement (ad .== bd) (au .|| bu)

{-# INLINE maskLogicalOpCodeNames #-}
maskLogicalOpCodeNames :: (IsString s) => [(s, MaskLogicalOpCode)]
maskLogicalOpCodeNames =
  [ ("mand", MAnd),
    ("mnand", MNand),
    ("mandn", MAndn),
    ("mxor", MXor),
    ("mor", MOr),
    ("mnor", MNor),
    ("morn", MOrn),
    ("mxnor", MXnor)
  ]

maskLogicalOpCodeParser ::
  (CharParser e s m) => s -> s -> m (Identity MaskLogicalOpCode)
maskLogicalOpCodeParser prefix =
  fmap Identity . opCodeParser maskLogicalOpCodeNames prefix

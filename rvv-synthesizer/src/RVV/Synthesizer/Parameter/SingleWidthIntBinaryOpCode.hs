{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.Parameter.SingleWidthIntBinaryOpCode
  ( SingleWidthIntBinaryOpCode (..),
    mkAdd,
    mkSub,
    mkRSub,
    mkAnd,
    mkOr,
    mkXor,
    mkAndn,
    mkOrn,
    mkXnor,
    mkMul,
    mkMulh,
    mkMulhu,
    mkMulhsu,
    mkSll,
    mkSrl,
    mkSra,
    mkRor,
    mkRol,
    mkMin,
    mkMinu,
    mkMax,
    mkMaxu,
    mkDiv,
    mkDivu,
    mkRem,
    mkRemu,
    mkSAdd,
    mkSAddu,
    mkSSub,
    mkSSubu,
    mkSlt,
    mkSltu,
    mkSeq,
    mkSne,
    mkCZeroNez,
    mkCZeroEqz,
    interpretSingleWidthBinaryOpCode,
    commutativeSingleWidthIntBinaryOp,
    binaryOpCodeUseAnyInvalidSemantics,
    singleWidthIntBinaryOpCodeParser,
    singleWidthIntBinaryOpCodeFeature,
  )
where

import Control.Exception (ArithException (Overflow, Underflow))
import Control.Monad.Except (runExceptT)
import Control.Monad.Identity (Identity (Identity))
import Data.Bits
  ( Bits (complement, shiftL, shiftR, xor, (.&.), (.|.)),
    FiniteBits (finiteBitSize),
  )
import qualified Data.HashSet as HS
import Data.String (IsString)
import GHC.Stack (HasCallStack)
import Grisette
  ( BV (bv, bvSelect, bvSext, bvZext),
    DivOr (divOr, remOr),
    LogicalOp ((.&&)),
    Mergeable,
    PPrint (pformat),
    SignConversion (toSigned, toUnsigned),
    SomeBVException,
    SymRotate (symRotate, symRotateNegated),
    SymShift (symShift, symShiftNegated),
    makePrefixedUnifiedCtor,
  )
import HieraSynth.Util.Parser (CharParser)
import Grisette.Unified
  ( BaseMonad,
    DecideEvalMode,
    GetBool,
    GetSomeIntN,
    GetSomeWordN,
    UnifiedITEOp,
    UnifiedSymEq,
    safeAdd,
    safeSub,
    symIte,
    symIteMerge,
    (./=),
    (.<),
    (.==),
  )
import RVV.EvalMode (EvalMode)
import RVV.Semantics.Element (VectorElement (VectorElement))
import RVV.Synthesizer.Feature.FeatureSet
  ( Feature (Compare, Logical),
    FeatureSet (opFeatures),
    Signedness (Signed, Unsigned),
    linearArithFeature,
    multiplicationFeature,
    multiplicationHighFeature,
    saturateLinearArithFeature,
    signedFeature,
  )
import RVV.Synthesizer.Parameter.Common
  ( combineAnyInvalid,
    combineArithInvalid,
    opCodeParser,
    shiftBits,
  )
import RVV.Util.Derive (deriveFull, noModeDeriveConfig)

data SingleWidthIntBinaryOpCode
  = Add
  | Sub
  | RSub
  | And
  | Or
  | Xor
  | Andn
  | Orn
  | Xnor
  | Mul
  | Mulh
  | Mulhu
  | Mulhsu
  | Sll
  | Srl
  | Sra
  | Ror
  | Rol
  | Min
  | Minu
  | Max
  | Maxu
  | Div
  | Divu
  | Rem
  | Remu
  | Slt
  | Sltu
  | Seq
  | Sne
  | SAddu
  | SAdd
  | SSubu
  | SSub
  | CZeroNez
  | CZeroEqz

commutativeSingleWidthIntBinaryOp :: [SingleWidthIntBinaryOpCode]
commutativeSingleWidthIntBinaryOp =
  [Add, And, Or, Xor, Mul, Mulh, Mulhu, Min, Minu, Max, Maxu, SAdd, SAddu]

deriveFull noModeDeriveConfig [''SingleWidthIntBinaryOpCode]
makePrefixedUnifiedCtor [''EvalMode] "mk" ''SingleWidthIntBinaryOpCode

riscvDiv ::
  forall mode v.
  ( DivOr v,
    Mergeable v,
    BV v,
    FiniteBits v,
    DecideEvalMode mode,
    UnifiedITEOp mode v,
    UnifiedSymEq mode v
  ) =>
  v ->
  v ->
  v
riscvDiv l r =
  symIte
    (r .== bv (finiteBitSize r) 0 :: GetBool mode)
    (bv (finiteBitSize l) $ -1)
    $ divOr (bv (finiteBitSize l) 1 `shiftL` (finiteBitSize l - 1)) l r

riscvRem ::
  forall mode v.
  ( DivOr v,
    Mergeable v,
    BV v,
    FiniteBits v,
    DecideEvalMode mode,
    UnifiedITEOp mode v,
    UnifiedSymEq mode v
  ) =>
  v ->
  v ->
  v
riscvRem l r =
  symIte (r .== bv (finiteBitSize r) 0 :: GetBool mode) l $
    remOr (bv (finiteBitSize l) 0) l r

binaryOpCodeUseAnyInvalidSemantics ::
  forall mode.
  (EvalMode mode) =>
  ( VectorElement mode ->
    VectorElement mode ->
    VectorElement mode
  ) ->
  ( VectorElement mode ->
    VectorElement mode ->
    VectorElement mode
  )
binaryOpCodeUseAnyInvalidSemantics f l@(VectorElement _ li) r@(VectorElement _ ri) =
  case f l r of
    VectorElement r _ ->
      VectorElement
        r
        ( symIte @mode
            (li .== 0 .&& ri .== 0)
            (bv (finiteBitSize r) 0)
            (bv (finiteBitSize r) $ -1)
        )

saddu ::
  forall mode.
  (EvalMode mode) =>
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  GetSomeWordN mode
saddu l r = symIteMerge $ do
  v <- runExceptT $ safeAdd @mode l r
  case v of
    Left (Left (_ :: SomeBVException)) -> error "Should not happen"
    Left (Right Underflow) -> return $ bv (finiteBitSize l) 0
    Left (Right Overflow) -> return $ bv (finiteBitSize l) $ -1
    Left _ -> error "Should not happen"
    Right v -> return v :: BaseMonad mode (GetSomeWordN mode)

sadd ::
  forall mode.
  (EvalMode mode) =>
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  GetSomeWordN mode
sadd l r = symIteMerge $ do
  v <- runExceptT $ safeAdd @mode (toSigned l :: GetSomeIntN mode) (toSigned r)
  case v of
    Left (Left (_ :: SomeBVException)) -> error "Should not happen"
    Left (Right Underflow) ->
      return $ bv (finiteBitSize l) 1 `shiftL` (finiteBitSize l - 1)
    Left (Right Overflow) -> return $ bv (finiteBitSize l) (-1) `shiftR` 1
    Left _ -> error "Should not happen"
    Right v -> return $ toUnsigned v :: BaseMonad mode (GetSomeWordN mode)

ssubu ::
  forall mode.
  (EvalMode mode) =>
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  GetSomeWordN mode
ssubu l r = symIteMerge $ do
  v <- runExceptT $ safeSub @mode l r
  case v of
    Left (Left (_ :: SomeBVException)) -> error "Should not happen"
    Left (Right Underflow) -> return $ bv (finiteBitSize l) 0
    Left (Right Overflow) -> return $ bv (finiteBitSize l) $ -1
    Left _ -> error "Should not happen"
    Right v -> return v :: BaseMonad mode (GetSomeWordN mode)

ssub ::
  forall mode.
  (EvalMode mode) =>
  GetSomeWordN mode ->
  GetSomeWordN mode ->
  GetSomeWordN mode
ssub l r = symIteMerge $ do
  v <- runExceptT $ safeSub @mode (toSigned l :: GetSomeIntN mode) (toSigned r)
  case v of
    Left (Left (_ :: SomeBVException)) -> error "Should not happen"
    Left (Right Underflow) ->
      return $ bv (finiteBitSize l) 1 `shiftL` (finiteBitSize l - 1)
    Left (Right Overflow) -> return $ bv (finiteBitSize l) (-1) `shiftR` 1
    Left _ -> error "Should not happen"
    Right v -> return $ toUnsigned v :: BaseMonad mode (GetSomeWordN mode)

ltu :: (EvalMode mode) => GetSomeWordN mode -> GetSomeWordN mode -> GetBool mode
ltu l r = l .< r

lt :: forall mode. (EvalMode mode) => GetSomeWordN mode -> GetSomeWordN mode -> GetBool mode
lt l r = toSigned l .< toSigned r

interpretSingleWidthBinaryOpCode ::
  forall mode.
  (EvalMode mode) =>
  SingleWidthIntBinaryOpCode ->
  VectorElement mode ->
  VectorElement mode ->
  VectorElement mode
interpretSingleWidthBinaryOpCode Add (VectorElement l li) (VectorElement r ri) =
  VectorElement (l + r) (combineArithInvalid li ri)
interpretSingleWidthBinaryOpCode RSub (VectorElement l li) (VectorElement r ri) =
  VectorElement (r - l) (combineArithInvalid li ri)
interpretSingleWidthBinaryOpCode Sub (VectorElement l li) (VectorElement r ri) =
  VectorElement (l - r) (combineArithInvalid li ri)
interpretSingleWidthBinaryOpCode And (VectorElement l li) (VectorElement r ri) =
  VectorElement (l .&. r) ((li .&. ri) .|. (li .&. r) .|. (l .&. ri))
interpretSingleWidthBinaryOpCode Or (VectorElement l li) (VectorElement r ri) =
  VectorElement
    (l .|. r)
    ((li .&. ri) .|. (li .&. complement r) .|. (complement l .&. ri))
interpretSingleWidthBinaryOpCode Xor (VectorElement l li) (VectorElement r ri) =
  VectorElement (l `xor` r) (li .|. ri)
interpretSingleWidthBinaryOpCode Andn (VectorElement l li) (VectorElement r ri) =
  VectorElement
    (l .&. complement r)
    ((li .&. ri) .|. (li .&. complement r) .|. (l .&. ri))
interpretSingleWidthBinaryOpCode Orn (VectorElement l li) (VectorElement r ri) =
  VectorElement
    (l .|. complement r)
    ((li .&. ri) .|. (li .&. r) .|. (complement l .&. ri))
interpretSingleWidthBinaryOpCode Xnor (VectorElement l li) (VectorElement r ri) =
  VectorElement (complement $ l `xor` r) (li .|. ri)
interpretSingleWidthBinaryOpCode Mul (VectorElement l li) (VectorElement r ri) =
  VectorElement (l * r) (combineArithInvalid li ri)
interpretSingleWidthBinaryOpCode Mulh (VectorElement l li) (VectorElement r ri) =
  VectorElement
    ( bvSelect (finiteBitSize l) (finiteBitSize l) $
        bvSext (2 * finiteBitSize l) l * bvSext (2 * finiteBitSize r) r
    )
    (combineAnyInvalid li ri)
interpretSingleWidthBinaryOpCode Mulhu (VectorElement l li) (VectorElement r ri) =
  VectorElement
    ( bvSelect (finiteBitSize l) (finiteBitSize l) $
        bvZext (2 * finiteBitSize l) l * bvZext (2 * finiteBitSize r) r
    )
    (combineAnyInvalid li ri)
interpretSingleWidthBinaryOpCode Mulhsu (VectorElement l li) (VectorElement r ri) =
  VectorElement
    ( bvSelect (finiteBitSize l) (finiteBitSize l) $
        bvSext (2 * finiteBitSize l) l * bvZext (2 * finiteBitSize r) r
    )
    (combineAnyInvalid li ri)
interpretSingleWidthBinaryOpCode Sll (VectorElement l li) (VectorElement r ri) =
  VectorElement
    (l `symShift` shiftBits r)
    ( symIte @mode
        (shiftBits ri .== 0)
        (li `symShift` shiftBits r)
        (bv (finiteBitSize l) $ -1)
    )
interpretSingleWidthBinaryOpCode Srl (VectorElement l li) (VectorElement r ri) =
  VectorElement
    (l `symShiftNegated` shiftBits r)
    ( symIte @mode
        (shiftBits ri .== 0)
        (li `symShiftNegated` shiftBits r)
        (bv (finiteBitSize l) $ -1)
    )
interpretSingleWidthBinaryOpCode Sra (VectorElement l li) (VectorElement r ri) =
  VectorElement
    (toUnsigned $ toSigned l `symShiftNegated` shiftBits (toSigned r))
    ( symIte @mode
        (shiftBits ri .== 0)
        (li `symShiftNegated` shiftBits r)
        (bv (finiteBitSize l) $ -1)
    )
interpretSingleWidthBinaryOpCode Rol (VectorElement l li) (VectorElement r ri) =
  VectorElement
    (l `symRotate` shiftBits r)
    ( symIte @mode
        (shiftBits ri .== 0)
        (li `symRotate` shiftBits r)
        (bv (finiteBitSize l) $ -1)
    )
interpretSingleWidthBinaryOpCode Ror (VectorElement l li) (VectorElement r ri) =
  VectorElement
    (l `symRotateNegated` shiftBits r)
    ( symIte @mode
        (shiftBits ri .== 0)
        (li `symRotateNegated` shiftBits r)
        (bv (finiteBitSize l) $ -1)
    )
interpretSingleWidthBinaryOpCode Min (VectorElement l li) (VectorElement r ri) =
  VectorElement
    (symIte @mode (lt l r) l r)
    (combineAnyInvalid li ri)
interpretSingleWidthBinaryOpCode Minu (VectorElement l li) (VectorElement r ri) =
  VectorElement (symIte @mode (ltu l r) l r) (combineAnyInvalid li ri)
interpretSingleWidthBinaryOpCode Max (VectorElement l li) (VectorElement r ri) =
  VectorElement
    (symIte @mode (lt l r) r l)
    (combineAnyInvalid li ri)
interpretSingleWidthBinaryOpCode Maxu (VectorElement l li) (VectorElement r ri) =
  VectorElement (symIte @mode (ltu l r) r l) (combineAnyInvalid li ri)
interpretSingleWidthBinaryOpCode Divu (VectorElement l li) (VectorElement r ri) =
  VectorElement (riscvDiv @mode l r) (combineAnyInvalid li ri)
interpretSingleWidthBinaryOpCode Div (VectorElement l li) (VectorElement r ri) =
  VectorElement
    (toUnsigned $ riscvDiv @mode (toSigned l) (toSigned r))
    (combineAnyInvalid li ri)
interpretSingleWidthBinaryOpCode Remu (VectorElement l li) (VectorElement r ri) =
  VectorElement (riscvRem @mode l r) (combineAnyInvalid li ri)
interpretSingleWidthBinaryOpCode Rem (VectorElement l li) (VectorElement r ri) =
  VectorElement
    (toUnsigned $ riscvRem @mode (toSigned l) (toSigned r))
    (combineAnyInvalid li ri)
interpretSingleWidthBinaryOpCode Slt (VectorElement l li) (VectorElement r ri) =
  VectorElement
    ( symIte
        @mode
        (lt l r)
        (bv (finiteBitSize l) 1)
        (bv (finiteBitSize l) 0)
    )
    (combineAnyInvalid li ri)
interpretSingleWidthBinaryOpCode Sltu (VectorElement l li) (VectorElement r ri) =
  VectorElement
    ( symIte
        @mode
        (ltu l r)
        (bv (finiteBitSize l) 1)
        (bv (finiteBitSize l) 0)
    )
    (combineAnyInvalid li ri)
interpretSingleWidthBinaryOpCode Seq (VectorElement l li) (VectorElement r ri) =
  VectorElement
    ( symIte
        @mode
        (l .== r)
        (bv (finiteBitSize l) 1)
        (bv (finiteBitSize l) 0)
    )
    (combineAnyInvalid li ri)
interpretSingleWidthBinaryOpCode Sne (VectorElement l li) (VectorElement r ri) =
  VectorElement
    ( symIte
        @mode
        (l ./= r)
        (bv (finiteBitSize l) 1)
        (bv (finiteBitSize l) 0)
    )
    (combineAnyInvalid li ri)
interpretSingleWidthBinaryOpCode SAddu (VectorElement l li) (VectorElement r ri) =
  VectorElement (saddu l r) (combineAnyInvalid li ri)
interpretSingleWidthBinaryOpCode SAdd (VectorElement l li) (VectorElement r ri) =
  VectorElement (sadd l r) (combineAnyInvalid li ri)
interpretSingleWidthBinaryOpCode SSubu (VectorElement l li) (VectorElement r ri) =
  VectorElement (ssubu l r) (combineAnyInvalid li ri)
interpretSingleWidthBinaryOpCode SSub (VectorElement l li) (VectorElement r ri) =
  VectorElement (ssub l r) (combineAnyInvalid li ri)
-- TODO: Check if this is correct
interpretSingleWidthBinaryOpCode CZeroNez (VectorElement l li) (VectorElement r ri) =
  VectorElement
    (symIte (r .== 0 :: GetBool mode) l (bv (finiteBitSize l) 0))
    ( symIte
        (ri ./= 0 :: GetBool mode)
        (bv (finiteBitSize l) (-1))
        (symIte (r .== 0 :: GetBool mode) li (bv (finiteBitSize l) (-1)))
    )
interpretSingleWidthBinaryOpCode CZeroEqz (VectorElement l li) (VectorElement r ri) =
  VectorElement
    (symIte (r .== 0 :: GetBool mode) (bv (finiteBitSize l) 0) l)
    ( symIte
        (ri ./= 0 :: GetBool mode)
        (bv (finiteBitSize l) (-1))
        (symIte (r .== 0 :: GetBool mode) (bv (finiteBitSize l) 0) li)
    )

instance PPrint SingleWidthIntBinaryOpCode where
  pformat Add = "add"
  pformat Sub = "sub"
  pformat RSub = "rsub"
  pformat And = "and"
  pformat Or = "or"
  pformat Xor = "xor"
  pformat Andn = "andn"
  pformat Orn = "orn"
  pformat Xnor = "xnor"
  pformat Mul = "mul"
  pformat Mulh = "mulh"
  pformat Mulhu = "mulhu"
  pformat Mulhsu = "mulhsu"
  pformat Sll = "sll"
  pformat Srl = "srl"
  pformat Sra = "sra"
  pformat Ror = "ror"
  pformat Rol = "rol"
  pformat Min = "min"
  pformat Minu = "minu"
  pformat Max = "max"
  pformat Maxu = "maxu"
  pformat Div = "div"
  pformat Divu = "divu"
  pformat Rem = "rem"
  pformat Remu = "remu"
  pformat Slt = "slt"
  pformat Sltu = "sltu"
  pformat Seq = "seq"
  pformat Sne = "sne"
  pformat SAddu = "saddu"
  pformat SAdd = "sadd"
  pformat SSubu = "ssubu"
  pformat SSub = "ssub"
  pformat CZeroNez = "czero.nez"
  pformat CZeroEqz = "czero.eqz"

{-# INLINE singleWidthIntBinaryOpCodeNames #-}
singleWidthIntBinaryOpCodeNames ::
  (IsString s) => [(s, SingleWidthIntBinaryOpCode)]
singleWidthIntBinaryOpCodeNames =
  [ ("add", Add),
    ("sub", Sub),
    ("rsub", RSub),
    ("and", And),
    ("or", Or),
    ("xor", Xor),
    ("mulhu", Mulhu),
    ("mulhsu", Mulhsu),
    ("mulh", Mulh),
    ("mul", Mul),
    ("sll", Sll),
    ("srl", Srl),
    ("sra", Sra),
    ("minu", Minu),
    ("min", Min),
    ("maxu", Maxu),
    ("max", Max),
    ("divu", Divu),
    ("div", Div),
    ("remu", Remu),
    ("rem", Rem),
    ("sltu", Sltu),
    ("slt", Slt),
    ("seq", Seq),
    ("sne", Sne),
    ("saddu", SAddu),
    ("sadd", SAdd),
    ("ssubu", SSubu),
    ("ssub", SSub)
  ]

singleWidthIntBinaryOpCodeParser ::
  (CharParser e s m) => s -> s -> m (Identity SingleWidthIntBinaryOpCode)
singleWidthIntBinaryOpCodeParser prefix =
  fmap Identity . opCodeParser singleWidthIntBinaryOpCodeNames prefix

singleWidthIntBinaryOpCodeFeature :: (HasCallStack) => SingleWidthIntBinaryOpCode -> FeatureSet
singleWidthIntBinaryOpCodeFeature op
  | op `elem` [Add, Sub, RSub, Sll, Srl] =
      linearArithFeature
  | op == Sra = linearArithFeature <> signedFeature [Signed]
  | op == Mul = multiplicationFeature
  | op `elem` [Mulh, Mulhu, Mulhsu] =
      multiplicationFeature
        <> multiplicationHighFeature
        <> ( case op of
               Mulh -> signedFeature [Signed]
               Mulhsu -> signedFeature [Signed, Unsigned]
               Mulhu -> signedFeature [Unsigned]
               _ -> error "Should not happen"
           )
  | op `elem` [Min, Max] =
      mempty {opFeatures = HS.singleton Compare} <> signedFeature [Signed]
  | op `elem` [Minu, Maxu] =
      mempty {opFeatures = HS.singleton Compare} <> signedFeature [Unsigned]
  | op `elem` [And, Or, Xor] =
      mempty {opFeatures = HS.singleton Logical}
  | op `elem` [SSub, SAdd] =
      saturateLinearArithFeature <> signedFeature [Signed]
  | op `elem` [SSubu, SAddu] =
      saturateLinearArithFeature <> signedFeature [Unsigned]
  | op == Slt =
      mempty {opFeatures = HS.singleton Compare} <> signedFeature [Signed]
  | op == Sltu =
      mempty {opFeatures = HS.singleton Compare} <> signedFeature [Unsigned]
  | otherwise = error "Not supported yet"

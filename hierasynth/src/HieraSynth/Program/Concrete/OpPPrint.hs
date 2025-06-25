{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module HieraSynth.Program.Concrete.OpPPrint
  ( VarIdMap,
    OpPPrintError (..),
    noArgumentsDescription,
    allPrefixesByTypes,
    PrefixByType (..),
    OpPPrint (..),
    prettyArguments,
    prettyResults,
  )
where

import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError))
import Data.Foldable (traverse_)
import qualified Data.HashMap.Lazy as HM
import Data.List ((\\))
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette
  ( PPrint (pformat),
    allClasses012,
    derive,
    pprintClasses,
  )
import HieraSynth.Context (ConcreteContext)
import HieraSynth.Operator.OpTyping
  ( DefaultType (DefaultType),
    OpTyping (OpTypeType, typeOp),
  )
import HieraSynth.TypeSignature (TypeSignature (TypeSignature))
import HieraSynth.Util.Pretty
  ( Doc,
    parenCommaList,
    parenCommaListIfNotSingle,
  )
import HieraSynth.Util.Show (showAsText)
import HieraSynth.VarId (ConcreteVarId)

data OpPPrintError varId op
  = IncorrectNumberOfArguments op Int Int
  | UndefinedArgument Int varId
  | IncorrectNumberOfResults op Int Int
  | RedefinedResult Int varId
  | PPrintTypingError op T.Text
  deriving (Generic, Functor)

derive [''OpPPrintError] (allClasses012 \\ pprintClasses)

instance
  (OpPPrint op, ConcreteVarId varId) =>
  PPrint (OpPPrintError varId op)
  where
  pformat (IncorrectNumberOfArguments op expectedNumArguments numArguments) =
    "Incorrect number of arguments for "
      <> pformatOp op
      <> ": expected "
      <> pformat expectedNumArguments
      <> " arguments, but got"
      <> pformat numArguments
  pformat (UndefinedArgument idx varId) =
    "The argument "
      <> pformat (toInteger varId)
      <> " at index "
      <> pformat idx
      <> " is undefined."
  pformat (IncorrectNumberOfResults op expectedNumResults numResults) =
    "Incorrect number of result for "
      <> pformatOp op
      <> ": expected "
      <> pformat expectedNumResults
      <> " results, but got"
      <> pformat numResults
      <> " results."
  pformat (RedefinedResult idx varId) =
    "The result "
      <> pformat (toInteger varId)
      <> " at index "
      <> pformat idx
      <> " is redefined."
  pformat (PPrintTypingError op err) =
    "Error while typing "
      <> pformatOp op
      <> ": "
      <> pformat err

class PrefixByType ty where
  prefixByType :: ty -> T.Text

instance PrefixByType DefaultType where
  prefixByType DefaultType = "r"

allPrefixesByTypes ::
  forall op varId.
  ( OpTyping op ConcreteContext,
    PrefixByType (OpTypeType op)
  ) =>
  op ->
  Either (OpPPrintError varId op) [T.Text]
allPrefixesByTypes op = case typeOp op of
  Right (TypeSignature _ resTypes) ->
    return $ prefixByType <$> resTypes
  Left err -> throwError $ PPrintTypingError op err

noArgumentsDescription ::
  (OpTyping op ConcreteContext) =>
  op ->
  Either (OpPPrintError varId op) [Maybe T.Text]
noArgumentsDescription op = case typeOp op of
  Right (TypeSignature argTypes _) ->
    return $ Nothing <$ argTypes
  Left err -> throwError $ PPrintTypingError op err

class OpPPrint op where
  prefixResults :: op -> Either (OpPPrintError varId op) [T.Text]
  default prefixResults ::
    ( OpTyping op ConcreteContext,
      PrefixByType (OpTypeType op)
    ) =>
    op ->
    Either (OpPPrintError varId op) [T.Text]
  prefixResults = allPrefixesByTypes
  describeArguments :: op -> Either (OpPPrintError varId op) [Maybe T.Text]
  default describeArguments ::
    (OpTyping op ConcreteContext) =>
    op ->
    Either (OpPPrintError varId op) [Maybe T.Text]
  describeArguments = noArgumentsDescription
  pformatOp :: op -> Doc ann
  default pformatOp :: (PPrint op) => op -> Doc ann
  pformatOp = pformat

type VarIdMap varId = HM.HashMap varId T.Text

prettyArguments ::
  ( ConcreteVarId varId,
    OpPPrint op
  ) =>
  op ->
  [varId] ->
  VarIdMap varId ->
  Either (OpPPrintError varId op) (Doc ann)
prettyArguments op varIds map = do
  let lookupVarId (idx, varId) =
        maybe
          (throwError $ UndefinedArgument idx varId)
          return
          (HM.lookup varId map)
  argNames <- traverse lookupVarId $ zip [0 ..] varIds
  argDescriptions <- describeArguments op
  when
    (length argNames /= length argDescriptions && not (null argDescriptions))
    $ throwError
    $ IncorrectNumberOfArguments op (length argDescriptions) (length argNames)
  let finalArgDescriptions =
        if null argDescriptions then Nothing <$ argNames else argDescriptions
  let describe argName Nothing = pformat argName
      describe argName (Just argDesc) =
        pformat argDesc <> "=" <> pformat argName
  let arpformat = zipWith describe argNames finalArgDescriptions
  return $ parenCommaList arpformat

prettyResults ::
  ( ConcreteVarId varId,
    OpPPrint op
  ) =>
  op ->
  [varId] ->
  VarIdMap varId ->
  Either (OpPPrintError varId op) (VarIdMap varId, Doc ann)
prettyResults op varIds map = do
  let ensureNotRedefined (idx, varId) =
        when (HM.member varId map) $ throwError $ RedefinedResult idx varId
  traverse_ ensureNotRedefined $ zip [0 ..] varIds
  prefixes <- prefixResults op
  when (length varIds /= length prefixes && not (null prefixes)) $
    throwError $
      IncorrectNumberOfResults op (length prefixes) (length varIds)
  let finalPrefixes = if null prefixes then "r" <$ varIds else prefixes
  let names =
        zipWith
          (\prefix varId -> prefix <> showAsText (toInteger varId))
          finalPrefixes
          varIds
  let newMap = HM.union map $ HM.fromList $ zip varIds names
  return (newMap, parenCommaListIfNotSingle $ pformat <$> names)

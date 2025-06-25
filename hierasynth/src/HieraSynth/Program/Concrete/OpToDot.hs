{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Program.Concrete.OpToDot
  ( VarIdToLabel,
    argumentsToFieldEdges,
    resultsToFieldEdges,
  )
where

import Control.Monad (when)
import Control.Monad.Error.Class (MonadError (throwError))
import Data.Foldable (traverse_)
import Data.GraphViz (DotEdge (DotEdge))
import Data.GraphViz.Attributes.Complete
  ( Attribute (HeadPort, TailPort),
    PortName (PN),
    PortPos (LabelledPort),
    RecordField (LabelledTarget),
    RecordFields,
  )
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import HieraSynth.Program.Concrete.OpPPrint
  ( OpPPrint (describeArguments, prefixResults),
    OpPPrintError
      ( IncorrectNumberOfArguments,
        IncorrectNumberOfResults,
        RedefinedResult,
        UndefinedArgument
      ),
  )
import HieraSynth.Util.Show (showAsText)
import HieraSynth.VarId (ConcreteVarId)

type VarIdToLabel varId = HM.HashMap varId (T.Text, PortName)

argumentsToFieldEdges ::
  (ConcreteVarId varId, OpPPrint op) =>
  T.Text ->
  op ->
  [varId] ->
  VarIdToLabel varId ->
  Either (OpPPrintError varId op) (RecordFields, [DotEdge T.Text])
argumentsToFieldEdges nodeId op argIds map = do
  argDescriptions <- describeArguments op
  when (length argIds /= length argDescriptions) $
    throwError $
      IncorrectNumberOfArguments op (length argDescriptions) (length argIds)
  let argPortAtPos argPos = TL.fromStrict $ "arg" <> showAsText argPos
  let buildArgLabel argPos argDesc =
        LabelledTarget (PN $ argPortAtPos argPos) $
          maybe (argPortAtPos argPos) TL.fromStrict argDesc
  let argLabel = zipWith buildArgLabel [0 ..] argDescriptions
  let lookupLabel map idx varId =
        maybe
          (throwError $ UndefinedArgument idx varId)
          return
          (HM.lookup varId map)
  argPreLabels <- traverse (uncurry $ lookupLabel map) $ zip [0 ..] argIds
  let preLabelToEdge (from, port) argPos =
        DotEdge
          from
          nodeId
          [ HeadPort $ LabelledPort (PN $ argPortAtPos argPos) Nothing,
            TailPort $ LabelledPort port Nothing
          ]
  return (argLabel, zipWith preLabelToEdge argPreLabels [0 ..])

resultsToFieldEdges ::
  ( ConcreteVarId varId,
    OpPPrint op
  ) =>
  T.Text ->
  op ->
  [varId] ->
  VarIdToLabel varId ->
  Either
    (OpPPrintError varId op)
    (VarIdToLabel varId, RecordFields)
resultsToFieldEdges nodeId op resIds map = do
  let ensureNotRedefined (idx, varId) =
        when (HM.member varId map) $ throwError $ RedefinedResult idx varId
  traverse_ ensureNotRedefined $ zip [0 ..] resIds
  prefixes <- prefixResults op
  when (length resIds /= length prefixes) $
    throwError $
      IncorrectNumberOfResults op (length prefixes) (length resIds)
  let resPortAtPos argPos = TL.fromStrict $ "res" <> showAsText argPos
  let buildResLabel argPos prefix =
        LabelledTarget (PN $ resPortAtPos argPos) $ TL.fromStrict prefix
  let resLabel = zipWith buildResLabel [0 ..] prefixes
  return
    ( foldl
        ( \m (resPos, resId) ->
            HM.insert resId (nodeId, PN $ resPortAtPos resPos) m
        )
        map
        $ zip [0 ..] resIds,
      resLabel
    )

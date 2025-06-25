{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module HieraSynth.Program.Choice.Counting
  ( SplitChoice (..),
    ValidArgument (..),
    CountNumProgs (..),
    CountNumProgsEvidence (..),
    ComponentChoicesNumResult (..),
    countNumProgsWithEvidence,
    countNumChoicesWithEvidence,
    countNumInstsWithEvidence,
    countNumComponentChoicesWithEvidence,
  )
where

import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.ST (ST, runST)
import Data.Foldable (Foldable (toList))
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef)
import Grisette (Mergeable, runFreshT, pattern Single)
import HieraSynth.Context (AngelicContext, SymbolicContext)
import HieraSynth.Operator.OpTyping (OpTyping (OpTypeType, typeOp))
import HieraSynth.Program.Choice.ChoiceTree (ChoiceTree (Branch, Leaf))
import HieraSynth.Program.Choice.ComponentBag (ComponentBag (ComponentBag))
import qualified HieraSynth.Program.Concrete as Concrete
import HieraSynth.Program.SymbolTable (SymbolTable (SymbolTable))
import HieraSynth.TypeSignature (TypeSignature (TypeSignature))

class SplitChoice sketchSpec where
  splitChoice :: sketchSpec -> [sketchSpec]

class ValidArgument ty where
  validArgument ::
    -- | The type of the argument
    ty ->
    -- | The type of the value to feed into the argument
    ty ->
    -- | Whether the value is valid for the argument
    Bool

splitIntoTypes ::
  forall sketchSpec.
  ( OpTyping sketchSpec AngelicContext,
    Mergeable (OpTypeType sketchSpec),
    SplitChoice sketchSpec
  ) =>
  sketchSpec ->
  [TypeSignature (OpTypeType sketchSpec)]
splitIntoTypes op = types
  where
    allOps = splitChoice op
    types' :: [SymbolicContext (TypeSignature (OpTypeType sketchSpec))]
    types' = map (flip runFreshT "tmp" . typeOp) allOps
    types =
      map
        ( \case
            ExceptT (Single (Right t)) -> t
            ExceptT (Single (Left r)) -> error $ "Type inference failed: " <> show r
            _ -> error "Type inference failed"
        )
        types'

choiceTreeSplitAsSingleChoices ::
  (SplitChoice sketchSpec) => ChoiceTree sketchSpec -> [sketchSpec]
choiceTreeSplitAsSingleChoices (Leaf ops) = concatMap splitChoice ops
choiceTreeSplitAsSingleChoices (Branch _ subLists) =
  concatMap choiceTreeSplitAsSingleChoices subLists

choiceTreeSplitIntoTypes ::
  forall sketchSpec.
  ( OpTyping sketchSpec AngelicContext,
    Mergeable (OpTypeType sketchSpec),
    SplitChoice sketchSpec
  ) =>
  ChoiceTree sketchSpec ->
  [TypeSignature (OpTypeType sketchSpec)]
choiceTreeSplitIntoTypes (Leaf ops) = concatMap splitIntoTypes ops
choiceTreeSplitIntoTypes (Branch _ subLists) =
  concatMap choiceTreeSplitIntoTypes subLists

choiceTreeSplitIntoTypesMap ::
  forall sketchSpec.
  ( OpTyping sketchSpec AngelicContext,
    Mergeable (OpTypeType sketchSpec),
    Eq (OpTypeType sketchSpec),
    Hashable (OpTypeType sketchSpec),
    SplitChoice sketchSpec
  ) =>
  ChoiceTree sketchSpec ->
  HM.HashMap (TypeSignature (OpTypeType sketchSpec)) Integer
choiceTreeSplitIntoTypesMap choiceTree =
  HM.fromListWith (+) $ map (,1) $ choiceTreeSplitIntoTypes choiceTree

data ComponentChoicesNumResult = ComponentChoicesNumResult
  { numComponents :: Integer,
    numTotalChoices :: Integer
  }

instance Semigroup ComponentChoicesNumResult where
  ComponentChoicesNumResult n1 c1 <> ComponentChoicesNumResult n2 c2 =
    ComponentChoicesNumResult (n1 + n2) (c1 + c2)

instance Monoid ComponentChoicesNumResult where
  mempty = ComponentChoicesNumResult 0 0

class CountNumProgs sketchSpec where
  countNumChoices :: sketchSpec -> Integer
  countNumProgs :: sketchSpec -> Integer
  countNumComponentChoices :: sketchSpec -> ComponentChoicesNumResult
  countNumInsts :: sketchSpec -> Integer

instance (CountNumProgs prog) => CountNumProgs (SymbolTable prog) where
  countNumChoices (SymbolTable tbl) = product $ countNumChoices . snd <$> tbl
  countNumProgs (SymbolTable tbl) = product $ countNumProgs . snd <$> tbl
  countNumComponentChoices (SymbolTable tbl) =
    mconcat $ countNumComponentChoices . snd <$> tbl
  countNumInsts (SymbolTable tbl) = sum $ countNumInsts . snd <$> tbl

instance
  (SplitChoice sketchSpec) =>
  CountNumProgs
    (Concrete.Prog (ChoiceTree sketchSpec) conVarId conTy)
  where
  countNumChoices (Concrete.Prog _ stmts _) =
    product $
      map
        (fromIntegral . length . choiceTreeSplitAsSingleChoices . Concrete.stmtOp)
        stmts
  countNumProgs = countNumChoices
  countNumComponentChoices _ = mempty
  countNumInsts (Concrete.Prog _ stmts _) = fromIntegral $ length stmts

data CountNumProgsEvidence sketchSpec where
  CountNumProgsEvidence ::
    (CountNumProgs sketchSpec) =>
    CountNumProgsEvidence sketchSpec

countNumProgsWithEvidence ::
  CountNumProgsEvidence sketchSpec ->
  sketchSpec ->
  Integer
countNumProgsWithEvidence CountNumProgsEvidence = countNumProgs

countNumChoicesWithEvidence ::
  CountNumProgsEvidence sketchSpec ->
  sketchSpec ->
  Integer
countNumChoicesWithEvidence CountNumProgsEvidence = countNumChoices

countNumComponentChoicesWithEvidence ::
  CountNumProgsEvidence sketchSpec ->
  sketchSpec ->
  ComponentChoicesNumResult
countNumComponentChoicesWithEvidence CountNumProgsEvidence =
  countNumComponentChoices

countNumInstsWithEvidence ::
  CountNumProgsEvidence sketchSpec ->
  sketchSpec ->
  Integer
countNumInstsWithEvidence CountNumProgsEvidence = countNumInsts

instance
  ( SplitChoice sketchSpec,
    OpTyping sketchSpec AngelicContext,
    ty0 ~ OpTypeType sketchSpec,
    Eq ty0,
    Hashable ty0,
    Mergeable ty0,
    ValidArgument ty0
  ) =>
  CountNumProgs (ComponentBag sketchSpec ty0)
  where
  countNumComponentChoices (ComponentBag _ components _) =
    let numComponents = fromIntegral $ sum $ snd <$> components
        numTotalChoices =
          fromIntegral $
            sum $
              ( \(tree, num) ->
                  num * length (choiceTreeSplitAsSingleChoices tree)
              )
                <$> components
     in ComponentChoicesNumResult numComponents numTotalChoices
  countNumChoices (ComponentBag _ components _) =
    product $
      map
        ( \(opList, n) ->
            fromIntegral $
              length (choiceTreeSplitAsSingleChoices opList) ^ n
        )
        components
  countNumProgs (ComponentBag argTypes components resTypes) = runST operation
    where
      operation :: forall s. ST s Integer
      operation = do
        let initialTypes :: HM.HashMap ty0 Integer
            initialTypes = HM.fromListWith (+) $ map (,1) argTypes
            componentNums = snd <$> components
            componentTypes = choiceTreeSplitIntoTypesMap . fst <$> components
        st :: STRef s (HM.HashMap [Int] (HM.HashMap (HM.HashMap ty0 Integer) Integer)) <-
          newSTRef $
            HM.singleton (0 <$ componentNums) $
              HM.singleton initialTypes 1
        let go :: [Int] -> ST s (HM.HashMap (HM.HashMap ty0 Integer) Integer)
            go remainingComponentNums = do
              cache <- readSTRef st
              case HM.lookup remainingComponentNums cache of
                Just x -> return x
                Nothing -> do
                  let idxs = [0 .. length remainingComponentNums - 1]
                  ress <- mapM (`goIdx` remainingComponentNums) idxs
                  let res = foldl1 (HM.unionWith (+)) ress
                  modifySTRef' st $ HM.insert remainingComponentNums res
                  return res
            goIdx :: Int -> [Int] -> ST s (HM.HashMap (HM.HashMap ty0 Integer) Integer)
            goIdx idx remainingComponentNums
              | remainingComponentNums !! idx == 0 = return HM.empty
              | otherwise = do
                  let newRemainingComponentNums =
                        take idx remainingComponentNums
                          ++ [remainingComponentNums !! idx - 1]
                          ++ drop (idx + 1) remainingComponentNums
                  l <- HM.toList <$> go newRemainingComponentNums
                  let types = HM.toList $ componentTypes !! idx
                  let r = do
                        (remainingType, prevNum) <- l
                        (opType, opNum) <- types
                        return $ goType remainingType prevNum opType opNum
                  return $ HM.fromListWith (+) r
            argChoices ::
              HM.HashMap ty0 Integer ->
              [ty0] ->
              Integer
            argChoices remainingTypes argTypes =
              let remainingTypesList = HM.toList remainingTypes
               in product $
                    map
                      ( \arg ->
                          sum
                            ( map
                                ( \remaining ->
                                    if validArgument arg (fst remaining)
                                      then snd remaining
                                      else 0
                                )
                                remainingTypesList
                            )
                      )
                      argTypes
            allArgChoices ::
              HM.HashMap (HM.HashMap ty0 Integer) Integer ->
              [ty0] ->
              Integer
            allArgChoices remainingTypes types =
              let remainingTypesList = HM.toList remainingTypes
               in sum $
                    map
                      ( \(remainingType, prevNum) ->
                          prevNum * argChoices remainingType types
                      )
                      remainingTypesList
            goType ::
              HM.HashMap ty0 Integer ->
              Integer ->
              TypeSignature ty0 ->
              Integer ->
              (HM.HashMap ty0 Integer, Integer)
            goType remainingTypes prevNum (TypeSignature argTypes retTypes) num = do
              let newRetType =
                    foldl (flip (HM.alter (Just . maybe 1 (+ 1)))) remainingTypes retTypes
              (newRetType, prevNum * num * argChoices remainingTypes argTypes)
        go componentNums
        rst <- readSTRef st
        let result = sum $ (`allArgChoices` resTypes) <$> toList rst
        return result
  countNumInsts (ComponentBag _ components _) =
    fromIntegral $ sum $ map snd components

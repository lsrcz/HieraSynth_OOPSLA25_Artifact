{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HieraSynth.Program.ComponentSketch.Program
  ( Stmt (..),
    ProgArg (..),
    ProgRes (..),
    Prog (..),
  )
where

import Control.Monad (join)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.State
  ( MonadState (get, put),
    MonadTrans (lift),
    State,
    StateT,
    evalState,
    gets,
  )
import Data.Bifunctor (Bifunctor (first))
import Data.Data (Proxy (Proxy))
import qualified Data.HashMap.Lazy as M
import Data.List (sortOn, (\\))
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grisette
  ( DeriveConfig (useNoStrategy),
    EvalSym,
    ITEOp (symIte),
    LogicalOp (false, symImplies, symNot, true, (.&&), (.||)),
    Mergeable,
    Mergeable3,
    MonadUnion,
    Solvable (con),
    SymBool,
    SymEq (symDistinct, (./=), (.==)),
    SymOrd ((.<), (.<=), (.>), (.>=)),
    ToCon (toCon),
    ToSym (toSym),
    Union,
    basicClasses01,
    basicClasses012,
    derive,
    deriveWith,
    mrgFmap,
    mrgIf,
    mrgSequence_,
    mrgTraverse_,
    ordClasses,
    simpleMerge,
    symAll,
    symAnd,
    symAny,
    symAssertWith,
    unifiedSymOrdClasses,
  )
import Grisette.Lib.Control.Monad (mrgReturn)
import Grisette.Lib.Control.Monad.Except (mrgThrowError)
import Grisette.Lib.Control.Monad.State.Class (mrgModify)
import Grisette.Lib.Control.Monad.Trans.State (mrgEvalStateT)
import HieraSynth.Context (MonadAngelicContext, MonadContext)
import HieraSynth.Operator.OpReachableSymbols
  ( OpReachableSymbols (opReachableSymbols),
  )
import HieraSynth.Operator.OpSemantics (OpSemantics (applyOp))
import HieraSynth.Operator.OpTyping (OpTyping (OpTypeType, typeAndSplitOp, typeOp))
import HieraSynth.Program.ComponentSketch.GenIntermediate
  ( GenIntermediate,
    Intermediates (Intermediates),
    genIntermediates,
    genOpIntermediates,
  )
import HieraSynth.Program.ComponentSketch.SymmetryReduction
  ( OpSymmetryReduction (opCommutativeArgPos, opUnreorderable),
  )
import qualified HieraSynth.Program.Concrete as Concrete
import HieraSynth.Program.CostModel.PerStmtCostModel
  ( OpCost (opCost),
    PerStmtCostObj (PerStmtCostObj),
  )
import HieraSynth.Program.ProgCost (ProgCost (progCost))
import HieraSynth.Program.ProgSemantics
  ( EvaledSymbolTable,
    ProgSemantics (runProg),
  )
import HieraSynth.Program.ProgTyping (ProgTyping (typeProg))
import HieraSynth.Program.ProgUtil
  ( ProgUtil
      ( ProgOpType,
        ProgStmtType,
        ProgTypeType,
        ProgVarIdType
      ),
    ProgUtilImpl
      ( getProgArgIds,
        getProgNumStmts,
        getProgResIds,
        getProgStmtAtIdx
      ),
    StmtUtil
      ( StmtOpType,
        StmtVarIdType
      ),
    StmtUtilImpl
      ( getStmtArgIds,
        getStmtDisabled,
        getStmtOp,
        getStmtResIds
      ),
  )
import HieraSynth.Program.SymbolTable
  ( ProgReachableSymbols (progReachableSymbols),
  )
import HieraSynth.TypeSignature
  ( TypeSignature (TypeSignature),
  )
import HieraSynth.Util.Show (showAsText)
import HieraSynth.VarId (ConcreteVarId, RelatedVarId, SymbolicVarId)

data Stmt op symVarId = Stmt
  { stmtOp :: op,
    stmtArgIds :: [symVarId],
    stmtArgNum :: symVarId,
    stmtResIds :: [symVarId],
    stmtResNum :: symVarId,
    stmtDisabled :: SymBool,
    stmtMustBeAfter :: [symVarId]
  }
  deriving (Generic)

data ProgArg ty = ProgArg
  { progArgName :: T.Text,
    progArgType :: ty
  }
  deriving (Generic)

data ProgRes symVarId ty = ProgRes
  { progResId :: symVarId,
    progResType :: ty
  }
  deriving (Generic)

data Prog op symVarId ty = Prog
  { progArgList :: [ProgArg ty],
    progStmtList :: [Stmt op symVarId],
    progResList :: [ProgRes symVarId ty]
  }
  deriving (Generic)

deriveWith
  mempty {useNoStrategy = True}
  [''ProgArg]
  (basicClasses01 \\ (ordClasses ++ unifiedSymOrdClasses))

deriveWith
  mempty {useNoStrategy = True}
  [''Stmt, ''ProgRes, ''Prog]
  (basicClasses012 \\ (ordClasses ++ unifiedSymOrdClasses))

deriveWith
  mempty {useNoStrategy = True}
  [''Prog]
  [''Mergeable3]

instance
  ( ToSym conOp symOp,
    ConcreteVarId conVarId,
    SymbolicVarId symVarId,
    Mergeable symOp,
    ToSym conTy symTy
  ) =>
  ToSym (Concrete.Prog conOp conVarId conTy) (Prog symOp symVarId symTy)
  where
  toSym (Concrete.Prog argList stmtList resList) =
    flip evalState initialMapping $ do
      stmts <- traverse toSymStmt stmtList
      res <- traverse toSymRes resList
      return $ Prog componentArgList stmts res
    where
      componentArgList =
        (\(Concrete.ProgArg name _ ty) -> ProgArg name (toSym ty)) <$> argList
      initialMapping :: M.HashMap conVarId symVarId
      initialMapping =
        M.fromList $
          first Concrete.progArgId <$> zip argList (fromIntegral <$> [0 ..])
      lookupId :: conVarId -> State (M.HashMap conVarId symVarId) symVarId
      lookupId varId = gets (fromMaybe (-1) . M.lookup varId)
      addId :: conVarId -> State (M.HashMap conVarId symVarId) symVarId
      addId conVarId = do
        m <- get
        let newId = fromIntegral $ M.size m
        put $ M.insert conVarId newId m
        return newId
      toSymStmt ::
        Concrete.Stmt conOp conVarId ->
        State (M.HashMap conVarId symVarId) (Stmt symOp symVarId)
      toSymStmt (Concrete.Stmt op argIds resIds) = do
        let symOp = toSym op
        conArgIds <- traverse lookupId argIds
        conResIds <- traverse addId resIds
        return $
          Stmt
            { stmtOp = symOp,
              stmtArgIds = conArgIds,
              stmtArgNum = fromIntegral $ length conArgIds,
              stmtResIds = conResIds,
              stmtResNum = fromIntegral $ length conResIds,
              stmtDisabled = con False,
              stmtMustBeAfter = []
            }
      toSymRes ::
        Concrete.ProgRes conVarId conTy ->
        State (M.HashMap conVarId symVarId) (ProgRes symVarId symTy)
      toSymRes (Concrete.ProgRes conId ty) = do
        symId <- lookupId conId
        let symTy = toSym ty
        return $ ProgRes symId symTy

instance
  ( Mergeable symOp,
    ToCon symOp conOp,
    RelatedVarId conVarId symVarId,
    ToCon symTy conTy
  ) =>
  ToCon (Prog symOp symVarId symTy) (Concrete.Prog conOp conVarId conTy)
  where
  toCon (Prog argList stmtList resList) = do
    conArgList <-
      traverse
        ( \(ProgArg name ty, varId) ->
            Concrete.ProgArg name varId <$> toCon ty
        )
        $ zip argList [0 ..]
    let toConStmt (Stmt op argIds argNum resIds resNum disabled _) = do
          disabled <- toCon disabled
          if disabled
            then return []
            else do
              conOp <- toCon op
              conArgIds <- toCon argIds
              conArgNum :: conVarId <- toCon argNum
              conResIds <- toCon resIds
              conResNum :: conVarId <- toCon resNum
              return
                [ Concrete.Stmt
                    conOp
                    (take (fromIntegral conArgNum) conArgIds)
                    (take (fromIntegral conResNum) conResIds)
                ]
    conStmts <- join <$> traverse toConStmt stmtList
    conResList <-
      traverse
        ( \(ProgRes resId ty) -> do
            conResId <- toCon resId
            conTy <- toCon ty
            return $
              Concrete.ProgRes
                { Concrete.progResId = conResId,
                  Concrete.progResType = conTy
                }
        )
        resList
    return $
      Concrete.Prog
        conArgList
        (sortOn (listToMaybe . Concrete.stmtResIds) conStmts)
        conResList

data IdValPair symVarId val = IdValPair SymBool symVarId (Union (Maybe val))

data CollectedDefUse symVarId val = CollectedDefUse
  { collectedDef :: [IdValPair symVarId val],
    collectedUse :: [IdValPair symVarId val]
  }

derive
  [''IdValPair, ''CollectedDefUse]
  [''Show, ''Eq, ''EvalSym, ''Mergeable]

addDefs ::
  ( MonadState (CollectedDefUse symVarId val) ctx,
    MonadUnion ctx,
    SymbolicVarId symVarId,
    Mergeable val
  ) =>
  [IdValPair symVarId val] ->
  ctx ()
addDefs def = mrgModify $ \s -> s {collectedDef = def ++ collectedDef s}

addUses ::
  ( MonadState (CollectedDefUse symVarId val) ctx,
    MonadUnion ctx,
    SymbolicVarId symVarId,
    Mergeable val
  ) =>
  [IdValPair symVarId val] ->
  ctx ()
addUses use = mrgModify $ \s -> s {collectedUse = use ++ collectedUse s}

addProgArgs ::
  ( MonadState (CollectedDefUse symVarId val) ctx,
    MonadUnion ctx,
    SymbolicVarId symVarId,
    Mergeable val
  ) =>
  [val] ->
  ctx ()
addProgArgs args =
  addDefs
    ( zipWith
        (\argId val -> IdValPair (con False) argId (mrgReturn $ Just val))
        (fromIntegral <$> [0 ..])
        args
    )

genProgResVals ::
  ( SymbolicVarId symVarId,
    Mergeable val,
    GenIntermediate sem ty val,
    MonadAngelicContext ctx
  ) =>
  sem ->
  [ProgRes symVarId ty] ->
  StateT (CollectedDefUse symVarId val) ctx [val]
genProgResVals sem resList = do
  resVals <- lift $ genIntermediates sem (progResType <$> resList)
  addUses
    ( zipWith
        (IdValPair (con False))
        (progResId <$> resList)
        (mrgReturn . Just <$> resVals)
    )
  return resVals

inBound :: (SymbolicVarId symVarId) => Int -> symVarId -> SymBool
inBound bound val =
  symInBound (fromIntegral bound) val
    .&& symAny (.== val) (fromIntegral <$> [0 .. bound - 1])

symInBound :: (SymbolicVarId symVarId) => symVarId -> symVarId -> SymBool
symInBound bound val = (0 .<= val) .&& (val .< bound)

constrainStmtStructure ::
  forall ctx op symVarId.
  ( SymbolicVarId symVarId,
    OpTyping op ctx,
    Mergeable op,
    MonadAngelicContext ctx
  ) =>
  Int ->
  Stmt op symVarId ->
  ctx ()
constrainStmtStructure
  idBound
  (Stmt opUnion argIds argNum resIds resNum _ mustBeAfters) = do
    symAssertWith "Out-of-bound statement results." $
      symAll (inBound idBound) resIds

    symAssertWith "result not canonical." $
      symAll (\(i, isucc) -> isucc .== i + 1) $
        zip resIds (tail resIds)

    mrgTraverse_
      ( \(res, mustBeAfter) ->
          symAssertWith "Failed must be after constraint." $ res .> mustBeAfter
      )
      [(res, mustBeAfter) | res <- resIds, mustBeAfter <- mustBeAfters]

    TypeSignature argTypes resTypes <- typeOp opUnion

    let usedArgIds = take (length argTypes) argIds
    let usedResIds = take (length resTypes) resIds

    symAssertWith "Variable is undefined." $
      symAll (\resId -> symAll (symInBound resId) usedArgIds) usedResIds

    symAssertWith "Incorrect number of arguments." $
      argNum .== fromIntegral (length argTypes)
    symAssertWith "Incorrect number of results." $
      resNum .== fromIntegral (length resTypes)

constrainStmt ::
  forall sem ctx op symVarId val.
  ( SymbolicVarId symVarId,
    GenIntermediate sem (OpTypeType op) val,
    OpSemantics sem op val ctx,
    OpTyping op ctx,
    Mergeable op,
    Mergeable (OpTypeType op),
    SymEq val,
    MonadAngelicContext ctx
  ) =>
  sem ->
  EvaledSymbolTable val ctx ->
  Stmt op symVarId ->
  StateT (CollectedDefUse symVarId val) ctx ()
constrainStmt
  sem
  table
  (Stmt opUnion argIds _ resIds _ disabled _) = do
    (op, signature) <- lift $ typeAndSplitOp opUnion
    Intermediates argVals resVals <-
      lift $ genOpIntermediates (Proxy @(OpTypeType op)) sem signature
    mrgIf disabled (return ()) $ do
      computedResVals <- lift $ applyOp sem table op argVals
      symAssertWith "Incorrect results." $ resVals .== computedResVals

    let getIdValPairs _ [] [] = mrgReturn []
        getIdValPairs disabled (i : is) [] =
          mrgFmap (IdValPair (con True) i (mrgReturn Nothing) :) $
            getIdValPairs disabled is []
        getIdValPairs _ [] _ =
          mrgThrowError $
            "The limit for is smaller than the actual number. "
              <> "Check your SymOpLimits."
        getIdValPairs disabled (i : is) (v : vs) =
          mrgFmap (IdValPair disabled i (mrgReturn . Just $ v) :) $
            getIdValPairs disabled is vs
    addUses =<< getIdValPairs disabled argIds argVals
    addDefs =<< getIdValPairs disabled resIds resVals

connected ::
  ( MonadUnion ctx,
    MonadContext ctx,
    SymbolicVarId symVarId,
    SymEq val,
    Mergeable val
  ) =>
  StateT (CollectedDefUse symVarId val) ctx ()
connected = do
  CollectedDefUse def use <- get
  mrgSequence_ $
    [ do
        symAssertWith "Using disabled values" $
          symImplies (defDisabled .&& defId .== useId) useDisabled
        symAssertWith "Def/use with same ID does not have the same value." $
          useDisabled .|| symImplies (defId .== useId) (defVal .== useVal)
    | IdValPair defDisabled defId defVal <- def,
      IdValPair useDisabled useId useVal <- use
    ]

progStructureConstraint ::
  ( SymbolicVarId symVarId,
    MonadAngelicContext ctx,
    OpSymmetryReduction op,
    Mergeable op,
    OpTyping op ctx,
    SymEq op
  ) =>
  Prog op symVarId ty ->
  [val] ->
  ctx ()
progStructureConstraint prog@(Prog arg stmts ret) inputs = do
  symAssertWith "non-canonical" $ canonicalOrderConstraint prog
  symAssertWith "commutative reduction" $ progCommutativeConstraint prog
  symAssertWith "no same op with same arg" $ noSameOpWithSameArg prog

  let bound = length inputs + sum (length . stmtResIds <$> stmts)
  symAssertWith
    ( "Expected "
        <> showAsText (length arg)
        <> " arguments, but got "
        <> showAsText (length inputs)
        <> " arguments."
    )
    $ length inputs .== length arg
  symAssertWith "Variable is undefined." $
    symAll (inBound bound) $
      progResId <$> ret

  mrgTraverse_ (constrainStmtStructure bound) stmts

  let allDefIds =
        take (length arg) (fromIntegral <$> [0 ..])
          ++ concatMap stmtResIds stmts
  symAssertWith "Variable is already defined." $ symDistinct allDefIds

instance
  {-# OVERLAPPABLE #-}
  ( SymbolicVarId symVarId,
    GenIntermediate sem ty val,
    OpSemantics sem op val ctx,
    OpTyping op ctx,
    Mergeable op,
    SymEq val,
    MonadAngelicContext ctx,
    Mergeable ty,
    OpTypeType op ~ ty,
    OpSymmetryReduction op,
    SymEq op
  ) =>
  ProgSemantics sem (Prog op symVarId ty) val ctx
  where
  runProg sem table prog@(Prog _ stmts ret) inputs = do
    progStructureConstraint prog inputs
    flip mrgEvalStateT (CollectedDefUse [] []) $ do
      addProgArgs inputs
      mrgTraverse_ (constrainStmt sem table) stmts
      resVals <- genProgResVals sem ret
      connected
      mrgReturn resVals

instance
  (Mergeable ty, SymbolicVarId varId) =>
  ProgTyping (Prog op varId ty)
  where
  typeProg prog =
    TypeSignature
      (progArgType <$> progArgList prog)
      (progResType <$> progResList prog)

-- instance ProgNaming (Prog op varId ty) where
--   nameProg = progName

instance StmtUtilImpl (Stmt op varId) op varId where
  getStmtArgIds = stmtArgIds
  getStmtResIds = stmtResIds
  getStmtOp = stmtOp
  getStmtDisabled = stmtDisabled

instance StmtUtil (Stmt op varId) where
  type StmtOpType (Stmt op varId) = op
  type StmtVarIdType (Stmt op varId) = varId

instance
  (SymbolicVarId varId) =>
  ProgUtilImpl (Prog op varId ty) op (Stmt op varId) varId
  where
  getProgArgIds prog = fst <$> zip (fromIntegral <$> [0 ..]) (progArgList prog)
  getProgResIds = map progResId . progResList
  getProgNumStmts = length . progStmtList
  getProgStmtAtIdx prog idx
    | idx >= getProgNumStmts prog = throwError "Statement index out of bounds."
    | otherwise = return $ progStmtList prog !! idx

instance (SymbolicVarId varId) => ProgUtil (Prog op varId ty) where
  type ProgTypeType (Prog op varId ty) = ty
  type ProgStmtType (Prog op varId ty) = Stmt op varId
  type ProgVarIdType (Prog op varId ty) = varId
  type ProgOpType (Prog op varId ty) = op

instance
  ( MonadContext ctx,
    MonadUnion ctx,
    OpCost opCostObj op cost ctx,
    Num cost,
    Mergeable cost
  ) =>
  ProgCost (PerStmtCostObj opCostObj) (Prog op varId ty) cost ctx
  where
  progCost (PerStmtCostObj obj) table prog = do
    stmtCosts <-
      traverse
        ( \stmt ->
            mrgIf (stmtDisabled stmt) (return 0) $ opCost obj table $ stmtOp stmt
        )
        (progStmtList prog)
    return $ sum stmtCosts

instance (OpReachableSymbols op) => ProgReachableSymbols (Prog op varId ty) where
  progReachableSymbols =
    mconcat . fmap (opReachableSymbols . stmtOp) . progStmtList

stmtUnreorderable ::
  (OpSymmetryReduction op, SymbolicVarId symVarId) =>
  Prog op symVarId ty ->
  Int ->
  Int ->
  SymBool
stmtUnreorderable prog i j
  | i == j = true
  | i >= length (progStmtList prog) = true
  | j >= length (progStmtList prog) = true
  | otherwise =
      let firstStmt = progStmtList prog !! i
          secondStmt = progStmtList prog !! j
          opRes = opUnreorderable (stmtOp firstStmt) (stmtOp secondStmt)
       in symIte
            (stmtDisabled firstStmt .|| stmtDisabled secondStmt)
            false
            (opRes .|| statementsDirectDep firstStmt secondStmt)

canonicalOrderConstraint ::
  (OpSymmetryReduction op, SymbolicVarId symVarId) =>
  Prog op symVarId ty ->
  SymBool
canonicalOrderConstraint prog =
  symAnd $ cond <$> [0 .. n - 1] <*> [0 .. n - 1]
  where
    stmts = progStmtList prog
    n = length stmts
    cond i j
      | i > j =
          statementsAdjacent (stmts !! i) (stmts !! j)
            `symImplies` stmtUnreorderable prog i j
      | otherwise = true

statementsDirectDep ::
  (SymbolicVarId symVarId) =>
  Stmt op symVarId ->
  Stmt op symVarId ->
  SymBool
statementsDirectDep src dest =
  symAny
    (uncurry (.==))
    [ (srcResId, destArgId)
    | srcResId <- stmtResIds src,
      destArgId <- stmtArgIds dest
    ]
    .&& symNot (stmtDisabled src)
    .&& symNot (stmtDisabled dest)

statementsAdjacent ::
  (SymbolicVarId symVarId) =>
  Stmt op symVarId ->
  Stmt op symVarId ->
  SymBool
statementsAdjacent first second =
  last (stmtResIds first) + 1 .== head (stmtResIds second)

statementCommutativeConstraint ::
  (OpSymmetryReduction op, SymbolicVarId symVarId) =>
  Stmt op symVarId ->
  SymBool
statementCommutativeConstraint (Stmt op argIds _ _ _ _ _) = simpleMerge $ do
  pos <- opCommutativeArgPos op
  return $ go pos
  where
    increasing [] = true
    increasing [_] = true
    increasing (a : b : xs) = a .<= b .&& increasing (b : xs)
    go [] = true
    go (x : xs) =
      let ids = (argIds !!) <$> x in increasing ids .&& go xs

progCommutativeConstraint ::
  (OpSymmetryReduction op, SymbolicVarId symVarId) =>
  Prog op symVarId ty ->
  SymBool
progCommutativeConstraint prog =
  symAnd $ statementCommutativeConstraint <$> progStmtList prog

noSameOpWithSameArg ::
  (OpSymmetryReduction op, SymEq op, SymbolicVarId symVarId) =>
  Prog op symVarId ty ->
  SymBool
noSameOpWithSameArg prog = do
  let stmts = progStmtList prog
  let pairs =
        [ (stmts !! a, stmts !! b)
        | a <- [0 .. length stmts - 1],
          b <- [0 .. a - 1]
        ]
  let constraintSingle stmt1 stmt2 = do
        let argIds1 = stmtArgIds stmt1
        let argIds2 = stmtArgIds stmt2
        let argNum1 = stmtArgNum stmt1
        let argNum2 = stmtArgNum stmt2
        let eqNum = argNum1 .== argNum2
        let idxes = fromIntegral <$> [0 ..]
        let eqId =
              symAnd $
                zipWith3 (\i a b -> a .== b .|| i .>= argNum1) idxes argIds1 argIds2
        ( eqNum
            .&& eqId
            .&& symNot (stmtDisabled stmt1)
            .&& symNot (stmtDisabled stmt2)
          )
          `symImplies` (stmtOp stmt1 ./= stmtOp stmt2)
  symAnd (uncurry constraintSingle <$> pairs)

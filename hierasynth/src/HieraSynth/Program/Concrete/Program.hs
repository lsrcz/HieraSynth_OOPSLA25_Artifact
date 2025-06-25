{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Do not use plain concrete programs on operators that may generate multiple
-- results that cannot be merged into a single result, or there can be
-- significant performance issues.
--
-- Use the `ProgMayMultiPath` wrapper to allow multiple results. However, note
-- that it is generally not a good idea to use non-simply-mergeable types as
-- results, as it can lead to exponential blowup in the number of results.
module HieraSynth.Program.Concrete.Program
  ( Stmt (..),
    ProgArg (..),
    ProgRes (..),
    Prog (..),
    ProgPPrintError (..),
    prettyStmt,
    prettyProg,
    stmtToDotNode,
    progToDotSubGraph,
    ProgPPrint (..),
    ProgToDot (..),
    eliminateDeadCode,
    eliminateProgTableDeadCode,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.State
  ( MonadState (get, put),
    MonadTrans (lift),
    StateT,
    evalStateT,
  )
import Data.Bifunctor (Bifunctor (second))
import Data.Foldable (traverse_)
import Data.GraphViz
  ( DotEdge (DotEdge),
    DotNode (DotNode),
    DotStatements (DotStmts, attrStmts, edgeStmts, nodeStmts, subGraphs),
    DotSubGraph (DotSG, isCluster, subGraphID, subGraphStmts),
    GlobalAttributes (GraphAttrs),
    GraphID (Str),
    Shape (Record),
    shape,
  )
import Data.GraphViz.Attributes (textLabel)
import Data.GraphViz.Attributes.Complete
  ( Attribute (HeadPort, Label, TailPort),
    Label (RecordLabel),
    PortName (PN),
    PortPos (LabelledPort),
    RecordField (FieldLabel, FlipFields, LabelledTarget),
  )
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.List ((\\))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.Generics (Generic)
import Grisette
  ( DeriveConfig (useNoStrategy),
    GenSymSimple (simpleFresh),
    Mergeable,
    Mergeable3,
    PPrint (pformat),
    ToSym (toSym),
    allClasses012,
    derive,
    deriveWith,
    pprintClasses,
    tryMerge,
  )
import HieraSynth.Context (MonadContext)
import HieraSynth.Operator.OpParser (OpParser (opParser))
import HieraSynth.Operator.OpReachableSymbols
  ( OpReachableSymbols (opReachableSymbols),
  )
import HieraSynth.Operator.OpSemantics (OpSemantics (applyOp))
import HieraSynth.Operator.OpTyping (OpTyping (OpTypeType))
import HieraSynth.Program.Choice.Split
  ( LowestSeqNum (lowestSeqNum),
    PartitionSpec (partitionSpec),
    lowestSeqNumList,
    partitionSpecList,
  )
import HieraSynth.Program.Concrete.OpPPrint
  ( OpPPrint (pformatOp),
    OpPPrintError,
    VarIdMap,
    prettyArguments,
    prettyResults,
  )
import HieraSynth.Program.Concrete.OpToDot
  ( VarIdToLabel,
    argumentsToFieldEdges,
    resultsToFieldEdges,
  )
import HieraSynth.Program.CostModel.PerStmtCostModel
  ( OpCost (opCost),
    PerStmtCostObj (PerStmtCostObj),
  )
import HieraSynth.Program.ProgCost (ProgCost (progCost))
import HieraSynth.Program.ProgPPrint
  ( ProgPPrint (pformatProg),
  )
import HieraSynth.Program.ProgParser (ProgParser (progParser))
import HieraSynth.Program.ProgSemantics (ProgSemantics (runProg))
import HieraSynth.Program.ProgToDot
  ( ProgToDot (toDotProg),
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
    SymbolTable (SymbolTable),
  )
import HieraSynth.Type.TypeParser (TypeParser (typeParser))
import HieraSynth.TypeSignature
  ( TypeSignature (TypeSignature),
  )
import HieraSynth.Util.Parser
  ( CharParser,
    colon,
    identifier,
    parenCommaSep,
    parenCommaSepOrSingleton,
    symbol,
  )
import HieraSynth.Util.Pretty
  ( Doc,
    concatWith,
    hardline,
    nest,
    parenCommaList,
    parenCommaListIfNotSingle,
    renderDoc,
    (<+>),
  )
import HieraSynth.Util.Show (showAsText)
import HieraSynth.VarId (ConcreteVarId)
import Text.Megaparsec (MonadParsec (try))

data Stmt op varId = Stmt
  { stmtOp :: op,
    stmtArgIds :: [varId],
    stmtResIds :: [varId]
  }
  deriving (Generic)

data ProgArg varId ty = ProgArg
  { progArgName :: T.Text,
    progArgId :: varId,
    progArgType :: ty
  }
  deriving (Generic)

data ProgRes varId ty = ProgRes
  { progResId :: varId,
    progResType :: ty
  }
  deriving (Generic)

data Prog op varId ty = Prog
  { progArgList :: [ProgArg varId ty],
    progStmtList :: [Stmt op varId],
    progResList :: [ProgRes varId ty]
  }
  deriving (Generic)

deriveWith
  mempty {useNoStrategy = True}
  [''Stmt, ''ProgRes, ''Prog, ''ProgArg]
  (allClasses012 \\ pprintClasses)

deriveWith
  mempty {useNoStrategy = True}
  [''Prog]
  [''Mergeable3]

data ProgPPrintError varId op
  = StmtPPrintError (Stmt op varId) Int (OpPPrintError varId op)
  | ResultUndefined Int varId
  | ExtractSubProgError T.Text
  deriving (Generic)

derive [''ProgPPrintError] (allClasses012 \\ pprintClasses)

instance
  (OpPPrint op, Show op, ConcreteVarId varId) =>
  PPrint (ProgPPrintError varId op)
  where
  pformat (StmtPPrintError stmt index err) =
    nest
      2
      ( "Error in statement "
          <> pformat index
          <> ": "
          <> hardline
          <> pformat err
          <> "."
      )
      <> hardline
      <> nest
        2
        ( "Raw statement: "
            <> hardline
            <> pformat (show stmt)
        )
  pformat (ResultUndefined index varId) =
    "Error in result "
      <> pformat index
      <> ": the variable "
      <> pformat (toInteger varId)
      <> " is undefined."
  pformat (ExtractSubProgError err) =
    "Error while extracting sub-program: " <> pformat err

prettyStmt ::
  ( ConcreteVarId varId,
    OpPPrint op
  ) =>
  Int ->
  Stmt op varId ->
  StateT (VarIdMap varId) (Either (ProgPPrintError varId op)) (Doc ann)
prettyStmt index stmt@(Stmt op argIds resIds) = do
  map <- get
  arpformat <- case prettyArguments op argIds map of
    Left err -> throwError $ StmtPPrintError stmt index err
    Right arpformat -> pure arpformat
  let opPretty = pformatOp op
  (newMap, resPretty) <- case prettyResults op resIds map of
    Left err -> throwError $ StmtPPrintError stmt index err
    Right resPretty -> pure resPretty
  put newMap
  return $ resPretty <> " = " <> opPretty <> arpformat

prettyProg ::
  ( ConcreteVarId varId,
    OpPPrint op,
    PPrint ty
  ) =>
  T.Text ->
  Prog op varId ty ->
  Either (ProgPPrintError varId op) (Doc ann)
prettyProg key (Prog argList stmtList resList) = do
  let initMap =
        HM.fromList $ map (\arg -> (progArgId arg, progArgName arg)) argList
  flip evalStateT initMap $ do
    stmtsPretty <- traverse (uncurry prettyStmt) (zip [0 ..] stmtList)
    let firstLine =
          nest (-2) $
            "def "
              <> pformat key
              <> parenCommaList
                ( map
                    ( \arg ->
                        pformat (progArgName arg)
                          <> ": "
                          <> pformat (progArgType arg)
                    )
                    argList
                )
              <> " -> "
              <> parenCommaListIfNotSingle (pformat . progResType <$> resList)
              <> ":"
    allMap <- get
    let lookupVarId (idx, varId) =
          maybe (throwError $ ResultUndefined idx varId) return $
            HM.lookup varId allMap
    retNames <- traverse lookupVarId (zip [0 ..] $ progResId <$> resList)
    let ret = "return" <+> parenCommaListIfNotSingle (pformat <$> retNames)
    return . nest 2 . concatWith (\x y -> x <> hardline <> y) $
      concat [[firstLine], stmtsPretty, [ret]]

instance
  ( OpPPrint op,
    ConcreteVarId varId,
    PPrint ty,
    Show op,
    Show ty
  ) =>
  ProgPPrint (Prog op varId ty)
  where
  pformatProg key prog = progDoc
    where
      progDoc = case prettyProg key prog of
        Left err ->
          Left $
            pformat err
              <> hardline
              <> nest 2 ("Raw program: " <> hardline <> pformat (show prog))
        Right doc -> Right doc

stmtToDotNode ::
  (ConcreteVarId varId, OpPPrint op) =>
  T.Text ->
  Int ->
  Stmt op varId ->
  StateT
    (VarIdToLabel varId)
    (Either (ProgPPrintError varId op))
    (DotNode T.Text, [DotEdge T.Text])
stmtToDotNode progName index stmt@(Stmt op argIds resIds) = do
  map <- get
  let nodeId = progName <> "_stmt" <> showAsText index
  (argFields, edges) <-
    case argumentsToFieldEdges nodeId op argIds map of
      Left err -> throwError $ StmtPPrintError stmt index err
      Right argFieldEdges -> pure argFieldEdges
  let opPretty = TL.fromStrict $ renderDoc 80 $ pformatOp op
  (newMap, resFields) <-
    case resultsToFieldEdges nodeId op resIds map of
      Left err -> throwError $ StmtPPrintError stmt index err
      Right resFields -> pure resFields
  put newMap
  return
    ( DotNode
        nodeId
        [ Label . RecordLabel $
            [ FlipFields
                [ FlipFields argFields,
                  FieldLabel opPretty,
                  FlipFields resFields
                ]
            ],
          shape Record
        ],
      edges
    )

progToDotSubGraph ::
  (ConcreteVarId varId, OpPPrint op, PPrint ty) =>
  T.Text ->
  Prog op varId ty ->
  Either (ProgPPrintError varId op) (DotSubGraph T.Text)
progToDotSubGraph key (Prog argList stmtList resList) = do
  let buildArgField arg =
        let argName = TL.fromStrict $ progArgName arg
            argType = TL.fromStrict $ renderDoc 80 (pformat $ progArgType arg)
         in LabelledTarget (PN argName) (argName <> ": " <> argType)
  let argNodeId = key <> "_args"
  let argNode =
        DotNode
          argNodeId
          [ Label . RecordLabel $
              [ FlipFields
                  [ FieldLabel "args",
                    FlipFields $ map buildArgField argList
                  ]
              ],
            shape Record
          ]
  let resPortAtPos pos = TL.fromStrict $ "res" <> showAsText pos
  let resLabel pos res =
        TL.fromStrict $
          "res"
            <> showAsText pos
            <> ": "
            <> renderDoc 80 (pformat (progResType res))
  let buildResField pos res =
        LabelledTarget (PN $ resPortAtPos pos) $ resLabel pos res
  let resNodeId = key <> "_res"
  let resNode =
        DotNode
          resNodeId
          [ Label . RecordLabel . return . FlipFields $
              [ FlipFields $ zipWith buildResField [0 ..] resList,
                FieldLabel "res"
              ],
            shape Record
          ]
  let initMap =
        HM.fromList $
          map
            ( \arg ->
                ( progArgId arg,
                  (argNodeId, PN $ TL.fromStrict $ progArgName arg)
                )
            )
            argList
  flip evalStateT initMap $ do
    stmtsPretty <-
      traverse (uncurry $ stmtToDotNode key) (zip [0 ..] stmtList)
    let nodes = fst <$> stmtsPretty
    let edges = concatMap snd stmtsPretty
    allMap <- get
    let lookupLabel map idx varId =
          maybe
            (throwError $ ResultUndefined idx varId)
            return
            (HM.lookup varId map)
    resPreLabels <-
      traverse (uncurry $ lookupLabel allMap) . zip [0 ..] $
        progResId <$> resList
    let preLabelToEdge (from, port) resPos =
          DotEdge
            from
            resNodeId
            [ HeadPort $ LabelledPort (PN $ resPortAtPos resPos) Nothing,
              TailPort $ LabelledPort port Nothing
            ]
    return $
      DotSG
        { isCluster = True,
          subGraphID = Just $ Str $ TL.fromStrict key,
          subGraphStmts =
            DotStmts
              { attrStmts = [GraphAttrs [textLabel $ TL.fromStrict key]],
                subGraphs = [],
                nodeStmts = [argNode] <> nodes <> [resNode],
                edgeStmts = edges <> zipWith preLabelToEdge resPreLabels [0 ..]
              }
        }

instance
  ( OpPPrint op,
    ConcreteVarId varId,
    PPrint ty,
    Show op,
    Show ty
  ) =>
  ProgToDot (Prog op varId ty)
  where
  toDotProg key prog =
    case progToDotSubGraph key prog of
      Left err ->
        let errTxt =
              renderDoc 80 $
                nest
                  2
                  ( "Error while pretty-printing program "
                      <> pformat key
                      <> hardline
                      <> pformat err
                  )
                  <> hardline
                  <> nest
                    2
                    ("Raw program: " <> hardline <> pformat (show prog))
         in DotSG
              { isCluster = True,
                subGraphID = Just $ Str $ TL.fromStrict key,
                subGraphStmts =
                  DotStmts
                    { attrStmts = [],
                      subGraphs = [],
                      nodeStmts = [DotNode errTxt []],
                      edgeStmts = []
                    }
              }
      Right graph -> graph

instance (ProgPPrint (Prog op varId ty)) => PPrint (Prog op varId ty) where
  pformat prog = case pformatProg "anonymous" prog of
    Left err ->
      nest
        2
        ( "Error while pretty-printing program "
            <> hardline
            <> err
        )
    Right doc -> doc

type Env varId val = HM.HashMap varId val

addVal ::
  (ConcreteVarId varId, MonadContext ctx) =>
  varId ->
  val ->
  StateT (Env varId val) ctx ()
addVal varId val = do
  env <- get
  when (HM.member varId env) . throwError $
    "Variable " <> showAsText varId <> " is already defined."
  put $ HM.insert varId val env

lookupVal ::
  (ConcreteVarId varId, MonadContext ctx) =>
  varId ->
  StateT (Env varId val) ctx val
lookupVal varId = do
  env <- get
  case HM.lookup varId env of
    Nothing -> throwError $ "Variable " <> showAsText varId <> " is undefined."
    Just val -> return val

instance
  {-# OVERLAPPABLE #-}
  ( OpSemantics semObj op val ctx,
    ConcreteVarId varId,
    Mergeable val,
    Mergeable ty,
    ty ~ OpTypeType op
  ) =>
  ProgSemantics semObj (Prog op varId ty) val ctx
  where
  runProg sem table (Prog arg stmts ret) inputs = tryMerge $ do
    when (length inputs /= length arg) . throwError $
      "Expected "
        <> showAsText (length arg)
        <> " arguments, but got "
        <> showAsText (length inputs)
        <> " arguments."
    let initialEnv = HM.fromList $ zip (progArgId <$> arg) inputs
    let runStmt (Stmt op argIds resIds) = do
          args <- traverse lookupVal argIds
          res <- lift $ applyOp sem table op args
          when (length res /= length resIds) . throwError $
            "Incorrect number of results."
          traverse_ (uncurry addVal) $ zip resIds res
    flip evalStateT initialEnv $ do
      traverse_ runStmt stmts
      traverse (lookupVal . progResId) ret

instance (Mergeable ty) => ProgTyping (Prog op varId ty) where
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
  getStmtDisabled _ = toSym False

instance StmtUtil (Stmt op varId) where
  type StmtVarIdType (Stmt op varId) = varId
  type StmtOpType (Stmt op varId) = op

instance ProgUtilImpl (Prog op varId ty) op (Stmt op varId) varId where
  getProgArgIds = map progArgId . progArgList
  getProgResIds = map progResId . progResList
  getProgNumStmts = length . progStmtList
  getProgStmtAtIdx prog idx
    | idx >= getProgNumStmts prog = throwError "Statement index out of bounds."
    | otherwise = return $ progStmtList prog !! idx

instance ProgUtil (Prog op varId ty) where
  type ProgTypeType (Prog op varId ty) = ty
  type ProgStmtType (Prog op varId ty) = Stmt op varId
  type ProgVarIdType (Prog op varId ty) = varId
  type ProgOpType (Prog op varId ty) = op

instance
  (MonadContext ctx, OpCost opCostObj op cost ctx, Num cost) =>
  ProgCost (PerStmtCostObj opCostObj) (Prog op varId ty) cost ctx
  where
  progCost (PerStmtCostObj obj) table (Prog _ stmts _) = do
    stmtCosts <- traverse (opCost obj table . stmtOp) stmts
    return $ sum stmtCosts

instance (OpReachableSymbols op) => ProgReachableSymbols (Prog op varId ty) where
  progReachableSymbols =
    mconcat . fmap (opReachableSymbols . stmtOp) . progStmtList

eliminateDeadCode ::
  (ConcreteVarId varId) => Prog op varId ty -> Prog op varId ty
eliminateDeadCode (Prog args stmts res) = do
  Prog args (reverse $ go (HS.fromList $ progResId <$> res) $ reverse stmts) res
  where
    go _ [] = []
    go reachable (stmt : rest) =
      if any (`HS.member` reachable) (stmtResIds stmt)
        then stmt : go (HS.union reachable (HS.fromList $ stmtArgIds stmt)) rest
        else go reachable rest

eliminateProgTableDeadCode ::
  (ConcreteVarId varId) =>
  SymbolTable (Prog op varId ty) ->
  SymbolTable (Prog op varId ty)
eliminateProgTableDeadCode (SymbolTable table) =
  SymbolTable $ map (second eliminateDeadCode) table

instance (LowestSeqNum op) => LowestSeqNum (Prog op varId ty) where
  lowestSeqNum succeeded (Prog _ stmts _) =
    lowestSeqNumList succeeded $ stmtOp <$> stmts

instance (PartitionSpec op) => PartitionSpec (Stmt op varId) where
  partitionSpec seqNum Stmt {..} =
    let lst = partitionSpec seqNum stmtOp
     in [Stmt {stmtOp = l, ..} | l <- lst]

instance (PartitionSpec op) => PartitionSpec (Prog op varId ty) where
  partitionSpec seqNum Prog {..} =
    let lst = partitionSpecList seqNum progStmtList
     in [Prog {progStmtList = l, ..} | l <- lst]

instance
  (GenSymSimple op0 op1, GenSymSimple ty0 ty1) =>
  GenSymSimple (Prog op0 varId ty0) (Prog op1 varId ty1)
  where
  simpleFresh (Prog args stmts ress) = do
    newStmts <- traverse goStmt stmts
    newArgs <-
      traverse
        ( \arg ->
            (\ty -> arg {progArgType = ty}) <$> simpleFresh (progArgType arg)
        )
        args
    newResTypes <-
      traverse
        ( \res ->
            (\ty -> res {progResType = ty}) <$> simpleFresh (progResType res)
        )
        ress
    return $ Prog newArgs newStmts newResTypes
    where
      goStmt (Stmt op argIds resIds) = do
        newOp <- simpleFresh op
        return $ Stmt newOp argIds resIds

-- Parsing

progArgParser ::
  (Num varId, TypeParser ty, CharParser e s m) => varId -> m (ProgArg varId ty)
progArgParser n = do
  i <- identifier
  colon
  ProgArg i n <$> typeParser

progArgListParser ::
  (Num varId, TypeParser ty, CharParser e s m) =>
  m ([ProgArg varId ty], HM.HashMap T.Text varId)
progArgListParser = do
  r <- parenCommaSep (progArgParser 0)
  let refined =
        zipWith
          (\i (ProgArg n _ t) -> ProgArg n i t)
          (fromIntegral <$> [0 ..])
          r
  let map = HM.fromList [(progArgName a, progArgId a) | a <- refined]
  return (refined, map)

progRetTypeListParser :: (TypeParser ty, CharParser e s m) => m [ty]
progRetTypeListParser = parenCommaSepOrSingleton typeParser

mapLookup ::
  (CharParser e s m) =>
  HM.HashMap T.Text varId -> [T.Text] -> m [varId]
mapLookup _ [] = return []
mapLookup map (i : is) = case HM.lookup i map of
  Just v -> (v :) <$> mapLookup map is
  Nothing -> fail $ "Unknown argument name: " <> T.unpack i

argListParser :: (CharParser e s m) => HM.HashMap T.Text varId -> m [varId]
argListParser map = do
  r <- parenCommaSep identifier
  mapLookup map r

resListParser ::
  (CharParser e s m, Num varId) =>
  HM.HashMap T.Text varId -> m ([varId], HM.HashMap T.Text varId)
resListParser map = do
  r <- parenCommaSepOrSingleton identifier
  let l = take (length r) $ fromIntegral <$> [(HM.size map) ..]
  return (l, HM.union map $ HM.fromList $ zip r l)

stmtParser ::
  (CharParser e s m, Num varId, OpParser op) =>
  HM.HashMap T.Text varId -> m (Stmt op varId, HM.HashMap T.Text varId)
stmtParser map = do
  (resList, newMap) <- resListParser map
  symbol "="
  op <- opParser
  argList <- argListParser map
  return (Stmt op argList resList, newMap)

progResListParser ::
  (CharParser e s m) =>
  [ty] ->
  HM.HashMap T.Text varId ->
  m [ProgRes varId ty]
progResListParser types map = do
  symbol "return"
  r <- parenCommaSepOrSingleton identifier
  lst <- mapLookup map r
  when (length lst /= length types) $ fail "Mismatched return types"
  return $ zipWith ProgRes lst types

instance
  (OpParser op, TypeParser ty, Num varId) =>
  ProgParser (Prog op varId ty)
  where
  progParser = do
    symbol "def"
    name <- identifier
    (progArgList, map) <- progArgListParser
    symbol "->"
    progResTypeList <- progRetTypeListParser
    symbol ":"
    let goResList map = ([],) <$> progResListParser progResTypeList map
    let go map =
          try (goResList map) <|> do
            (stmt, newMap) <- stmtParser map
            (remainingStmtList, resList) <- go newMap
            return (stmt : remainingStmtList, resList)
    (stmtList, resList) <- go map
    return (name, Prog progArgList stmtList resList)

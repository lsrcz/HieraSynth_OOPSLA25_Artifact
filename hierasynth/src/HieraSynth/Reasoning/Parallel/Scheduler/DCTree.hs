{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module HieraSynth.Reasoning.Parallel.Scheduler.DCTree
  ( NodeId (..),
    DCTree,
    nodeParent,
    nodeDividedChildren,
    nodeExtraParents,
    nodeExtraChildren,
    nodeFailed,
    nodeSketchSpec,
    numNodes,
    leafNodes,
    nodeDepth,
    insertRootSketch,
    insertRootSketches,
    insertSplittedSketches,
    markNodeFailed,
    emptyDCTree,
    directSiblingNodes,
    allChildrenNodes,
    allSiblingNodes,
    rootNodes,
  )
where

import Data.Bifunctor (first)
import qualified Data.HashMap.Internal.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Grisette (PPrint (pformat), derive)

newtype NodeId = NodeId Int

data Node sketchSpec = Node
  { _nodeId :: NodeId,
    _nodeParent :: Maybe NodeId,
    _nodeDividedChildren :: Maybe (HS.HashSet NodeId),
    _nodeExtraParents :: HS.HashSet NodeId,
    _nodeExtraChildren :: HS.HashSet NodeId,
    _nodeFailed :: Bool,
    _nodeSketchSpec :: sketchSpec
  }

data DCTree sketchSpec = DCTree
  { dcTreeNodes :: HM.HashMap NodeId (Node sketchSpec),
    dcTreeRoots :: HS.HashSet NodeId
  }

derive
  [''NodeId, ''Node, ''DCTree]
  [''Eq, ''Ord, ''Hashable]

instance Show NodeId where
  show (NodeId n) = show n

instance PPrint NodeId where
  pformat (NodeId n) = pformat n

derive
  [''Node, ''DCTree]
  [''Show, ''PPrint]

emptyDCTree :: DCTree sketchSpec
emptyDCTree = DCTree HM.empty mempty

numNodes :: DCTree sketchSpec -> Int
numNodes = HM.size . dcTreeNodes

rootNodes :: DCTree sketchSpec -> HS.HashSet NodeId
rootNodes = dcTreeRoots

leafNodes :: DCTree sketchSpec -> HS.HashSet NodeId
leafNodes DCTree {..} =
  let nodes = HM.elems dcTreeNodes
   in HS.fromList $
        _nodeId
          <$> filter
            ( \node ->
                HS.null (_nodeExtraChildren node)
                  && maybe True HS.null (_nodeDividedChildren node)
            )
            nodes

nodeDepth :: DCTree sketchSpec -> NodeId -> Int
nodeDepth tree@DCTree {..} nid =
  case nodeParent tree nid of
    Nothing -> 0
    Just parent -> 1 + nodeDepth tree parent

{-# INLINE _getNode #-}
_getNode :: DCTree sketchSpec -> NodeId -> Node sketchSpec
_getNode DCTree {..} nid = dcTreeNodes HM.! nid

{-# INLINE nodeParent #-}
nodeParent :: DCTree sketchSpec -> NodeId -> Maybe NodeId
nodeParent tree nid = _nodeParent $ _getNode tree nid

{-# INLINE nodeDividedChildren #-}
nodeDividedChildren :: DCTree sketchSpec -> NodeId -> Maybe (HS.HashSet NodeId)
nodeDividedChildren tree nid =
  _nodeDividedChildren $ _getNode tree nid

{-# INLINE nodeExtraParents #-}
nodeExtraParents :: DCTree sketchSpec -> NodeId -> HS.HashSet NodeId
nodeExtraParents tree nid = _nodeExtraParents $ _getNode tree nid

{-# INLINE nodeExtraChildren #-}
nodeExtraChildren :: DCTree sketchSpec -> NodeId -> HS.HashSet NodeId
nodeExtraChildren tree nid = _nodeExtraChildren $ _getNode tree nid

{-# INLINE nodeFailed #-}
nodeFailed :: DCTree sketchSpec -> NodeId -> Bool
nodeFailed tree nid = _nodeFailed $ _getNode tree nid

{-# INLINE nodeSketchSpec #-}
nodeSketchSpec :: DCTree sketchSpec -> NodeId -> sketchSpec
nodeSketchSpec nid tree = _nodeSketchSpec $ _getNode nid tree

{-# INLINE _nextNodeId #-}
_nextNodeId :: DCTree sketchSpec -> NodeId
_nextNodeId DCTree {..} = NodeId $ HM.size dcTreeNodes + 1

_insertSketch ::
  (Hashable sketchSpec) =>
  DCTree sketchSpec ->
  Maybe NodeId ->
  sketchSpec ->
  -- | node id, new node inserted, new tree
  (NodeId, DCTree sketchSpec)
_insertSketch tree@DCTree {..} parent sketch =
  let nodeId = _nextNodeId tree
      node failed = Node nodeId parent Nothing mempty mempty failed sketch
   in case parent of
        Nothing ->
          ( nodeId,
            tree
              { dcTreeNodes = HM.insert nodeId (node False) dcTreeNodes,
                dcTreeRoots = HS.insert nodeId dcTreeRoots
              }
          )
        Just parentId ->
          let parentNode = dcTreeNodes HM.! parentId
           in ( nodeId,
                tree
                  { dcTreeNodes =
                      HM.insert
                        nodeId
                        (node $ _nodeFailed parentNode)
                        dcTreeNodes
                  }
              )

insertRootSketch ::
  (Hashable sketchSpec) =>
  DCTree sketchSpec ->
  sketchSpec ->
  (NodeId, DCTree sketchSpec)
insertRootSketch tree = _insertSketch tree Nothing

insertRootSketches ::
  (Hashable sketchSpec) =>
  DCTree sketchSpec ->
  [sketchSpec] ->
  ([NodeId], DCTree sketchSpec)
insertRootSketches tree sketches =
  first reverse $
    foldl
      ( \(nodeIds, newTree) sketch ->
          let (nodeId, newTree2) = _insertSketch newTree Nothing sketch
           in (nodeId : nodeIds, newTree2)
      )
      (mempty, tree)
      sketches

insertSplittedSketches ::
  (Hashable sketchSpec) =>
  DCTree sketchSpec ->
  NodeId ->
  [sketchSpec] ->
  ([NodeId], DCTree sketchSpec)
insertSplittedSketches tree parent sketches =
  let (ids, newTree) =
        foldl
          ( \(nodeIds, newTree) sketch ->
              let (nodeId, tree) = _insertSketch newTree (Just parent) sketch
               in (nodeId : nodeIds, tree)
          )
          ([], tree)
          sketches
      parentNode = dcTreeNodes tree HM.! parent
   in case _nodeDividedChildren parentNode of
        Nothing ->
          ( reverse ids,
            newTree
              { dcTreeNodes =
                  HM.insert
                    parent
                    (parentNode {_nodeDividedChildren = Just (HS.fromList ids)})
                    $ dcTreeNodes newTree
              }
          )
        Just _ ->
          error "insertSplittedSketches: parent node is already splitted"

_checkAndPropagateSplittedFailure ::
  DCTree sketchSpec -> NodeId -> (HS.HashSet NodeId, DCTree sketchSpec)
_checkAndPropagateSplittedFailure tree@DCTree {..} nid =
  if nodeFailed tree nid
    then (mempty, tree)
    else case nodeDividedChildren tree nid of
      Nothing -> (mempty, tree)
      Just children ->
        if all (nodeFailed tree) children
          then markNodeFailed tree nid
          else (mempty, tree)

_setNodeFailed :: DCTree sketchSpec -> NodeId -> DCTree sketchSpec
_setNodeFailed tree@DCTree {..} nid =
  let node = dcTreeNodes HM.! nid
   in tree
        { dcTreeNodes =
            HM.insert
              nid
              (node {_nodeFailed = True})
              dcTreeNodes
        }

markNodeFailed ::
  DCTree sketchSpec -> NodeId -> (HS.HashSet NodeId, DCTree sketchSpec)
markNodeFailed tree@DCTree {..} nid =
  if nodeFailed tree nid
    then (mempty, tree)
    else
      let tree0 = _setNodeFailed tree nid
          (markedFailed', tree1) =
            foldl
              ( \(oldInferredFailed, tree) nid ->
                  first (HS.union oldInferredFailed) $ markNodeFailed tree nid
              )
              (mempty, tree0)
              ( fromMaybe mempty (nodeDividedChildren tree nid)
                  <> nodeExtraChildren tree nid
              )
          markedFailed = HS.insert nid markedFailed'
       in maybe
            (markedFailed, tree1)
            ( first (HS.union markedFailed)
                . _checkAndPropagateSplittedFailure tree1
            )
            (nodeParent tree nid)

_parentNode :: DCTree sketchSpec -> NodeId -> Maybe (Node sketchSpec)
_parentNode DCTree {..} nid =
  let node = dcTreeNodes HM.! nid
   in case _nodeParent node of
        Nothing -> Nothing
        Just parent -> Just $ dcTreeNodes HM.! parent

directSiblingNodes :: DCTree sketchSpec -> NodeId -> HS.HashSet NodeId
directSiblingNodes tree@DCTree {..} nid = HS.delete nid . fromMaybe mempty $ do
  parent <- nodeParent tree nid
  nodeDividedChildren tree parent

allChildrenNodes :: DCTree sketchSpec -> NodeId -> HS.HashSet NodeId
allChildrenNodes tree@DCTree {..} nid = iter initialFrontier initialFrontier
  where
    children nid =
      fromMaybe mempty (nodeDividedChildren tree nid)
        <> nodeExtraChildren tree nid
    initialFrontier = children nid
    iter :: HS.HashSet NodeId -> HS.HashSet NodeId -> HS.HashSet NodeId
    iter visited frontier =
      let newFrontier =
            mconcat (children <$> HS.toList frontier)
              `HS.difference` visited
       in if HS.null frontier
            then visited
            else iter (HS.union visited newFrontier) newFrontier

allSiblingNodes :: DCTree sketchSpec -> NodeId -> HS.HashSet NodeId
allSiblingNodes tree@DCTree {..} nid = HS.delete nid . fromMaybe mempty $ do
  parent <- nodeParent tree nid
  return $
    fromMaybe mempty (nodeDividedChildren tree parent)
      <> nodeExtraChildren tree parent

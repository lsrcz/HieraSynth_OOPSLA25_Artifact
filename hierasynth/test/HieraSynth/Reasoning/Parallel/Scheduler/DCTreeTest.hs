module HieraSynth.Reasoning.Parallel.Scheduler.DCTreeTest (dcTreeTest) where

import qualified Data.HashSet as HS
import HieraSynth.Reasoning.Parallel.Scheduler.DCTree
  ( DCTree,
    NodeId (NodeId),
    allChildrenNodes,
    allSiblingNodes,
    directSiblingNodes,
    emptyDCTree,
    insertRootSketch,
    insertRootSketches,
    insertSplittedSketches,
    leafNodes,
    markNodeFailed,
    nodeDividedChildren,
    nodeFailed,
    nodeParent,
    numNodes,
    rootNodes,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

dcTreeTest :: Test
dcTreeTest =
  testGroup
    "HieraSynth.Reasoning.Parallel.DCTree"
    [ testGroup
        "No extra links"
        [ testCase "emptyDCTree" $ do
            let tree = emptyDCTree :: DCTree Int
            numNodes tree @?= 0
            rootNodes tree @?= mempty
            leafNodes tree @?= mempty,
          testCase "insertRootSketch" $ do
            let tree0 = emptyDCTree :: DCTree Int
            let (nodeId, tree1) = insertRootSketch tree0 1
            numNodes tree1 @?= 1
            nodeId @?= NodeId 1
            nodeParent tree1 nodeId @?= Nothing
            rootNodes tree1 @?= HS.singleton nodeId
            leafNodes tree1 @?= HS.singleton nodeId
            let (nodeId2, tree2) = insertRootSketch tree1 2
            numNodes tree2 @?= 2
            nodeId2 @?= NodeId 2
            nodeParent tree2 nodeId2 @?= Nothing
            rootNodes tree2 @?= HS.fromList [nodeId, nodeId2]
            leafNodes tree2 @?= HS.fromList [nodeId, nodeId2],
          testCase "insertRootSketches" $ do
            let tree0 = emptyDCTree :: DCTree Int
            let (nodeIds, tree1) = insertRootSketches tree0 [1, 2]
            numNodes tree1 @?= 2
            nodeIds @?= [NodeId 1, NodeId 2]
            rootNodes tree1 @?= HS.fromList [NodeId 1, NodeId 2]
            leafNodes tree1 @?= HS.fromList [NodeId 1, NodeId 2],
          testCase "insertSplittedSketches" $ do
            let tree0 = emptyDCTree :: DCTree Int
            let (nodeId1, tree1) = insertRootSketch tree0 1
            let (nodeId2, tree2) = insertRootSketch tree1 2
            let (nodeIds3, tree3) = insertSplittedSketches tree2 nodeId1 [3, 4]
            numNodes tree3 @?= 4
            nodeIds3 @?= [NodeId 3, NodeId 4]
            nodeParent tree3 (NodeId 3) @?= Just nodeId1
            nodeParent tree3 (NodeId 4) @?= Just nodeId1
            nodeDividedChildren tree3 nodeId1
              @?= Just (HS.fromList [NodeId 3, NodeId 4])
            rootNodes tree3 @?= HS.fromList [nodeId1, nodeId2]
            leafNodes tree3 @?= HS.fromList [nodeId2, NodeId 3, NodeId 4],
          testCase "markNodeFailed -- down" $ do
            let tree0 = emptyDCTree :: DCTree Int
            let (nodeId1, tree1) = insertRootSketch tree0 1
            let (nodeId2, tree2) = insertRootSketch tree1 2
            let (_, tree3) = insertSplittedSketches tree2 nodeId1 [3, 4]
            let (_, tree4) = insertSplittedSketches tree3 (NodeId 3) [5, 6]
            let (_, tree5) = insertSplittedSketches tree4 (NodeId 4) [7, 8]
            let (marked, tree6) = markNodeFailed tree5 (NodeId 4)
            marked @?= HS.fromList [NodeId 4, NodeId 7, NodeId 8]
            let (marked2, tree7) = markNodeFailed tree6 (NodeId 1)
            marked2 @?= HS.fromList [nodeId1, NodeId 3, NodeId 5, NodeId 6]
            nodeFailed tree7 nodeId1 @?= True
            nodeFailed tree7 nodeId2 @?= False
            nodeFailed tree7 (NodeId 3) @?= True
            nodeFailed tree7 (NodeId 4) @?= True
            nodeFailed tree7 (NodeId 5) @?= True
            nodeFailed tree7 (NodeId 6) @?= True,
          testCase "markNodeFailed -- up" $ do
            let tree0 = emptyDCTree :: DCTree Int
            let (nodeId1, tree1) = insertRootSketch tree0 1
            let (nodeId2, tree2) = insertRootSketch tree1 2
            let (_, tree3) = insertSplittedSketches tree2 nodeId1 [3, 4]
            let (marked, tree4) = markNodeFailed tree3 (NodeId 3)
            marked @?= HS.fromList [NodeId 3]
            nodeFailed tree4 nodeId1 @?= False
            nodeFailed tree4 nodeId2 @?= False
            nodeFailed tree4 (NodeId 3) @?= True
            nodeFailed tree4 (NodeId 4) @?= False
            let (marked2, tree5) = markNodeFailed tree4 (NodeId 4)
            marked2 @?= HS.fromList [nodeId1, NodeId 4]
            nodeFailed tree5 nodeId1 @?= True
            nodeFailed tree5 nodeId2 @?= False
            nodeFailed tree5 (NodeId 3) @?= True
            nodeFailed tree5 (NodeId 4) @?= True,
          testCase "insertSplittedSketches -- failed" $ do
            let tree0 = emptyDCTree :: DCTree Int
            let (nodeId1, tree1) = insertRootSketch tree0 1
            let (_, tree2) = markNodeFailed tree1 nodeId1
            let (_, tree3) = insertSplittedSketches tree2 nodeId1 [2, 3]
            nodeFailed tree3 nodeId1 @?= True
            nodeFailed tree3 (NodeId 2) @?= True
            nodeFailed tree3 (NodeId 3) @?= True,
          testCase "siblingNodes" $ do
            let tree0 = emptyDCTree :: DCTree Int
            let (nodeId1, tree1) = insertRootSketch tree0 1
            let (_, tree2) = insertRootSketch tree1 2
            let (_, tree3) = insertSplittedSketches tree2 nodeId1 [3, 4, 5]
            allSiblingNodes tree3 nodeId1 @?= HS.empty
            allSiblingNodes tree3 (NodeId 3)
              @?= HS.fromList [NodeId 4, NodeId 5]
            directSiblingNodes tree3 nodeId1 @?= HS.empty
            directSiblingNodes tree3 (NodeId 3)
              @?= HS.fromList [NodeId 4, NodeId 5],
          testCase "allChildrenNodes" $ do
            let tree0 = emptyDCTree :: DCTree Int
            let (nodeId1, tree1) = insertRootSketch tree0 1
            let (_, tree2) = insertRootSketch tree1 2
            let (_, tree3) = insertSplittedSketches tree2 nodeId1 [3, 4, 5]
            let (_, tree4) = insertSplittedSketches tree3 (NodeId 3) [6, 7, 8]
            allChildrenNodes tree4 nodeId1
              @?= HS.fromList
                [NodeId 3, NodeId 4, NodeId 5, NodeId 6, NodeId 7, NodeId 8]
        ]
    ]

{-# LANGUAGE UndecidableInstances #-}

module HieraSynth.Program.ProgToDot (ProgToDot (..)) where

import Data.GraphViz
  ( DotGraph
      ( DotGraph,
        directedGraph,
        graphID,
        graphStatements,
        strictGraph
      ),
    DotStatements (DotStmts, attrStmts, edgeStmts, nodeStmts, subGraphs),
    DotSubGraph,
    PrintDot (unqtDot),
  )
import qualified Data.Text as T
import HieraSynth.Program.ProgTyping (ProgTyping)
import HieraSynth.Program.ProgUtil (ProgUtil)
import HieraSynth.Program.SymbolTable (SymbolTable (SymbolTable))

class ProgToDot prog where
  toDotProg :: T.Text -> prog -> DotSubGraph T.Text

instance
  (ProgToDot prog, ProgUtil prog, ProgTyping prog) =>
  PrintDot (SymbolTable prog)
  where
  unqtDot (SymbolTable lst) =
    unqtDot $
      DotGraph
        { strictGraph = False,
          directedGraph = True,
          graphID = Nothing,
          graphStatements =
            DotStmts
              { attrStmts = [],
                subGraphs = uncurry toDotProg <$> lst,
                nodeStmts = [],
                edgeStmts = []
              }
        }

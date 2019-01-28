{-# LANGUAGE PartialTypeSignatures #-}
module GV where

import           Data.Graph.Inductive.Graph        (Node, subgraph)
import           Data.Graph.Inductive.Query.DFS    (dfs)
import           Data.GraphViz                     (GlobalAttributes (GraphAttrs),
                                                    GraphvizCanvas (Xlib),
                                                    GraphvizCommand (Dot),
                                                    fmtNode, globalAttributes,
                                                    graphToDot,
                                                    nonClusteredParams,
                                                    runGraphvizCanvas)
import           Data.GraphViz.Algorithms          (transitiveReduction)
import           Data.GraphViz.Attributes.Complete
import           Data.Text.Lazy                    (fromStrict)
import           TheGraph                          (theGraph)

showGraph :: Node -> IO ()
showGraph packageId = runGraphvizCanvas Dot g Xlib
  where
    depsOf nodeId = subgraph (dfs [nodeId] theGraph) theGraph
    g = transitiveReduction $ graphToDot params $ depsOf  packageId

params = nonClusteredParams
    { fmtNode = \(n,l) -> [Label (StrLabel (fromStrict  l))]
    , globalAttributes = [GraphAttrs [RankDir FromLeft] ]
    }

{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PartialTypeSignatures #-}
module GraphViz (showGraph, TransitiveConfig(..)) where

import           Data.Graph.Inductive.Graph        (Node, subgraph)
import           Data.Graph.Inductive.Query.DFS    (dfs)
import           Data.GraphViz                     (DotGraph, GlobalAttributes (GraphAttrs),
                                                    GraphvizCanvas (Xlib),
                                                    GraphvizCommand (Dot),
                                                    GraphvizParams, fmtNode,
                                                    globalAttributes,
                                                    graphToDot,
                                                    nonClusteredParams,
                                                    runGraphvizCanvas)
import           Data.GraphViz.Algorithms          (transitiveReduction)
import           Data.GraphViz.Attributes.Complete
import           Data.Text.Internal                (Text)
import           Data.Text.Lazy                    (fromStrict)
import           TheGraph                          (theGraph)

showGraph :: TransitiveConfig -> Node -> IO ()
showGraph tc packageId = runGraphvizCanvas Dot g Xlib
  where
    depsOf nodeId = subgraph (dfs [nodeId] theGraph) theGraph
    g = applyTransitiveConfig tc $ graphToDot params $ depsOf  packageId


params :: GraphvizParams n Text el () Text
params = nonClusteredParams
    { fmtNode = \(_nodeId, nodeLabel) -> [Label (StrLabel (fromStrict nodeLabel))]
    , globalAttributes = [GraphAttrs [RankDir FromLeft] ]
    }

data TransitiveConfig = ShowTransitive | HideTransitive

applyTransitiveConfig :: TransitiveConfig -> DotGraph Node -> DotGraph Node
applyTransitiveConfig = \case
    ShowTransitive -> id
    HideTransitive -> transitiveReduction

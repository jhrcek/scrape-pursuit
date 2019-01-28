{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module Main where
import           Conduit                           (concatC, iterMC, liftIO,
                                                    mapMC, runConduit, sinkList,
                                                    yieldMany, (.|))
import           Data.Graph.Inductive.Graph        (mkGraph)
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Map                          as Map
import           Data.Maybe                        (catMaybes, fromMaybe)
import qualified Data.Set                          as Set
import           Data.Text                         (Text, unpack)
import qualified Data.Text                         as Text
import           Test.WebDriver                    (Selector (ByCSS, ByClass),
                                                    WD, WDConfig, attr, chrome,
                                                    defaultConfig, finallyClose,
                                                    findElems, getText,
                                                    openPage, runSession,
                                                    useBrowser)

type NodeLabel = Text

main :: IO ()
main = do
    dependencyPairs <- runSession chromeCaps . finallyClose $ do
        pkgLinks <- getAllPackageLinks
        runConduit $
            yieldMany pkgLinks
            .| mapMC getDependencyEdges
            .| concatC
            .| iterMC (liftIO . print)
            .| sinkList
    let depGraph = buildGraph dependencyPairs
    print depGraph

buildGraph :: [(Text, Text)] -> Gr NodeLabel ()
buildGraph edgeLabels =
    mkGraph nodes edges
  where
    uniquePkgNames = Set.toList $ foldr (\(from, to) acc -> Set.insert from $ Set.insert to acc) Set.empty edgeLabels
    pkgNameToId = Map.fromList $ zip uniquePkgNames [0..]
    nodes = zip [0..] uniquePkgNames
    lookupId lbl = Map.findWithDefault 0 lbl pkgNameToId
    edges = fmap (\(fromLabel, toLabel) -> (lookupId fromLabel, lookupId toLabel, ())   ) edgeLabels

getDependencyEdges :: Text -> WD [(Text, Text)]
getDependencyEdges url = do
    openPage (unpack url)
    deps <- traverse getText =<< findElems (ByClass "deplink__link")
    let pkgName = last $ Text.splitOn "/" url
    return $ mkEdges pkgName deps

mkEdges :: Text -> [Text] -> [(Text, Text)]
mkEdges pkgName =
    fmap (\dep -> (dropPurescript pkgName, dropPurescript dep))

dropPurescript :: Text -> Text
dropPurescript x = fromMaybe ("JS:" <> x) $ Text.stripPrefix "purescript-" x

chromeCaps :: WDConfig
chromeCaps =
    useBrowser chrome defaultConfig

getAllPackageLinks :: WD [Text]
getAllPackageLinks = do
    openPage "http://localhost:3000/"
    pkgDocLinks <- findElems (ByCSS ".multi-col__col:not(:nth-child(1)) a")
    catMaybes <$> mapM (`attr` "href") pkgDocLinks

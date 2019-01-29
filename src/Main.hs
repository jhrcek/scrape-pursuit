{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wall #-}
module Main where
import           Conduit                           (concatC, iterMC, liftIO,
                                                    mapC, mapMC, runConduit,
                                                    sinkList, yieldMany, (.|))
import           Data.Graph.Inductive.Graph        (mkGraph)
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Map                          as Map
import           Data.Maybe                        (catMaybes, fromMaybe)
import qualified Data.Set                          as Set
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import qualified Data.Text.IO
import           Test.WebDriver                    (Selector (ByCSS, ByClass, ByXPath),
                                                    WD, WDConfig, attr, chrome,
                                                    defaultConfig, finallyClose,
                                                    findElem, findElems,
                                                    getCurrentURL, getText,
                                                    openPage, runSession,
                                                    useBrowser)

type NodeLabel = Text

data PackageInfo = PackageInfo
    { packageName      :: Text
    , githubOrgAndRepo :: Text
    , packageDeps      :: [Text]
    , latestVersion    :: Text
    } deriving Show

main :: IO ()
main = do
    dependencyPairs <- runSession chromeCaps . finallyClose $ do
        pkgLinks <- getAllPackageLinks
        runConduit $
            yieldMany pkgLinks
            .| mapMC getPackageInfo
            .| iterMC (liftIO . Data.Text.IO.putStrLn . githubOrgAndRepo)
            .| mapC getDependencyEdges
            .| concatC
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

getPackageInfo :: Text -> WD PackageInfo
getPackageInfo url = do
    openPage (Text.unpack url)
    packageDeps <- traverse getText =<< findElems (ByClass "deplink__link")
    githubOrgAndRepo <- getText =<< findElem (ByXPath "//dt[contains(text(),'Repository')]/following-sibling::dd[1]/a")
    currentUrl <- Text.pack <$> getCurrentURL
    let packageName = lastError ("Failed to retrieve package name from url " <> Text.unpack url) $ Text.splitOn "/" url
    let latestVersion = lastError ("Failed to retrieve version from url " <> Text.unpack currentUrl) $ Text.splitOn "/" currentUrl
    return PackageInfo{..}

lastError :: String -> [a] -> a
lastError err [] = error err
lastError _ xs   = last xs

getDependencyEdges :: PackageInfo -> [(Text, Text)]
getDependencyEdges PackageInfo{packageName, packageDeps} =
   fmap (\dep -> (dropPurescript packageName, dropPurescript dep)) packageDeps

dropPurescript :: Text -> Text
dropPurescript x =
    fromMaybe ("JS:" <> x) $ Text.stripPrefix "purescript-" x

chromeCaps :: WDConfig
chromeCaps =
    useBrowser chrome defaultConfig

getAllPackageLinks :: WD [Text]
getAllPackageLinks = do
    openPage "http://localhost:3000/"
    pkgDocLinks <- findElems (ByCSS ".multi-col__col:not(:nth-child(1)) a")
    catMaybes <$> mapM (`attr` "href") pkgDocLinks

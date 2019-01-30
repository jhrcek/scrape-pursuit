{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Conduit                           (concatMapC, iterMC, liftIO,
                                                    mapMC, runConduit, sinkList,
                                                    yieldMany, (.|))
import           Control.Exception                 (Handler (Handler), catches)
import qualified Control.Foldl                     as Fold
import           Control.Monad                     (when)
import           Data.Graph.Inductive.Graph        (mkGraph)
import           Data.Graph.Inductive.PatriciaTree (Gr)
import qualified Data.Map                          as Map
import           Data.Maybe                        (catMaybes, fromMaybe)
import qualified Data.Set                          as Set
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import           GithubAPI                         (Repo (Repo), Tag (Tag))
import qualified GithubAPI
import           Test.WebDriver                    (Selector (ByCSS, ByClass, ByXPath),
                                                    WD, WDConfig, attr, chrome,
                                                    defaultConfig, finallyClose,
                                                    findElem, findElems,
                                                    getCurrentURL, getText,
                                                    openPage, runSession,
                                                    useBrowser)
import           Turtle
import           Turtle.Line                       (lineToText)
type NodeLabel = Text

data PackageInfo = PackageInfo
    { packageName      :: Text
    , githubOrgAndRepo :: Repo
    , packageDeps      :: [Text]
    , latestVersion    :: Text
    } deriving Show

main :: IO ()
main = do
    dependencyPairs <- runSession chromeCaps . finallyClose $ do
        pkgLinks <- dropWhile (not . Text.isInfixOf "purescript-ring-modules") <$> getAllPackageLinks
        runConduit $
            yieldMany pkgLinks
            .| mapMC getPackageInfo
            .| iterMC (liftIO . checkLatestVersionOnPursuitCorrespondsToLatestGithubTag)
            .| concatMapC getDependencyEdges
            .| sinkList
    let depGraph = buildGraph dependencyPairs
    print depGraph

checkLatestVersionOnPursuitCorrespondsToLatestGithubTag :: PackageInfo -> IO ()
checkLatestVersionOnPursuitCorrespondsToLatestGithubTag
    PackageInfo{githubOrgAndRepo, latestVersion} = do
        mTag <- GithubAPI.getLatestTag githubOrgAndRepo
        case mTag of
          Nothing -> putStrLn $ "Failed to retrieve latest tag for " <> show githubOrgAndRepo
          Just (Tag tag) -> case Text.uncons tag of
            Just ('v', version) -> when (version /= latestVersion) $ do
                putStrLn $ "Package docs not up to date for " <> show githubOrgAndRepo
                           <> ". Pursuit: " <> show latestVersion
                           <> ", GitHub: " <> show tag
                fixVersionMismatch githubOrgAndRepo (Tag tag)
                  `catches`
                      [ Handler $ \ (_ :: ExitCode) -> void $ putStrLn "Upgrade of module docs failed"
                      , Handler $ \ (_ :: ShellFailed) -> void $ putStrLn "Upgrade of module docs failed"
                      ]

            Just _ -> putStrLn $ "WARNING: latest tag doesn't start with 'v':" <> show tag
            Nothing -> putStrLn "WARNING: empty tag!"

fixVersionMismatch :: Repo -> Tag -> IO ()
fixVersionMismatch r@(Repo orgAndRepo) (Tag tag) = do
    mClonedToLine <- fold
        (inproc "cgr.hs" ["git@github.com:" <> orgAndRepo <> ".git"] empty)
        (Fold.find $ \line -> "Repo cloned to " `Text.isPrefixOf` lineToText line)
    case mClonedToLine of
      Nothing -> putStrLn $ "WARNING: failed to clone repo " <> show r
      Just line -> do
        let clonedRepoDir = fromText . last . Text.words $ lineToText line
        cd clonedRepoDir
        shells "git tag" empty
        procs "git" ["checkout", tag] empty
        putStrLn "Is this ok? [y/n]"
        response <- getLine
        when (response == "y") $ do
            shells "bower install && pulp build" empty
            printf ("cd "%fp%" && pulp publish --no-push\n") clonedRepoDir
            putStrLn "Done? [y/n]"
            void getLine



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
    githubOrgAndRepo <- Repo <$> (getText =<< findElem (ByXPath "//dt[contains(text(),'Repository')]/following-sibling::dd[1]/a"))
    currentUrl <- Text.pack <$> getCurrentURL
    let packageName = lastError ("Failed to retrieve package name from url " <> Text.unpack url) $ Text.splitOn "/" url
    let latestVersion = lastError ("Failed to retrieve version from url " <> Text.unpack currentUrl) $ Text.splitOn "/" currentUrl
    return PackageInfo{..}

lastError :: String -> [a] -> a
lastError er [] = error er
lastError _ xs  = last xs

getDependencyEdges :: PackageInfo -> [(Text, Text)]
getDependencyEdges PackageInfo{packageName, packageDeps} =
   fmap (\dep -> (dropPurescript packageName, dropPurescript dep)) packageDeps

dropPurescript :: Text -> Text
dropPurescript pkgName =
    fromMaybe ("JS:" <> pkgName) $ Text.stripPrefix "purescript-" pkgName

chromeCaps :: WDConfig
chromeCaps =
    useBrowser chrome defaultConfig

getAllPackageLinks :: WD [Text]
getAllPackageLinks = do
    openPage "http://localhost:3000/"
    pkgDocLinks <- findElems (ByCSS ".multi-col__col:not(:nth-child(1)) a")
    catMaybes <$> mapM (`attr` "href") pkgDocLinks

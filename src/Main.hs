{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module Main where
import           Conduit        (concatC, liftIO, mapMC, mapM_C, runConduit,
                                 yieldMany, (.|))
import           Data.Maybe     (catMaybes, fromMaybe)
import           Data.Text      (Text, unpack)
import qualified Data.Text      as Text

import qualified Data.Text.IO   as Text
import           Test.WebDriver (Selector (ByCSS, ByClass), WD, WDConfig, attr,
                                 chrome, defaultConfig, finallyClose, findElems,
                                 getText, openPage, runSession, useBrowser)

main :: IO ()
main = runSession chromeCaps . finallyClose $ do
    pkgLinks <- getAllPackageLinks
    runConduit $
        yieldMany pkgLinks
        .| mapMC getDependencyEdges
        .| concatC
        .| mapM_C (liftIO . Text.putStrLn)

getDependencyEdges :: Text -> WD [Text]
getDependencyEdges url = do
    openPage (unpack url)
    deps <- traverse getText =<< findElems (ByClass "deplink__link")
    let pkgName = last $ Text.splitOn "/" url
    return $ mkEdges pkgName deps

mkEdges :: Text -> [Text] -> [Text]
mkEdges pkgName =
    fmap (\dep -> mkEdge (dropPurescript pkgName) (dropPurescript dep))
  where
    mkEdge x y = "\"" <> x <> "\" -> \"" <> y <> "\""

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

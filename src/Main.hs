{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Conduit        (concatC, liftIO, mapMC, mapM_C, runConduit,
                                 yieldMany, (.|))
import           Data.Maybe     (fromMaybe)
import           Data.Text      (Text, unpack)
import qualified Data.Text      as Text
import qualified Data.Text.IO   as Text
import           Test.WebDriver (Selector (ByCSS), WD, WDConfig, chrome,
                                 defaultConfig, finallyClose, findElems,
                                 getText, openPage, runSession, useBrowser)

main :: IO ()
main = do
    urls <- liftIO $ Text.lines <$> Text.readFile "x"
    runSession chromeCaps . finallyClose . runConduit $
        yieldMany urls
        .| mapMC getDependencyEdges
        .| concatC
        .| mapM_C (liftIO . Text.putStrLn)

getDependencyEdges :: Text -> WD [Text]
getDependencyEdges url = do
    openPage (unpack url)
    deps <- traverse getText =<< findElems (ByCSS ".deplink__link")
    let pkgName = last $ Text.splitOn "/" url
    return $ mkEdges pkgName deps

mkEdges :: Text -> [Text] -> [Text]
mkEdges pkgName =
    fmap (\dep -> "\"" <> dropPurescript pkgName <> "\" -> \"" <> dropPurescript dep <> "\"")

dropPurescript :: Text -> Text
dropPurescript x = fromMaybe ("JS:" <> x) $ Text.stripPrefix "purescript-" x

chromeCaps :: WDConfig
chromeCaps =
    useBrowser chrome defaultConfig

{-# LANGUAGE OverloadedStrings #-}
module Config
    ( Config
    , GitHubToken
    , load
    , gitHubToken
    , getToken
    ) where

import           Data.ByteString     (ByteString)
import           Options.Applicative

newtype GitHubToken = GitHubToken
    { getToken :: ByteString
    } deriving (Show)

newtype Config = Config
    { gitHubToken :: GitHubToken
    } deriving (Show)

load :: IO Config
load = execParser opts
  where
    opts :: ParserInfo Config
    opts = info (configParser <**> helper) fullDesc

    configParser :: Parser Config
    configParser = Config . GitHubToken
        <$> strOption
         ( long "github-token"
        <> short 't'
        <> metavar "TOKEN"
        <> help "GitHub token" )

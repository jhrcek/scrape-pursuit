{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module GithubAPI (getLatestTag, Repo(..), Tag(..)) where
import           Config              (GitHubToken, getToken)
import           Control.Exception   (handle)
import           Data.Aeson          (Value)
import           Data.Text           (Text, unpack)
import           Lens.Micro          (Traversal', ix, (&), (?~), (^?))
import           Lens.Micro.Aeson    (key, _Array, _String)
import           Network.HTTP.Client (HttpException)
import           Network.Wreq        (Options, Response, asJSON, auth, defaults,
                                      getWith, oauth2Token, responseBody)
import           Util                (logWarn)

newtype Repo = Repo Text
instance Show Repo where show (Repo userSlashOrg) = unpack userSlashOrg

newtype Tag = Tag Text
instance Show Tag where show (Tag tag) = unpack tag

getLatestTag :: GitHubToken -> Repo -> IO (Maybe Tag)
getLatestTag token (Repo orgSlashRepo) = handle httpExceptionHandler $ do
    resp <- asJSON =<< getWith (opts token) (unpack $ "https://api.github.com/repos/" <> orgSlashRepo <> "/tags")
    return $ fmap Tag $ resp ^? latestTag
  where
    latestTag :: Traversal' (Response Value) Text
    latestTag = responseBody @Value . _Array . ix 0 {- assuming GitHub api always returns tags sorted by most-recent-first -} . key "name" . _String



httpExceptionHandler :: HttpException -> IO (Maybe Tag)
httpExceptionHandler e = do
    logWarn $ "got exception: " <> show e
    return Nothing

opts :: GitHubToken -> Options
opts token = defaults & auth ?~ oauth2Token (getToken token)

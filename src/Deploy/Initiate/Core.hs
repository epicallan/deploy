{-# LANGUAGE OverloadedStrings #-}

module Deploy.Initiate.Core (runDeploy) where

import           Control.Exception.Safe                (catchIO)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Trans.Class             (lift)
import           Control.Monad.Trans.Reader
import           Data.ByteString.Char8                 as BS hiding (getLine,
                                                              putStrLn)
import           Data.Maybe
import           Deploy.Types                          (Repo (..))
import           Deploy.Util                           (handlerIO)
import           Network.HTTP.Client                   (defaultManagerSettings,
                                                        httpLbs, newManager,
                                                        parseRequest)
import           Network.HTTP.Client.MultipartFormData (formDataBody, partBS,
                                                        partFileSource)
import           System.Directory                      (getCurrentDirectory)
import           System.Process                        (callCommand)


getRepoDetails :: IO (Maybe Repo)
getRepoDetails = do
  repoPath <- liftIO getCurrentDirectory
  repoName <- getLine
  pure $ Just (Repo (Just repoName) (Just repoPath))

archiveFiles :: ReaderT Repo IO ()
archiveFiles = do
  repo <- ask
  case path repo of
    Just filePath ->
      liftIO $ catchIO (callCommand ("git archive --format=tar.gz --output" ++ filePath ++ ".tar.gz master ")) handlerIO
    Nothing -> liftIO $ putStrLn "repo has no valid name"


uploadFile :: ReaderT Repo IO ()
uploadFile = do
  repo <- ask
  manager <- liftIO $ newManager defaultManagerSettings
  req <- parseRequest "http://localhost:8080/deploy"
  resp <- lift $ formDataBody (form repo) req >>=  flip httpLbs manager
  liftIO $ print resp
  where
    form repo = [ partBS "name" (repoName repo), partFileSource "file" (archivePath repo)]

    repoName :: Repo -> BS.ByteString
    repoName repo = BS.pack $ fromJust (name repo)

    archivePath :: Repo -> String
    archivePath repo = fromJust (path repo) ++ ".tar.gz"


runDeploy :: IO ()
runDeploy = do
  repoM <- getRepoDetails
  case repoM of
    Nothing -> putStrLn "failed to get repo details"
    Just repo -> do
      runReaderT archiveFiles repo
      runReaderT uploadFile repo
      putStrLn "success"


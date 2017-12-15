{-# LANGUAGE OverloadedStrings #-}

module Deploy.Initiate.Core where

import           Control.Exception.Safe                (IOException, catchIO)
import           Control.Monad.Except
import           Control.Monad.Trans.Reader
import           Data.ByteString.Char8                 as BS hiding (getLine,
                                                              putStrLn)
import           Deploy.Types                          (Repo (..))
import           Network.HTTP.Client                   (defaultManagerSettings,
                                                        httpLbs, newManager,
                                                        parseRequest)
import           Network.HTTP.Client.MultipartFormData (formDataBody, partBS,
                                                        partFileSource)
import           System.Directory                      (getCurrentDirectory)
import           System.Process                        (callCommand,
                                                        readCreateProcess,
                                                        shell)

handlerIO :: IOException -> IO ()
handlerIO ex = putStrLn $ "caught exception " ++ show ex

getRepoDetails :: IO (Maybe Repo)
getRepoDetails = do
  repoPath <- liftIO getCurrentDirectory
  repoName <- getLine
  pure $ Just (Repo repoName repoPath)

archiveFiles :: ReaderT Repo IO ()
archiveFiles = do
  repo <- ask
  liftIO $ catchIO (callCommand ("git archive --format=tar.gz --output" ++ path repo ++ ".tar.gz master ")) handlerIO


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
    repoName repo = BS.pack $ name repo

    archivePath :: Repo -> String
    archivePath repo = path repo ++ ".tar.gz"


runDeploy :: IO ()
runDeploy = do
  repo <- getRepoDetails
  case repo of
    Nothing -> putStrLn "failed to get repo details"
    Just repo -> do
      runReaderT archiveFiles repo
      runReaderT uploadFile repo
      putStrLn "success"


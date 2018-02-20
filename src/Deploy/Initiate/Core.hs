module Deploy.Initiate.Core (runDeploy) where

import           Control.Exception.Safe                (catchIO)
import           Data.ByteString.Char8                 as BS hiding (getLine,
                                                              putStrLn)
import           Data.Maybe
import           Data.String                           (String)
import           Data.Text                             as T
import           Deploy.Types                          (Repo (..))
import           Deploy.Util                           (handlerIO)
import           Network.HTTP.Client                   (defaultManagerSettings,
                                                        httpLbs, newManager,
                                                        parseRequest)
import           Network.HTTP.Client.MultipartFormData (formDataBody, partBS,
                                                        partFileSource)
import           Protolude
import           System.Directory                      (getCurrentDirectory)
import           System.Process                        (callCommand)


getRepoDetails :: IO (Maybe Repo)
getRepoDetails = do
  repoPath <- liftIO getCurrentDirectory
  print "enter repo name: "
  repoName <- getLine --- TODO: get from package.json or config file
  pure $ Just (Repo (Just $ T.unpack repoName) (Just repoPath))

archiveFiles :: ReaderT Repo IO ()
archiveFiles = do
  repo <- ask
  case path repo of
    Just filePath -> do
      let archiveCmd = "git archive --format=tar.gz --output " ++ filePath ++ ".tar.gz master"
      liftIO $ catchIO (callCommand archiveCmd) handlerIO
    Nothing -> liftIO $ print "repo has no valid name"

-- TODO: add progress bar
uploadFile :: ReaderT Repo IO ()
uploadFile = do
  repo <- ask
  manager <- liftIO $ newManager defaultManagerSettings
  req <- parseRequest "http://88.80.186.143:8888/upload"
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
      print "uploaded file success"


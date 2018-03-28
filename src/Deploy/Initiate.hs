-- | Initates project upload to the deploy service
-- - get a list of files / folders to bundle up using git if no config with specified file upload exist.
-- - When a files or files are specified for upload, they are not archived by git.

module Deploy.Initiate (runDeploy) where

import           Protolude                    hiding (Text)

import           Data.String                  (String)
import           Data.Text
import           Deploy.Types                 (DeployError (..), RepoInit (..))
import           Dhall                        hiding (Text)
import           Network.HTTP.Client          (Request (..),
                                               defaultManagerSettings, httpLbs,
                                               newManager, parseRequest)
import           Network.HTTP.Client.Internal (StreamFileStatus (..),
                                               observedStreamFile)
import           System.Directory             (getCurrentDirectory)
import           System.IO.Temp               (createTempDirectory,
                                               getCanonicalTemporaryDirectory)
import           System.Posix.Files           (fileExist)
import           System.Process               (callCommand)
import           Util                         as U (split)

import qualified Data.Text.Lazy               as TL

-- | read in config file if exists
getConfig :: String -> IO (Maybe RepoInit)
getConfig repoPath = do
  let configPath = repoPath ++ "/deploy.dhall"
  hasConfigFile <- liftIO $ fileExist configPath
  if hasConfigFile then
    input auto (TL.pack configPath) >>= pure . Just
    else pure Nothing

-- | Gets repo's path
getRepoDetails :: IO (Maybe RepoInit)
getRepoDetails = do
  path     <- liftIO getCurrentDirectory
  confM    <- getConfig path
  case confM of
    Nothing   -> liftIO $ throwIO  MissingConfigError
    Just conf -> pure $ RepoInit
              <$> Just (name conf <|> dirName path)
              <*> Just (files conf)
              <*> Just (deployIP conf)
  where
    dirName :: String -> Maybe Text
    dirName   = lastMay . fmap pack . U.split '/'


-- | zip files in file list or default to zipping files under git
-- | TODO: for git based uploads, only upload what has changed
-- | perf consideration turn upload into a binary and upload chunks
archiveFiles :: ReaderT RepoInit IO String
archiveFiles = do
  repo <- ask
  case name repo of
    Nothing   -> liftIO $ throwIO NoRepoName
    Just name ->
      case files repo of
        Just files -> liftIO $ gzipFiles (unpack name) files
        Nothing    -> liftIO $ gitArchiveFiles (unpack name)

  where
    createRepoTmpDir :: String -> IO String
    createRepoTmpDir name =
      getCanonicalTemporaryDirectory >>= (`createTempDirectory` name)
    gitArchiveFiles :: String -> IO String
    gitArchiveFiles name = do
      let archiveCmd tempPath = "git archive --format=tar.gz --output " <> tempPath <> ".tar.gz master"
      repoTmpDir <- createRepoTmpDir name
      callCommand $ archiveCmd repoTmpDir
      pure $ repoTmpDir <> ".tar.gz"
    gzipFiles :: String -> [Text] -> IO String
    gzipFiles name files = do
        repoTmpDir <- createRepoTmpDir name
        mapM_ (\x -> callCommand $ "cp -a -R " <> unpack x <> " " <> repoTmpDir) files
        callCommand $ "tar -zcvf " <> name <> ".tar.gz" <> repoTmpDir
        pure $ repoTmpDir ++ ".tar.gz"

uploadFile :: String -> ReaderT RepoInit IO ()
uploadFile archivePath = do
  repo <- ask
  let address = deployIP repo
  manager <- liftIO $ newManager defaultManagerSettings
  case name repo of
    Nothing -> throwIO NoRepoName
    Just name -> do
      initReq <- parseRequest $ unpack address ++ "/upload/" ++ unpack name
      body    <- liftIO $ observedStreamFile streamingStatus archivePath
      let req = initReq {requestBody = body, method = "POST"}
      resp <- liftIO (httpLbs req manager)
      liftIO $ print resp
  where
    streamingStatus :: StreamFileStatus -> IO ()
    streamingStatus status =
      let progress = (readSoFar status `div` fileSize status ) * 100
      in print $  "\n upload progress so far: " <> (show progress :: Text) <> "%"

-- | archive project and upload it upstream
runDeploy :: IO ()
runDeploy = do
  repoM <- getRepoDetails
  case repoM of
    Nothing   ->  throwIO RepoDetailsConfigError
    Just repo -> do
      archivePath <- runReaderT archiveFiles repo
      runReaderT (uploadFile archivePath) repo
      print ("uploaded file success" :: Text)

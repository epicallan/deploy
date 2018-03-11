module Deploy.Initiate.Core (runDeploy) where

import           Protolude                    hiding (Text)

import           Data.String                  (String)
import           Data.Text
import qualified Data.Text.Lazy               as TL
import           Deploy.Types                 (Repo (..))
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

-- | read in config file if exists
-- | Gets repo's path
-- | get repo name from current directory's name or from config name

data IniateError =
    MissingConfigError
  | NoRepoName
  | MissingDeloyIPError
  | RepoDetailsConfigError deriving (Show)

instance Exception  IniateError

getConfig :: String -> IO (Maybe Repo)
getConfig repoPath = do
  let configPath = repoPath ++ "/deploy.dhall"
  hasConfigFile <- liftIO $ fileExist configPath
  if hasConfigFile then
    input auto (TL.pack configPath) >>= pure . Just
    else pure Nothing

getRepoDetails :: IO (Maybe Repo)
getRepoDetails = do
  path     <- liftIO getCurrentDirectory
  confM    <- getConfig path
  case confM of
    Nothing   -> liftIO $ throwIO  MissingConfigError
    Just conf -> pure $ Repo
              <$> Just (repoName conf <|> dirName path)
              <*> Just mempty
              <*> Just (repoFiles conf)
              <*> Just (deployIP conf)
  where
    dirName :: String -> Maybe Text
    dirName   = lastMay . fmap pack . U.split '/'


-- | zip files in file list or default to zipping files under git
-- | TODO: for git based uploads, only upload what has changed
-- | perf consideration turn upload into a binary and upload chunks
archiveFiles :: ReaderT Repo IO String
archiveFiles = do
  repo <- ask
  case repoName repo of
    Nothing       -> liftIO $ throwIO NoRepoName
    Just name ->
      case repoFiles repo of
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

-- TODO: add progress bar
uploadFile :: String -> ReaderT Repo IO ()
uploadFile archivePath = do
  repo <- ask
  manager <- liftIO $ newManager defaultManagerSettings
  case deployIP repo of
    Nothing -> throwIO MissingDeloyIPError
    Just address -> do
      initReq <- parseRequest $ unpack address ++ "/upload"
      body    <- liftIO $ observedStreamFile streamingStatus archivePath
      let req = initReq {requestBody = body, method = "POST"}
      resp <- liftIO (httpLbs req manager)
      liftIO $ print resp
  where
    streamingStatus :: StreamFileStatus -> IO ()
    streamingStatus status =
      print $  "\n read so far: " <> (show (readSoFar status) :: Text)

-- cleanup ::  ReaderT Repo IO ()

runDeploy :: IO ()
runDeploy = do
  repoM <- getRepoDetails
  case repoM of
    Nothing   ->  throwIO RepoDetailsConfigError
    Just repo -> do
      archivePath <- runReaderT archiveFiles repo
      runReaderT (uploadFile archivePath) repo
      print ("uploaded file success" :: Text)

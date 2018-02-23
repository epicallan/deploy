module Deploy.Initiate.Core (runDeploy) where

import qualified Data.ByteString.Char8                 as BS hiding (getLine,
                                                              putStrLn)
import           Data.Maybe
import           Data.String                           (String)
import           Data.Text
import qualified Data.Text.Lazy                        as TL
import           Deploy.Types                          (Repo (..))
import           Dhall                                 hiding (Text)
import           Network.HTTP.Client                   (defaultManagerSettings,
                                                        httpLbs, newManager,
                                                        parseRequest)
import           Network.HTTP.Client.MultipartFormData (formDataBody, partBS,
                                                        partFileSource)
import           Protolude                             hiding (Text)
import           System.Directory                      (getCurrentDirectory)
import           System.IO.Temp
import           System.Posix.Files                    (fileExist)
import           System.Process                        (callCommand)
import           Util                                  as U (split)

-- | read in config file if exists
-- | Gets repo's path
-- | get repo name from current directory's name or from config name

data IniateError =
    NoRepoPathError
  | MissingConfigError
  | RepoDetailsConfigError deriving (Show)

instance Exception  IniateError

getConfig :: String -> IO (Maybe Repo)
getConfig repoPath = do
  hasConfigFile <- liftIO $ fileExist (repoPath ++ "/deploy-conf.dhall")
  if hasConfigFile then
    input auto (TL.pack $ repoPath ++ "/deploy-conf") >>= pure . Just
    else pure Nothing

getRepoDetails :: IO (Maybe Repo)
getRepoDetails = do
  path     <- liftIO getCurrentDirectory
  confM    <- getConfig path
  case confM of
    Nothing   -> liftIO $ throwIO  MissingConfigError
    Just conf -> pure $ Repo
              <$> Just (repoName conf <|> dirName path)
              <*> Just (repoPath conf <|> Just (pack path))
              <*> Just (repoFiles conf)
              <*> Just (deployIP conf)
  where
    dirName :: String -> Maybe Text
    dirName   = lastMay . fmap pack . U.split '/'


-- | zip files in file list or default to zipping files under git
-- | TODO: for git based uploads, only upload what has chanhed
-- | perf consideration turn upload into a binary and upload chunks
archiveFiles :: ReaderT Repo IO ()
archiveFiles = do
  repo <- ask
  case repoPath repo of
    Nothing       -> liftIO $ throwIO NoRepoPathError
    Just filePath ->
      case repoFiles repo of
        Just files -> liftIO $ zipFiles files
        Nothing    -> liftIO $ gitArchive $ unpack filePath

  where
    createRepoTmpDir repoName =
      getCanonicalTemporaryDirectory >>= (\sysTmpDir -> createTempDirectory sysTempDir repoName)
    gitArchiveFiles = do
      let archiveCmd tempPath = "git archive --format=tar.gz --output " <> tempPath <> ".tar.gz master"
      repoTmpDir <- createRepoTmpDirpoName
      callCommand $ archiveCmd repoTmpDir
    gzipFiles :: Text -> [Text] -> IO ()
    gzipFiles repoName files = do
        repoTmpDir <- createRepoTmpDirpoName
        mapM_ (\x -> callCommand $ "cp -a -R " <> unpack x <> " " <> repoTmpDir) files
        callCommand "tar -zcvf " <> repoName ".tar.gz" <> repoTmpDir

-- TODO: add progress bar
uploadFile :: ReaderT Repo IO ()
uploadFile = do
  repo <- ask
  manager <- liftIO $ newManager defaultManagerSettings
  req <- parseRequest "http://88.80.186.143:8888/upload" -- TODO: should be an option
  resp <- lift $ formDataBody (form repo) req >>=  flip httpLbs manager
  liftIO $ print resp
  where
    form repo = [ partBS "name" (name repo), partFileSource "file" (archivePath repo)]

    name :: Repo -> BS.ByteString
    name repo = encodeUtf8 $ fromJust (repoName repo)

    archivePath :: Repo -> String
    archivePath repo = unpack $ fromJust (repoPath repo) <> ".tar.gz"

-- cleanup ::  ReaderT Repo IO ()

runDeploy :: IO ()
runDeploy = do
  repoM <- getRepoDetails
  case repoM of
    Nothing   ->  throwIO RepoDetailsConfigError
    Just repo -> do
      runReaderT archiveFiles repo
      runReaderT uploadFile repo
      print ("uploaded file success" :: Text)

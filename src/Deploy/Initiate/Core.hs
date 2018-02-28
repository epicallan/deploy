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
import           System.IO.Temp                        (createTempDirectory, getCanonicalTemporaryDirectory)
import           System.Posix.Files                    (fileExist)
import           System.Process                        (callCommand)
import           Util                                  as U (split)

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
              <*> Nothing
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
      pure $ repoTmpDir <> ".tar.gz master"
    gzipFiles :: String -> [Text] -> IO String
    gzipFiles name files = do
        repoTmpDir <- createRepoTmpDir name
        mapM_ (\x -> callCommand $ "cp -a -R " <> unpack x <> " " <> repoTmpDir) files
        callCommand $ "tar -zcvf " <> name <> ".tar.gz" <> repoTmpDir
        pure $ repoTmpDir ++ ".tar.gz master"

-- TODO: add progress bar
uploadFile :: String -> ReaderT Repo IO ()
uploadFile archivePath = do
  repo <- ask
  manager <- liftIO $ newManager defaultManagerSettings
  case deployIP repo of
    Nothing -> throwIO MissingDeloyIPError
    Just address -> do
      req <- parseRequest $ unpack address ++ "/upload"
      --  Request -> Manager -> (Response BodyReader -> IO a) -> IO a
      -- withResponce (formDataBody (form repo))
      resp <- lift $ formDataBody (form repo) req >>=  flip httpLbs manager
      liftIO $ print resp
  where
    form repo = [ partBS "name" (name repo), partFileSource "file" archivePath]

    name :: Repo -> BS.ByteString
    name repo = encodeUtf8 $ fromJust (repoName repo)

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

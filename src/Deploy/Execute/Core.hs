{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Deploy.Execute.Core (
    buildContainer
  , runContainer
  , unarchiveFile) where
import           Control.Monad.Catch        (MonadMask)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Reader
import           Data.Maybe
import           Data.Text                  (pack)
import           Deploy.Types               (Repo (..))
import           Docker.Client              hiding (name, path)
import           System.Directory           (getHomeDirectory)
import           System.Process             (callCommand)


unarchiveFile :: ReaderT Repo IO ()
unarchiveFile = do
  repo <- ask
  let filePath           = fromJust $ path repo -- TODO: refactor
  let repoName           = fromJust $ name repo
  let repoFilePath base  = base ++ repoName ++ "/" ++ repoName ++ ".tar.gz "
  homePath <- liftIO getHomeDirectory
  liftIO $ callCommand ("mkdir " ++ homePath ++ repoName)
  liftIO $ callCommand ("mv "++ filePath ++ " " ++ repoFilePath homePath)
  liftIO $ callCommand ("tar xzvf " ++ repoFilePath homePath ++ " -C "  ++ "~/" ++ repoName)


runDocker ::(MonadMask m, MonadIO m) => DockerT m b -> m b
runDocker f = do
  h <- unixHttpHandler "/var/run/docker.sock" -- this file in the docker container is linked to the one on the host
  runDockerT (defaultClientOpts, h) f


buildContainer :: ReaderT Repo IO (Either DockerError ContainerID)
buildContainer  = do
  repo <- ask
  let  repoName    = fromJust $ name repo
  let  imageName   = pack $ repoName ++  ":latest"
  runDocker $ do
    ev <- getDockerVersion
    case ev of
      Left err -> return $ Left err
      Right v -> do
        liftIO $ putStrLn $ "host docker version: " ++ show v
        ctxDir <- liftIO ((++ ("/" ++ repoName)) <$> getHomeDirectory)
        liftIO $ putStrLn $ "ctxDir: " ++ ctxDir
        buildImageFromDockerfile (defaultBuildOpts imageName) ctxDir
        createContainer (defaultCreateOpts imageName) (Just $ pack repoName)


runContainer :: ContainerID -> IO (Either DockerError ())
runContainer containerId' =liftIO $
  runDocker $ startContainer defaultStartOpts containerId'

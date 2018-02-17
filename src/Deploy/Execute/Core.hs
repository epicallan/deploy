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
import           Data.Text                  (Text, pack)
import           Deploy.Types               (Repo (..))
import           Docker.Client              hiding (name, path)
import           System.Process             (callCommand)


unarchiveFile :: ReaderT Repo IO ()
unarchiveFile = do
  repo <- ask
  let filePath = fromJust $ path repo -- TODO: refactor
  let repoName = fromJust $ name repo
  liftIO $ callCommand ("mkdir " ++ "/" ++ repoName)
  liftIO $ callCommand ("mv "++ filePath ++ " " ++ repoFilePath repoName)
  liftIO $ callCommand ("tar xzvf " ++ repoFilePath repoName )
  where
    repoFilePath repoName = "/" ++ repoName ++ "/" ++ repoName ++ ".tar.gz "

runDocker ::(MonadMask m, MonadIO m) => DockerT m b -> m b
runDocker f = do
  h <- unixHttpHandler "/var/run/docker.sock" -- this file in the docker container is linked to the one on the host
  runDockerT (defaultClientOpts, h) f


buildContainer :: ReaderT Repo IO (Either DockerError ContainerID)
buildContainer  = do
  repo <- ask
  let repoName    = fromJust $ name repo
  let imageName   =  pack $ repoName ++  ":latest"
  let  buildOpts  = defaultBuildOpts imageName
  let  dockerfilePath  =  "/" ++ repoName ++ "/" ++ repoName
  runDocker $ do
    buildImageFromDockerfile buildOpts dockerfilePath
    createContainer (defaultCreateOpts imageName) Nothing


runContainer :: ContainerID -> IO (Either DockerError ())
runContainer containerId' =liftIO $
  runDocker $ startContainer defaultStartOpts containerId'

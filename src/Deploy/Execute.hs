-- | Executes dockerfile or docker-compose script in upload
module Deploy.Execute (
    buildContainer
  , runContainer
  , unarchiveFile) where

import           Protolude

import           Control.Exception.Safe (MonadMask)
import           Data.Text              (unpack)
import           Deploy.Types           (DeployError (..), Repo (..))
import           Docker.Client          hiding (name, path)
import           System.Process         (callCommand)


unarchiveFile :: ReaderT Repo IO ()
unarchiveFile = do
  repo <- ask
  case uploadPath repo of
    Nothing -> liftIO $ throwIO  MissingUploadPath
    Just filePath ->
        liftIO $ callCommand (unpack $ "tar xzvf " <> filePath)


runDocker ::(MonadMask m, MonadIO m) => DockerT m b -> m b
runDocker f = do
  h <- unixHttpHandler "/var/run/docker.sock" -- this file in the docker container is linked to the one on the host
  runDockerT (defaultClientOpts, h) f

-- | builds docker container from dockerfile, we publish all exposed ports in dockerfile on host
-- TODO: we could probably provide an option in the config file for listing the ports to publish
buildContainer :: ReaderT Repo IO (Either DockerError ContainerID)
buildContainer  = do
  repo <- ask
  case repoName repo of
    Nothing ->  liftIO $ throwIO  NoRepoName
    Just name -> do
      let  imageName = name <> ":latest"
      runDocker $ do
        eResult <- buildImageFromDockerfile (defaultBuildOpts imageName) ("/uploads" <> unpack name)
        case eResult of
          Left err -> pure $  Left err
          Right _  -> createContainer (createOptions imageName) (Just name)
  where
    createOptions :: Text -> CreateOpts
    createOptions imageName =
      let cHostConfig = defaultHostConfig { publishAllPorts = True }
      in CreateOpts { containerConfig = defaultContainerConfig imageName, hostConfig = cHostConfig }

runContainer :: ContainerID -> IO (Either DockerError ())
runContainer dContainerId = liftIO $
  runDocker $ startContainer defaultStartOpts dContainerId

-- | Executes dockerfile or docker-compose script in upload
module Deploy.Execute (executeDeploy) where

import           Protolude

import           Control.Exception.Safe   (MonadMask)
import           Data.String              (String)
import           Data.Text                (unpack)
import           Deploy.Types             (RepoEx (..))
import           Docker.Client            hiding (name, path)
import           System.Directory         (removeDirectoryRecursive)
import           System.Process           (callCommand)

import           Deploy.Parser.Dockerfile (exposedPort)

unarchiveFile :: ReaderT RepoEx IO ()
unarchiveFile = do
  repo <- ask
  let archivePath = rxArchivePath repo
  let uploadPath = rxUploadPath repo
  liftIO $ callCommand $ unpack $ "tar xzvf " <> archivePath <> " -C " <> uploadPath


runDocker ::(MonadMask m, MonadIO m) => DockerT m b -> m b
runDocker f = do
  h <- unixHttpHandler "/var/run/docker.sock" -- this file in the docker container is linked to the one on the host
  runDockerT (defaultClientOpts, h) f

-- | builds docker container from dockerfile, we publish all exposed ports in dockerfile on host
-- TODO: we could probably provide an option in the config file for listing the ports to publish
buildContainer :: ReaderT RepoEx IO (Either DockerError ContainerID)
buildContainer  = do
  repo <- ask
  let name           = rxName repo
  let uploadFilePath = rxUploadPath repo
  let imageName      = name <> ":latest"
  runDocker $ do
    cPort   <- liftIO $ getContainerPort uploadFilePath
    -- print $ ("port: " :: Text) <> show cPort
    eResult <- buildImageFromDockerfile (defaultBuildOpts imageName) (unpack uploadFilePath)
    case eResult of
      Left err -> pure $  Left err
      -- TODO: should delete prev container
      Right _  -> createContainer (createOptions imageName cPort) Nothing
  where
    createOptions :: Text -> Integer -> CreateOpts
    createOptions imageName port = defaultCreateOpts imageName
                              & addPortBinding (PortBinding port TCP [HostPort "0.0.0.0" port])
                              & addExposedPort (ExposedPort port TCP)


-- | gets the exposed port in a docker file
getContainerPort :: Text -> IO Integer
getContainerPort dockerContextPath = do
  contents <- readFile dockerfilePath
  pure $ fromMaybe 80 (exposedPort contents)
  where
    dockerfilePath :: String
    dockerfilePath = unpack $ dockerContextPath <> "/Dockerfile"


runContainer :: ContainerID -> IO (Either DockerError ())
runContainer dContainerId = liftIO $
  runDocker $ startContainer defaultStartOpts dContainerId

executeDeploy :: RepoEx -> IO ()
executeDeploy repo = do
  liftIO $ runReaderT unarchiveFile repo
  ebuildResults <- liftIO $ runReaderT buildContainer repo
  case ebuildResults of
      Left err          -> print err
      Right containerId -> do
          erunResult <- liftIO $ runContainer containerId
          -- clean up
          liftIO $ removeDirectoryRecursive (unpack $ rxUploadPath repo)
          -- TDDO: send notification fo successful deployment
          case erunResult of
              Left err ->  print err
              Right _  ->  print $ "Deployed container for " <> rxName repo

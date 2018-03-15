-- | Executes dockerfile or docker-compose script in upload
module Deploy.Execute (
    buildContainer
  , runContainer
  , unarchiveFile) where

import           Protolude

import           Control.Exception.Safe (MonadMask)
import           Data.Text              (unpack)
import           Deploy.Types           (Repo (..))
import           Docker.Client          hiding (name, path)
import qualified Prelude                as P (userError)
-- import           System.Directory           (getHomeDirectory)
import           System.Process         (callCommand)


unarchiveFile :: ReaderT Repo IO ()
unarchiveFile = do
  repo <- ask
  case uploadPath repo of
    Nothing -> liftIO $ ioError $ P.userError (unpack "missing upload path")
    Just filePath ->
        liftIO $ callCommand (unpack $ "tar xzvf " <> filePath)


runDocker ::(MonadMask m, MonadIO m) => DockerT m b -> m b
runDocker f = do
  h <- unixHttpHandler "/var/run/docker.sock" -- this file in the docker container is linked to the one on the host
  runDockerT (defaultClientOpts, h) f

-- TODO: publish the exposed port in docker file
buildContainer :: ReaderT Repo IO (Either DockerError ContainerID)
buildContainer  = do
  repo <- ask
  case repoName repo of
    Nothing -> liftIO $ ioError $ P.userError (unpack "missing repo name")
    Just repoName' -> do
      let  imageName = repoName' <> ":latest"
      runDocker $ do
        eResult <- buildImageFromDockerfile (defaultBuildOpts imageName) ("/uploads" <> unpack repoName')
        case eResult of
          Left err -> pure $  Left err
          Right _  ->  createContainer (defaultCreateOpts imageName) (Just repoName')


runContainer :: ContainerID -> IO (Either DockerError ())
runContainer containerId' =liftIO $
  runDocker $ startContainer defaultStartOpts containerId'

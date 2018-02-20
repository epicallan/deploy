module Deploy.Execute.Core (
    buildContainer
  , runContainer
  , unarchiveFile) where
import           Control.Exception.Safe (MonadMask)
import           Data.Maybe
import           Data.Text              (pack)
import           Deploy.Types           (Repo (..))
import           Docker.Client          hiding (name, path)
import           Protolude
-- import           System.Directory           (getHomeDirectory)
import           System.Process         (callCommand)


--TODO: use environment variabels to change root / base path for local development testing
-- TODO: delete any existing folder with same name as new archive
unarchiveFile :: ReaderT Repo IO ()
unarchiveFile = do
  repo <- ask
  let filePath           = fromJust $ path repo
  let repoName           = fromJust $ name repo
  let repoFilePath       = "/" ++ repoName ++ "/" ++ repoName ++ ".tar.gz "
  liftIO $ callCommand ("mkdir " ++ "/" ++ repoName)
  liftIO $ callCommand ("mv "++ filePath ++ " " ++ repoFilePath)
  liftIO $ callCommand ("tar xzvf " ++ repoFilePath ++ " -C "  ++ "/" ++ repoName)


runDocker ::(MonadMask m, MonadIO m) => DockerT m b -> m b
runDocker f = do
  h <- unixHttpHandler "/var/run/docker.sock" -- this file in the docker container is linked to the one on the host
  runDockerT (defaultClientOpts, h) f

-- TODO: publish the exposed port in docker file
buildContainer :: ReaderT Repo IO (Either DockerError ContainerID)
buildContainer  = do
  repo <- ask
  let  repoName    = fromJust $ name repo
  let  imageName   = pack $ repoName ++  ":latest"
  runDocker $ do
    buildImageFromDockerfile (defaultBuildOpts imageName) ("/" ++ repoName)
    createContainer (defaultCreateOpts imageName) (Just $ pack repoName)


runContainer :: ContainerID -> IO (Either DockerError ())
runContainer containerId' =liftIO $
  runDocker $ startContainer defaultStartOpts containerId'

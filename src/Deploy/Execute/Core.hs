module Deploy.Execute.Core (
    buildContainer
  , runContainer
  , unarchiveFile) where
import           Control.Exception.Safe (MonadMask)
import           Data.Maybe
import           Data.Text              (unpack)
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
  let filePath    = fromJust $ uploadPath repo
  let rname       = fromJust $ repoName repo
  let rpath       = "/" <> rname <> "/" <> rname <> ".tar.gz "
  liftIO $ callCommand (unpack $ "mkdir " <> "/" <> rname)
  liftIO $ callCommand (unpack $ "mv " <> filePath <> " " <> rpath)
  liftIO $ callCommand (unpack $ "tar xzvf " <> rpath <> " -C "  <> "/" <> rname)


runDocker ::(MonadMask m, MonadIO m) => DockerT m b -> m b
runDocker f = do
  h <- unixHttpHandler "/var/run/docker.sock" -- this file in the docker container is linked to the one on the host
  runDockerT (defaultClientOpts, h) f

-- TODO: publish the exposed port in docker file
buildContainer :: ReaderT Repo IO (Either DockerError ContainerID)
buildContainer  = do
  repo <- ask
  let  rname    = fromJust $ repoName repo
  let  imageName = rname <> ":latest"
  runDocker $ do
    eResult <- buildImageFromDockerfile (defaultBuildOpts imageName) ("/" <> unpack rname)
    case eResult of
      Left err -> pure $  Left err
      Right _  ->  createContainer (defaultCreateOpts imageName) (repoName repo)



runContainer :: ContainerID -> IO (Either DockerError ())
runContainer containerId' =liftIO $
  runDocker $ startContainer defaultStartOpts containerId'

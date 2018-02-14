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
  let filePath = fromJust $ path repo
  let repoName = fromJust $ name repo
  liftIO $ callCommand ("mkdir " ++ repoDir repoName)
  liftIO $ callCommand ("mv "++ filePath ++ " " ++ repoFilePath repoName)
  liftIO $ callCommand ("tar xzvf " ++ repoFilePath repoName )
  where
  repoDir repoName = "/Users/allan/" ++ repoName
  repoFilePath repoName = repoDir repoName ++ "/" ++ repoName ++ ".tar.gz "


runDocker ::(MonadMask m, MonadIO m) => DockerT m b -> m b
runDocker f = do
  h <- defaultHttpHandler
  runDockerT (defaultClientOpts, h) f


buildContainer :: ReaderT Repo IO (Either DockerError ContainerID)
buildContainer  = do
  repo <- ask
  runDocker $ do
    buildImageFromDockerfile (buildOpts repo) (dockerfilePath repo)
    createContainer (defaultCreateOpts (imageName repo)) Nothing
  where
    imageName:: Repo -> Text
    imageName repo =  pack $ fromJust (name repo) ++  ":latest"

    buildOpts :: Repo -> BuildOpts
    buildOpts repo = defaultBuildOpts (imageName repo)

    dockerfilePath ::  Repo -> FilePath
    dockerfilePath repo =  "/Users/allan/" ++ fromJust (name repo)


runContainer :: ContainerID -> IO (Either DockerError ())
runContainer containerId' =liftIO $
  runDocker $ startContainer defaultStartOpts containerId'

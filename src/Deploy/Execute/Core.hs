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
import           Data.Text                  (Text, pack, unpack)
import           Deploy.Types               (Repo (..))
import           Docker.Client              hiding (name, path)
import           System.Process             (callCommand, readCreateProcess,
                                             shell)


unarchiveFile :: ReaderT Repo IO ()
unarchiveFile = do
  repo <- ask
  let filePath = fromJust $ path repo
  let repoName = fromJust $ name repo
  liftIO $ callCommand ("mv "++ filePath ++ " /src/" ++ repoName ++ ".tgz")
  liftIO $ callCommand ("tar xzvf /src/" ++ repoName ++ ".tgz")


runDocker ::(MonadMask m, MonadIO m) => DockerT m b -> m b
runDocker f = do
  h <- defaultHttpHandler
  runDockerT (defaultClientOpts, h) f


buildContainer :: ReaderT Repo IO (Either DockerError ContainerID)
buildContainer  = do
  repo <- ask
  runDocker $ do
    r <- buildImageFromDockerfile (buildOpts repo) (dockerfilePath repo)
    createContainer (defaultCreateOpts (imageName repo)) Nothing
  where
    imageName:: Repo -> Text
    imageName repo =  pack $ fromJust (name repo) ++  ":latest"

    buildOpts :: Repo -> BuildOpts
    buildOpts repo = defaultBuildOpts (imageName repo)

    dockerfilePath ::  Repo -> FilePath
    dockerfilePath repo =  "/src" ++ fromJust (name repo)


runContainer :: ContainerID -> IO (Either DockerError ())
runContainer containerId =liftIO $
  runDocker $ startContainer defaultStartOpts containerId

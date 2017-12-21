{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Deploy.Execute.Core where
import           Control.Monad.Catch        (MonadMask)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader
import           Data.Maybe
import           Data.Text                  (Text, pack, unpack)
import           Deploy.Types               (Repo (..))
import           Docker.Client              hiding (name, path)
import           Servant.Multipart          (FileData (..), FromMultipart,
                                             MultipartData (..),
                                             MultipartForm (..), Tmp,
                                             fromMultipart, lookupFile,
                                             lookupInput)
import           System.Process             (callCommand, readCreateProcess,
                                             shell)


{-|
  runContainer :: ReaderT Repo IO
-}


instance FromMultipart Tmp Repo where
  fromMultipart form =
    Just $ Repo (repoName form) (filePath form)
      where
        repoName form = unpack <$> lookupInput "name" form

        filePath form = unpack <$> fmap fdFileName (lookupFile "file" form)



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


buildContainer :: ReaderT Repo IO ContainerID
buildContainer  = do
  repo <- ask
  runDocker $ do
    r <- buildImageFromDockerfile (buildOpts repo) (dockerfilePath repo)
    eresult <- createContainer (defaultCreateOpts "repo:latest") Nothing
    case eresult of
      Left errors -> return 1
      Left id     -> return id
  where
    buildOpts :: Repo -> BuildOpts
    buildOpts repo = defaultBuildOpts (pack $ fromJust (name repo) ++  ":latest")

    dockerfilePath ::  Repo -> FilePath
    dockerfilePath repo =  "/src" ++ fromJust (name repo)
      -- case eitherImage of
      --   Right _ -> createContainer (defaultCreateOpts "repo:latest") Nothing
      --   Left _  -> lift $ putStrLn "failed to create docker image"

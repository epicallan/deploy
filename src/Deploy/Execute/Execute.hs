-- | routes handlers
module Deploy.Execute (deploy) where
import           Control.Monad.IO.Class        (liftIO)
import           Data.Aeson                    (eitherDecode)
import qualified Data.ByteString.Lazy.Internal as B
import           Data.Maybe                    (maybe)
import           Deploy.Types                  (Repo (..))


{-|
  functions;
  unarchiveFile:: FilePath -> ExceptT FileErrors IO ()

  mergeFiles :: ExtraFilesPath -> TreeFilePath ->  ExceptT FileErrors IO ()

  createDockerImage :: ReaderT Repo IO ()

  createDockerContainer :: ReaderT Repo IO

  runContainer :: ReaderT Repo IO

-}

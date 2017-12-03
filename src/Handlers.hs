-- | routes handlers
module Handlers (deploy) where
import           Control.Monad.IO.Class        (liftIO)
import           Data.Aeson                    (eitherDecode)
import qualified Data.ByteString.Lazy.Internal as B
import           Data.Maybe                    (maybe)
import           Types                         (GithubResponse (..), Repo,
                                                Repository (..))
import           Utils                         (getRepo, runDeployBash)

-- for deploy route / deploy route
deploy :: B.ByteString -> [Repo] -> IO ()
deploy jsonRes repos =
    case (eitherDecode jsonRes :: Either String GithubResponse) of
        Left err -> print err
        Right res -> case maybeRepo res of
            Nothing   -> print $ "repo: " ++ show res ++ " not found"
            Just repo -> runDeployBash repo >>= print
    where
        repoName res = name (repository res)
        maybeRepo res =  getRepo (repoName res)  repos

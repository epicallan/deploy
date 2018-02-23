module Deploy.API  (
  startApp
 ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Text                  (pack)
import           Deploy.Execute.Core        (buildContainer, runContainer,
                                             unarchiveFile)
import           Deploy.Types               (Repo (..))
import           Network.Wai.Handler.Warp
import Data.String (String)
import           Protolude
import           Servant
import           Servant.Multipart          (FromMultipart, MultipartForm, Tmp,
                                             fdPayload, fromMultipart, 
                                             lookupFile, lookupInput)



type API = "upload" :> MultipartForm Tmp Repo :> Post '[JSON] String
      :<|> "home" :> Get '[JSON] String


api :: Proxy API
api = Proxy

-- TODO: Increase timeout
-- TODO: upload only changed files
-- TODO: handle async exceptions better
instance FromMultipart Tmp Repo where
  fromMultipart form =
    Just $ Repo (repoName' form) (filePath form) Nothing Nothing
      where
        repoName' = lookupInput "name"

        filePath = (pack . fdPayload <$>) .lookupFile "file"

uploadHandler :: Repo -> Handler String
uploadHandler repo = do
    liftIO $ print repo
    liftIO $ runReaderT unarchiveFile repo
    ebuildResults <- liftIO $ runReaderT buildContainer repo
    case ebuildResults of
      Left err  -> return $ show err
      Right id' -> do
        erunResult <- liftIO $ runContainer id'
        case erunResult of
          Left err -> return $ show err
          Right _  -> return "started container"

home :: Handler String
home = return "Deploy API is live"

routes :: Server API
routes =
  uploadHandler :<|> home

startApp :: IO ()
startApp = run 8888 (serve api routes)

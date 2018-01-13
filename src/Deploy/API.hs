{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Deploy.API  (
  startApp
 ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Text                  (Text, pack, unpack)
import           Deploy.Execute.Core        (buildContainer, runContainer,
                                             unarchiveFile)
import           Deploy.Types               (Repo (..))
import           Docker.Client              hiding (name, path)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Multipart          (FromMultipart, MultipartData (..),
                                             MultipartForm (..), Tmp (..),
                                             fdFileName, fdPayload,
                                             fromMultipart, iName, iValue,
                                             lookupFile, lookupInput)



type API = "upload" :> MultipartForm Tmp Repo :> Post '[JSON] String

api :: Proxy API
api = Proxy


instance FromMultipart Tmp Repo where
  fromMultipart form =
    Just $ Repo (repoName form) (filePath form)
      where
        repoName form = unpack <$> lookupInput "name" form

        filePath form = fmap fdPayload (lookupFile "file" form)

routes :: Server API
routes = uploadHandler

  where uploadHandler :: Repo -> Handler String
        uploadHandler repo = do
          liftIO $ runReaderT unarchiveFile repo
          ebuildResults <- liftIO $ runReaderT buildContainer repo
          case ebuildResults of
            Left err       -> return $ show err
            Right id -> do
              erunResult <- liftIO $ runContainer id
              case erunResult of
                Left err -> return $ show err
                Right _  -> return "started container"


startApp :: IO ()
startApp = run 8080 (serve api routes)

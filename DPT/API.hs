{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Lib
    ( startApp
    , app
    ) where

import           Control.Monad.IO.Class
-- import           Data.Aeson
-- import           Data.Aeson.TH
-- import           Data.List
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Types                    (GithubResponse (..), Repo,
                                           Repository (..), SSHKey (..),
                                           Status (..))
import           Utils                    (addKey)

type API = "deploy" :> ReqBody '[JSON] GithubResponse :> Post '[JSON] Repository
    :<|> "addSSHKey" :> ReqBody '[JSON] SSHKey  :> Post '[JSON] Status

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = deploy
    :<|> addSSHKey

    where   deploy :: GithubResponse -> Handler Repository
            deploy resp = do
                liftIO $ print resp
                return $ Repository "allan" "hey"
            --- change to add repository
            addSSHKey :: SSHKey -> Handler Status
            addSSHKey (SSHKey _ key) = do
                liftIO $ addKey key
                return $ Status "success" "repo key added"

            --- getDeploySSHKey

            ---

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Deploy.API  (
  startApp
, upload
 ) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Multipart        (MultipartData (..),
                                           MultipartForm (..), Tmp (..),
                                           fdFileName, fdPayload, iName, iValue)

import qualified Data.ByteString.Lazy     as LBS


type API = "upload" :> MultipartForm Tmp (MultipartData Tmp) :> Post '[JSON] Integer

api :: Proxy API
api = Proxy

-- this is a stub, to be replaced
upload :: Server API
upload multipartData = do
  liftIO $ do
    putStrLn "Inputs:"
    forM_ (inputs multipartData) $ \input ->
      putStrLn $ "  " ++ show (iName input)
            ++ " -> " ++ show (iValue input)

    forM_ (files multipartData) $ \file -> do
      let content = fdPayload file
      putStrLn $ "Content of " ++ show (fdFileName file)
      LBS.putStr content
  return 0

startApp :: IO ()
startApp = run 8080 (serve api upload)

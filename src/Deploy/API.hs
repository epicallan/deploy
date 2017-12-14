{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Deploy.API where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Text.Encoding                    (encodeUtf8)
import           Network                               (withSocketsDo)
import           Network.HTTP.Client                   hiding (Proxy)
import           Network.HTTP.Client.MultipartFormData
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Multipart

import qualified Data.ByteString.Lazy                  as LBS


type API = "upload" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] Integer

api :: Proxy API
api = Proxy


-- MultipartData consists in textual inputs,
-- accessible through its "inputs" field, as well
-- as files, accessible through its "files" field.
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

startServer :: IO ()
startServer = run 8080 (serve api upload)

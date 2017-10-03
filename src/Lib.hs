
{-# LANGUAGE OverloadedStrings #-}

module Lib (isDomainUp) where

import           Control.Exception   (SomeException, try)
import           Network.HTTP.Simple (Request, getResponseStatusCode, httpLBS)
import           Prelude

isDomainUp :: Request -> IO (Either String String)
isDomainUp url = do
    eresponse <- try $ httpLBS url
    return $ case eresponse of
        Left err -> Left $ show (err :: SomeException)
        Right response  -> checkStatus (getResponseStatusCode response :: Int)
            where
                checkStatus statusCode
                    | statusCode == 200 = Right "site is live"
                    | statusCode == 400  = Right "site is down"
                    | otherwise = Right "site might be down, please check it"

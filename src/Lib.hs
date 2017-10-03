
{-# LANGUAGE OverloadedStrings #-}

module Lib (
        isDomainUp
    ,   runDeployBash
    ,   readConf) where
import           Control.Exception    (SomeException, try)
import           Data.Aeson           (eitherDecode)
import qualified Data.ByteString.Lazy as B
import           Data.Maybe           (fromMaybe)
import qualified Data.Text            as T
import           Data.Text.IO         (hGetContents)

import           Network.HTTP.Simple  (Request, getResponseStatusCode, httpLBS)
import           Prelude
import           System.Process       (CreateProcess (..),
                                       StdStream (CreatePipe), createProcess,
                                       proc)
import           Types                (AppConf)

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

readConf :: FilePath -> IO (Either String AppConf)
readConf jsonFile = do
    bJson <- B.readFile jsonFile
    return (eitherDecode bJson :: Either String AppConf)


runDeployBash :: String -> Maybe [String] -> IO T.Text
runDeployBash bashCmds args = do
    (_, outHandle, _, _) <- createProcess (proc bashCmds $ fromMaybe [] args ){ std_out = CreatePipe }
    case outHandle of
        Nothing     -> return "Failed to get handle, command possibly didnt run"
        Just handle -> hGetContents handle

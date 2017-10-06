
{-# LANGUAGE OverloadedStrings #-}

module Lib (
        isDomainUp
    ,   runDeployBash
    ,   getRepo
    ,   readRepos
    ,   sendEmail
    ) where
import           Control.Exception          (SomeException, try)
import           Control.Monad.Trans.Reader
import           Data.Aeson                 (eitherDecode)
import qualified Data.ByteString.Lazy       as B
import           Data.List                  (find)
import           Data.Maybe                 (fromMaybe, maybe)
import qualified Data.Text                  as T
import           Data.Text.IO               (hGetContents)
import qualified Data.Text.Lazy             as TL
import           Network.HaskellNet.Auth    (AuthType (PLAIN))
import           Network.HaskellNet.SMTP    (authenticate, doSMTPPort,
                                             sendPlainTextMail)
import           Network.HTTP.Simple        (Request, getResponseStatusCode,
                                             httpLBS)
import           Prelude
import           System.Process             (CreateProcess (..),
                                             StdStream (CreatePipe),
                                             createProcess, shell)
import           Types                      (EmailConf (..), Repo (..))

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

-- TODO: should come from a data store
readRepos :: FilePath -> IO (Either String [Repo])
readRepos jsonFile = do
    bJson <- B.readFile jsonFile
    return (eitherDecode bJson :: Either String [Repo])

-- get repo; this should be a search in the db. currently we will look in a json file
getRepo :: String -> [Repo] -> Maybe Repo
getRepo reponame = find (\repo -> name repo == reponame)

runDeployBash :: Repo -> IO T.Text
runDeployBash repo = do
    (_, outHandle, _, _) <- createProcess (shell $ getCmds repo ){ std_out = CreatePipe }
    case outHandle of
        Nothing     -> return "Failed to get handle, command possibly didnt run"
        Just handle -> hGetContents handle
    where
        getCmds repo = T.unpack $ bashScript repo


-- send email
sendEmail :: EmailConf -> IO ()
sendEmail conf = doSMTPPort (smtpServer conf) (smtpPort conf) $ \conn -> do
    authSucceed <- authenticate PLAIN (email conf) (password conf) conn
    if authSucceed
        then sendPlainTextMail "receiver@server.com" "sender@server.com" "subject" (TL.pack "Hello! This is the mail body!") conn
        else print "Authentication failed."

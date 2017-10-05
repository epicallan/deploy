
{-# LANGUAGE OverloadedStrings #-}

module Lib (
        isDomainUp
    ,   runDeployBash
    ) where
import           Control.Exception          (SomeException, try)
import           Control.Monad.Trans.Reader
import           Data.Aeson                 (eitherDecode)
import qualified Data.ByteString.Lazy       as B
import           Data.List                  (find)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T
import           Data.Text.IO               (hGetContents)
import qualified Data.Text.Lazy             as TL
import           Network.HaskellNet.Auth
import           Network.HaskellNet.SMTP
import           Network.HTTP.Simple        (Request, getResponseStatusCode,
                                             httpLBS)
import           Prelude
import           System.Process             (CreateProcess (..),
                                             StdStream (CreatePipe),
                                             createProcess, proc)
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

-- readConf :: FilePath -> IO (Either String AppConf)
-- readConf jsonFile = do
--     bJson <- B.readFile jsonFile
--     return (eitherDecode bJson :: Either String AppConf)


runDeployBash :: String -> Maybe [String] -> IO T.Text
runDeployBash bashCmds args = do
    (_, outHandle, _, _) <- createProcess (proc bashCmds $ fromMaybe [] args ){ std_out = CreatePipe }
    case outHandle of
        Nothing     -> return "Failed to get handle, command possibly didnt run"
        Just handle -> hGetContents handle



-- send email
sendEmail :: EmailConf -> IO ()
sendEmail conf = doSMTPPort (smtpServer conf) 587 $ \conn -> do
    authSucceed <- authenticate PLAIN (email conf) (password conf) conn
    if authSucceed
        then sendPlainTextMail "receiver@server.com" "sender@server.com" "subject" (TL.pack "Hello! This is the mail body!") conn
        else print "Authentication failed."


-- get repo; this should be a search in the db. currently we will look in a json file
getRepoToDeploy :: String -> [Repo] -> Maybe Repo
getRepoToDeploy reponame = find (\(Repo name _ _ ) -> name == reponame)

-- stream bash output
--- handle -> IO Either String String

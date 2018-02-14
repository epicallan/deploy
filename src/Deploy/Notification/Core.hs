
{-# LANGUAGE OverloadedStrings #-}

module Deploy.Notification.Core (
        isDomainUp
    ,   sendEmail
    ) where
import           Control.Exception         (SomeException, try)
import qualified Data.Text.Lazy            as TL
import           Deploy.Notification.Types (EmailConf (..))
import           Network.HaskellNet.Auth   (AuthType (PLAIN))
import           Network.HaskellNet.SMTP   (authenticate, doSMTPPort,
                                            sendPlainTextMail)
import           Network.HTTP.Simple       (Request, getResponseStatusCode,
                                            httpLBS)
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


-- send email
sendEmail :: EmailConf -> IO ()
sendEmail conf = doSMTPPort (smtpServer conf) (smtpPort conf) $ \conn -> do
    authSucceed <- authenticate PLAIN (email conf) (password conf) conn
    if authSucceed
        then sendPlainTextMail "receiver@server.com" "sender@server.com" "subject" (TL.pack "Hello! This is the mail body!") conn
        else print "Authentication failed."

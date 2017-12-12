module DPT.Notification.Types
    (
        EmailConf (..)
    ) where

import           Network.Socket.Internal (PortNumber)

data EmailConf = EmailConf {
    email      :: String   -- for use with gmail
 ,  password   :: String  -- for emailing using gmail
 ,  smtpServer :: String
 ,  smtpPort   :: PortNumber
} deriving (Show)

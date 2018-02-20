module Deploy.Notification.Types
    (
        EmailConf (..)
    ) where
import           Data.String             (String)
import           Network.Socket.Internal (PortNumber)
import           Protolude

data EmailConf = EmailConf {
    email      :: String   -- for use with gmail
 ,  password   :: String  -- for emailing using gmail
 ,  smtpServer :: String
 ,  smtpPort   :: PortNumber
} deriving (Show)

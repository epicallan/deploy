module Deploy.Notification.Conf (conf) where
import           DPT.Notification.Types  (EmailConf (..))
import           Network.Socket.Internal (PortNumber (PortNum))

conf :: EmailConf
conf = EmailConf {
        email = "myemail"
    ,   password = "mypassword"
    ,   smtpServer = "smtp.gmail.com"
    ,   smtpPort =  PortNum 587
}

module Deploy.Notification.Conf (conf) where

import           Deploy.Notification.Types (EmailConf (..))

conf :: EmailConf
conf = EmailConf {
        email = "myemail"
    ,   password = "mypassword"
    ,   smtpServer = "smtp.gmail.com"
    ,   smtpPort =  587
}

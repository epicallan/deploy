
module Deploy.Types
    (
        Repo (..)
    ,   Status (..)
    ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.String  (String)
import           GHC.Generics (Generic)
import           Protolude



-- | repo to deploy, this data can come from a data store or from a json file
data Repo = Repo {
        name :: Maybe String
    ,   path :: Maybe FilePath
} deriving (Show , Generic)

instance FromJSON Repo
instance ToJSON Repo

data Status = Status {
    status :: String
,   info   :: String
} deriving (Show, Generic)


instance FromJSON Status
instance ToJSON Status

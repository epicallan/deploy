
module Deploy.Types
    (
      Status (..)
    , RepoInit (..)
    , RepoEx (..)
    , DeployError (..)
    ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.String  (String)
import           Dhall        hiding (Text)
import           GHC.Generics (Generic)
import           Protolude



data Status
    = Status {
        status :: String
    ,   info   :: String
    } deriving (Show, Generic)


instance FromJSON Status
instance ToJSON Status

data RepoInit =
    RepoInit {
        riName     :: Maybe Text
    ,   riFiles    :: Maybe [Text]
    ,   riDeployIP :: Text
    } deriving (Generic, Show)

instance FromJSON RepoInit
instance ToJSON RepoInit
instance Interpret RepoInit

data RepoEx =
    RepoEx {
        rxName        :: Text
    ,   rxUploadPath  :: Text
    ,   rxArchivePath :: Text
    } deriving (Generic, Show)

instance FromJSON RepoEx
instance ToJSON RepoEx


data DeployError =
    MissingConfigError
  | NoRepoName
  | MissingDeloyIPError
  | RepoDetailsConfigError deriving (Show)

instance Exception  DeployError

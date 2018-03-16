
module Deploy.Types
    (
      Status (..)
    , Repo (..)
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


data Repo =
     Repo {
        repoName   :: Maybe Text
    ,   uploadPath :: Maybe Text
    ,   repoFiles  :: Maybe [Text]
    ,   deployIP   :: Maybe Text
    } deriving (Generic, Show)

instance FromJSON Repo
instance ToJSON Repo
instance Interpret Repo

data DeployError =
    MissingConfigError
  | NoRepoName
  | MissingDeloyIPError
  | MissingUploadPath
  | RepoDetailsConfigError deriving (Show)

instance Exception  DeployError

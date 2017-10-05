{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types
    ( GithubResponse (..)
    , TravisResponse (..)
    , EmailResponse (..)
    , EmailConf (..)
    , Repo (..)
    ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)
import           Prelude      hiding (id)


data Repository =
    Repository {
            name :: Text
        ,   url  :: Text
    } deriving (Show , Generic)

newtype Commit = Commit { id :: Text } deriving (Show , Generic)

data GithubResponse =
    GithubResponse {
            ref        :: Text
        ,   id         :: Int
        ,   repository :: Repository
        ,   commits    :: [Commit]
    } deriving (Show , Generic)

data TravisResponse =
    TravisResponse {
            id     :: Int
        ,   branch :: Text
        ,   state  :: Text
        ,   commit :: Text
    } deriving (Show , Generic)

-- | email notification following succesful deploy
data EmailResponse = EmailResponse {
        status :: Text
    ,   name   :: Text
    } deriving (Show)

-- | repo to deploy, this data can come from a data store or from a json file
data Repo = Repo {
        name       :: String
    ,   bashScript :: Maybe Text -- can be partially provided by user or autogenerated, there will be a default
    ,   emails     :: [String] -- emails to send notifications
} deriving (Show , Generic)

data EmailConf = EmailConf {
       email      :: String   -- for use with gmail
    ,  password   :: String  -- for emailing using gmail
    ,  smtpServer :: String
} deriving (Show)

instance FromJSON Repo
instance ToJSON Repo

instance FromJSON Repository
instance ToJSON Repository

instance FromJSON Commit
instance ToJSON Commit

instance FromJSON GithubResponse
instance ToJSON GithubResponse

instance FromJSON TravisResponse
instance ToJSON TravisResponse

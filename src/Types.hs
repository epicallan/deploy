{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types
    ( GithubResponse (..)
    , TravisResponse (..)
    , EmailResponse (..)
    , EmailConf (..)
    , Repository (..)
    , Repo (..)
    , Status (..)
    , Key
    , SSHKey (..)
    ) where

import           Data.Aeson              (FromJSON, ToJSON)
import           Data.Text               (Text)
import           GHC.Generics            (Generic)
import           Network.Socket.Internal (PortNumber)
import           Prelude                 hiding (id)


data Repository =
    Repository {
            name :: String
        ,   url  :: String
    } deriving (Show , Generic)

-- newtype Commit = Commit { id :: String } deriving (Show , Generic)

data GithubResponse =
    GithubResponse {
            ref        :: String
        ,   repository :: Repository
    } deriving (Show , Generic)

data TravisResponse =
    TravisResponse {
            id     :: Int
        ,   branch :: String
        ,   state  :: String
    } deriving (Show , Generic)

-- | email notification following succesful deploy
data EmailResponse = EmailResponse {
        status :: String
    ,   name   :: String
    } deriving (Show)

-- | repo to deploy, this data can come from a data store or from a json file
data Repo = Repo {
        name   :: String
    ,   host   :: String
    ,   port   :: Int
    ,   user   :: Int
    ,   sshKey :: String
} deriving (Show , Generic)

data EmailConf = EmailConf {
       email      :: String   -- for use with gmail
    ,  password   :: String  -- for emailing using gmail
    ,  smtpServer :: String
    ,  smtpPort   :: PortNumber
} deriving (Show)

data SSHKey = SSHKey {
        name :: String
    ,   key  :: String
} deriving (Show, Generic)

data Status = Status {
    status :: String
,   info   :: String
} deriving (Show, Generic)

instance FromJSON SSHKey

instance FromJSON Status
instance ToJSON Status

instance FromJSON Repo
instance ToJSON Repo

instance FromJSON Repository
instance ToJSON Repository

instance FromJSON GithubResponse
instance ToJSON GithubResponse

instance FromJSON TravisResponse
instance ToJSON TravisResponse

type Key = String

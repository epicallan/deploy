{-# LANGUAGE DeriveGeneric #-}

module Types
    ( GithubResponse (..)
    , TravisResponse (..)
    , EmailResponse (..)
    , AppConf (..)
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

data EmailResponse = EmailResponse {
        status :: Text
    ,   name   :: Text
    } deriving (Show)

data Repo = Repo {
        name               :: Text
    ,   branch             :: Text
    ,   fileToDeploy       :: Maybe String
    ,   gitTag             :: Maybe String
    ,   useTravisForStatus :: Boolean
} deriving (Show , Generic)

data AppConf = DeployConf {
       sshUser   :: Text  -- for ssh access
    ,  gEmail    :: Text    -- for use with gmail
    ,  gPassword :: Text  -- for emailing using gmail
    ,  emails    :: [Text] -- to send notifications
    ,  repos     :: [Repo] -- repos to deploy, read from a json file
} deriving (Show)

instance FromJSON Repository
instance ToJSON Repository

instance FromJSON Commit
instance ToJSON Commit

instance FromJSON GithubResponse
instance ToJSON GithubResponse

instance FromJSON TravisResponse
instance ToJSON TravisResponse

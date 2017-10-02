{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

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

-- | email notification following succesful deploy
data EmailResponse = EmailResponse {
        status :: Text
    ,   name   :: Text
    } deriving (Show)

-- | repo to deploy
data Repo = Repo {
        name               :: Text
    ,   branch             :: Text
    ,   fileToDeploy       :: Maybe String -- if not provided, container will be built from source
    ,   gitTag             :: Maybe String
    ,   useTravisForStatus :: Bool
} deriving (Show , Generic)

data AppConf = AppConf {
       sshUser   :: Text  -- for ssh access
    ,  gEmail    :: Text    -- for use with gmail
    ,  gPassword :: Text  -- for emailing using gmail
    ,  emails    :: [Text] -- emails to send notifications
    ,  repos     :: [Repo] -- repos to deploy, read from a json file
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

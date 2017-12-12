{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DPT.Deploy.Types
    (
    Repo (..)
    ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)
import           Prelude      hiding (id)


-- | repo to deploy, this data can come from a data store or from a json file
data Repo = Repo {
        name   :: String
    ,   commit :: String
    ,   port   :: Maybe Int
} deriving (Show , Generic)

instance FromJSON Repo
instance ToJSON Repo

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types
    (
     Status (..)
    ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

data Status = Status {
    status :: String
,   info   :: String
} deriving (Show, Generic)

instance FromJSON Status
instance ToJSON Status

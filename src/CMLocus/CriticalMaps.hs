{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module CMLocus.CriticalMaps
    ( Location(..)
    , Request(..)
    , Reply(..)
    , send
    )
  where

import GHC.Generics (Generic)

import Control.Lens ((^?))
import Data.Aeson (FromJSON, ToJSON(toJSON))
import Data.Aeson.Lens (_JSON)
import Data.Map (Map)
import Data.Text (Text)
import qualified Network.Wreq as Wreq (post, responseBody)

data Location = Location
    { latitude :: Int
    , longitude :: Int
    , timestamp :: Maybe Int
    }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data Request = Request
    { location :: Location
    , device :: Text
    }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data Reply = Reply
    { locations :: Map Text Location
    }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

send :: Request -> IO Reply
send loc = do
    res <- Wreq.post "https://api.criticalmaps.net/" (toJSON loc)
    let Just reply = res ^? Wreq.responseBody . _JSON
    pure reply

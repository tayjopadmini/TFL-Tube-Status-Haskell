{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Tfl_Tube_Data_Types where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import GHC.Generics

-- | This is the data type of Linestatuses which has one member known as line severity description
data LinesStatus =
  LinesStatus { statusSeverityDescription :: String
            } deriving (Generic,Show)

-- | This is the data type of Lines which has two members known as name and lineStatuses
data Lines =
 Lines { name :: String
       , lineStatuses :: [LinesStatus]

          } deriving (Generic,Show)
-- | This is the data type of Stoppoint which has two members known as naptanId and commonName
data Stoppoint =
 Stoppoint { naptanId :: String
           , commonName :: String

          } deriving (Generic,Show)
-- | This is the data type of Stoppoint which has 8 members known as lineId, lineName, platformName, modeName, destinationName, timeToStation, currentLocation and towards
data Arrivals =
 Arrivals { lineId :: String
           , lineName :: String
           , platformName :: String
           , modeName :: String
           , destinationName :: String
           , timeToStation :: Int
           , currentLocation :: String
           , towards :: String

          } deriving (Generic,Show)

instance FromJSON LinesStatus
instance ToJSON LinesStatus
instance FromJSON Lines
instance ToJSON Lines
instance FromJSON Stoppoint
instance ToJSON Stoppoint
instance FromJSON Arrivals
instance ToJSON Arrivals

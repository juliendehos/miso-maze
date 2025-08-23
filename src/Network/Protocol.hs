{-# LANGUAGE DeriveGeneric #-}

module Network.Protocol where

import Data.Aeson as Aeson
import GHC.Generics
import Miso

import Game.Common

data ServerMsg
  = Rejected MisoString
  | UpdateBoard MisoString
  | UpdateInfos Int [Player]    -- remaining meetings, players
  deriving (Eq, Generic, Show)

instance ToJSON ServerMsg where
    toEncoding = genericToEncoding Aeson.defaultOptions

instance FromJSON ServerMsg

data ClientMsg
  = Join MisoString
  | Play Move
  deriving (Eq, Generic, Show)

instance ToJSON ClientMsg where
    toEncoding = genericToEncoding Aeson.defaultOptions

instance FromJSON ClientMsg


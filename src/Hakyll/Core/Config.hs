module Hakyll.Core.Config where

import Data.Aeson.Types (FromJSON, Key, Object, parseMaybe, (.:))
import Data.Maybe (fromMaybe)
import Hakyll (Identifier, Metadata, Rules, getMetadata)

loadConfig :: Identifier -> Rules Metadata
loadConfig = getMetadata

configMaybe :: (FromJSON a) => a -> Key -> Object -> a
configMaybe d key val = fromMaybe d $ parseMaybe (.: key) val

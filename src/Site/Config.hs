{-# LANGUAGE OverloadedStrings #-}

module Site.Config where

import Data.Aeson (Key, Object)
import Data.Aeson.Types (FromJSON, Key, Object, parseMaybe, (.:))
import Data.Maybe (fromMaybe)
import Hakyll (Identifier, Metadata, Rules, getMetadata)

highlightStyle' = "highlightStyle" :: Key
compileWallpapers' = "compileWallpapers" :: Key
debugMode' = "debugMode" :: Key
modeColor' = "modeColor" :: Key

load :: Identifier -> Rules Metadata
load = getMetadata

configMaybe :: (FromJSON a) => a -> Key -> Object -> a
configMaybe d key val = fromMaybe d $ parseMaybe (.: key) val

highlightStyle :: Object -> String
highlightStyle = configMaybe "breezeDark" highlightStyle'

compileWallpapers :: Object -> Bool
compileWallpapers = configMaybe False compileWallpapers'

debugMode :: Object -> Bool
debugMode = configMaybe False debugMode'

modeColor :: Object -> String
modeColor config = do
    if debugMode config
        then "red"
        else "purple"

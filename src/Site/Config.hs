{-# LANGUAGE OverloadedStrings #-}

module Site.Config where

import Hakyll.Core.Config (configMaybe)
import Data.Aeson (Key, Object)

highlightStyle' = "highlightStyle" :: Key
compileWallpapers' = "compileWallpapers" :: Key
debugMode' = "debugMode" :: Key

highlightStyle :: Object -> String
highlightStyle = configMaybe "breezeDark" highlightStyle'

compileWallpapers :: Object -> Bool
compileWallpapers = configMaybe False compileWallpapers'

debugMode :: Object -> Bool
debugMode = configMaybe False debugMode'

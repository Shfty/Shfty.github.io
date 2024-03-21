{-# LANGUAGE OverloadedStrings #-}

module Site.Pattern where

import Hakyll (Pattern, complement, fromGlob, hasNoVersion, hasVersion, toFilePath, (.&&.), (.||.))
import Hakyll.Web.Menu (menuSection)
import qualified Site.Pattern.Directory as Directory
import qualified Site.Pattern.Extension as Extension
import System.FilePath (replaceFileName)

filePat file = fromGlob $ "**/" ++ file

config = "config" :: Pattern

wallpaperLandscape = Directory.images .&&. filePat "base.png" :: Pattern
wallpaperPortrait = Directory.images .&&. filePat "base-vertical.png" :: Pattern
wallpaper = wallpaperLandscape .||. wallpaperPortrait
notWallpaper = complement wallpaper

image = Directory.images .&&. Extension.image .&&. notWallpaper
font = Directory.fonts .&&. Extension.font

staticAsset = image .||. font

pageAsset = Directory.pages .&&. (Extension.image .||. Extension.video)

notMetadata = complement Extension.metadata

imageAsset = Extension.image
videoAsset = Extension.video

asset = imageAsset .||. videoAsset
notAsset = complement asset

index = "**/index.*" :: Pattern
notIndex = complement index

categories = Directory.pages .&&. index .&&. hasNoVersion
pages = Directory.pages .&&. notIndex .&&. notAsset .&&. notMetadata .&&. hasNoVersion

template = Directory.templates .&&. Extension.html

scss = Directory.css .&&. Extension.scss
mainScss = Directory.css .&&. filePat "main.scss"

-- Match all the immediate children of the provided ident
children ident = do
    let path = toFilePath ident
    let childPages = fromGlob (replaceFileName path "*.*") .&&. notIndex
    let childCats = fromGlob $ replaceFileName path "*/index.*"
    (childPages .||. childCats) .&&. hasVersion menuSection

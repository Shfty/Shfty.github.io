{-# LANGUAGE OverloadedStrings #-}

module Site.Pattern where

import Hakyll ((.||.), (.&&.), hasNoVersion, complement, Pattern)

--- Hakyll Patterns

config = "config" :: Pattern

images = "images/*" :: Pattern

wallpaperLandscape = "images/base.png" :: Pattern
wallpaperPortrait = "images/base-vertical.png" :: Pattern
wallpaper = wallpaperLandscape .||. wallpaperPortrait
notWallpaper = complement wallpaper

image = images .&&. notWallpaper

staticAsset =
    image
        .||. "images/**.png"
        .||. "images/**.jpg"
        .||. "images/**.jpeg"
        .||. "images/**.gif"
        .||. "images/**.mkv"
        .||. "fonts/**.woff"
        .||. "fonts/**.woff2"

pageAsset =
    "pages/**.png"
        .||. "pages/**.jpg"
        .||. "pages/**.jpeg"
        .||. "pages/**.gif"
        .||. "pages/**.mkv"

metadata = "**.metadata" :: Pattern
notMetadata = complement metadata

imageAsset = "**.png" .||. "**.jpeg" .||. "**.jpg" .||. "**.gif"
videoAsset = "**.mkv" .||. "**.mp4" .||. "**.webm"

asset = imageAsset .||. videoAsset
notAsset = complement asset

base = "pages/**" :: Pattern

index = "**/index.*" :: Pattern
notIndex = complement index

categories = base .&&. index .&&. hasNoVersion
pages = base .&&. notIndex .&&. notAsset .&&. notMetadata .&&. hasNoVersion

template = "templates/**.html" :: Pattern

scss = "css/**.scss" :: Pattern
mainScss = "css/main.scss" :: Pattern


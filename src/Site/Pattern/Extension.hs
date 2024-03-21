module Site.Pattern.Extension where

import Hakyll (fromGlob, (.||.))
import System.FilePath ((<.>))

extensionPat ext = fromGlob $ "**" <.> ext

png = extensionPat "png"
jpg = extensionPat "jpg" .||. extensionPat "jpeg"
gif = extensionPat "gif"

mkv = extensionPat "mkv"
mp4 = extensionPat "mp4"
webm = extensionPat "webm"

woff = extensionPat "woff"
woff2 = extensionPat "woff2"

html = extensionPat "html"
scss = extensionPat "scss"

metadata = extensionPat "metadata"

image = png .||. jpg .||. gif
video = mkv .||. mp4 .||. webm
font = woff .||. woff2


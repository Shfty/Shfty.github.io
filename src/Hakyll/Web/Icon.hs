module Hakyll.Web.Icon where

import Hakyll (Context)
import Hakyll.Core.Metadata.Maybe (maybeMetadataField)

maybeIconField :: String -> Context a
maybeIconField = maybeMetadataField "icon"

maybeIconColorField :: String -> Context a
maybeIconColorField = maybeMetadataField "icon-color"

iconCtx :: String -> String -> Context a
iconCtx icon color = maybeIconField icon <> maybeIconColorField color

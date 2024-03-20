{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Web.Glyphs where

import Data.Aeson (Value)
import Text.Blaze.Html (Html)
import Text.Blaze.Html5 (Attribute, AttributeValue, Markup, Tag, customAttribute, pre, stringTag, (!))
import Text.Blaze.Internal (customAttribute, customLeaf)

-- Custom HTML tags
glyphTag :: Tag
glyphTag = stringTag "x-glyph"

glyph :: Markup
glyph = customLeaf glyphTag True

glyphTy :: AttributeValue -> Attribute
glyphTy = customAttribute "type"

leftSoftDivider :: Markup
leftSoftDivider = glyph ! glyphTy "left-soft-divider"

preSpace :: Html
preSpace = pre " "

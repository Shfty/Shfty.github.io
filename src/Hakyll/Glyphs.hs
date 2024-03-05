{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Glyphs where

import Text.Blaze.Internal (customLeaf, customAttribute)

import qualified Text.Blaze.Html5 as H

-- Custom HTML tags
glyphTag = H.stringTag "x-glyph"
glyph = customLeaf glyphTag True
glyphTy = customAttribute "type"

leftSoftDivider = glyph H.! glyphTy "left-soft-divider"
preSpace = H.pre " "


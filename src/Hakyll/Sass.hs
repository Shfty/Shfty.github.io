{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Sass where

import Hakyll
import Hakyll.Web.Sass

import Text.Pandoc.Highlighting

-- SASS hot-reloading
sassHandling style template = do
    scssDependency <- makePatternDependency template
    rulesExtraDependencies [scssDependency] $ match template $ do
        route $ setExtension "css"
        compile (fmap compressCss <$> sassCompiler)

    create ["css/syntax.css"] $ do
        route idRoute
        compile $ makeItem $ styleToCss style

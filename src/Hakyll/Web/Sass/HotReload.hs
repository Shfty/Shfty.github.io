{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Web.Sass.HotReload where

import Hakyll (
    Pattern,
    compile,
    complement,
    compressCss,
    create,
    idRoute,
    makeItem,
    makePatternDependency,
    match,
    route,
    rulesExtraDependencies,
    setExtension,
    (.&&.),
 )
import Hakyll.Core.Identifier (Identifier)
import Hakyll.Core.Rules (Rules)
import Hakyll.Web.Sass (sassCompiler)
import Text.Pandoc.Highlighting (Style, styleToCss)

-- SASS hot-reloading
hotReloadSASS :: Pattern -> Pattern -> Rules ()
hotReloadSASS template deps = do
    scssDependency <- makePatternDependency (deps .&&. complement template)
    rulesExtraDependencies [scssDependency] $ match template $ do
        route $ setExtension "css"
        compile (fmap compressCss <$> sassCompiler)


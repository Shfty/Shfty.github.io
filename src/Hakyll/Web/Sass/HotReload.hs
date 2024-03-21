{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Web.Sass.HotReload where

import Control.Monad (liftM)
import Data.Functor ((<&>))
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
    dependency <- makePatternDependency (deps .&&. complement template)
    rulesExtraDependencies [dependency] $ match template $ do
        route $ setExtension "css"
        compile (fmap compressCss <$> sassCompiler)

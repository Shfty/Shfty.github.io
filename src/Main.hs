{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Function
import Hakyll
import Hakyll.Core
import Hakyll.Web
import Site
import qualified Site.Compiler as Compiler
import qualified Site.Config as Config
import qualified Site.Context as Context
import qualified Site.Identifier as Identifier
import qualified Site.Pattern as Pattern
import qualified Site.Pattern.Directory as Directory
import qualified Site.Pattern.Extension as Extension
import qualified Site.Routes as Routes
import qualified Site.Rules as Rules
import qualified Site.Template as Template

main :: IO ()
main = site $ do
    -- Load git branch into a context
    branchContext <- Context.branchField "branch"

    -- Load site config
    config <- Config.load $ fromFilePath "config"

    -- Get debug mode flag
    let debugModeContext =
            boolField "debugMode" (const $ Config.debugMode config)
                <> constField "modeColor" (Config.modeColor config)

    -- Assemble global site context
    let siteContext = branchContext <> debugModeContext <> Context.children <> Context.site

    -- Load code highlighting style
    let highlightStyle = Config.highlightStyle config
    pandocStyle <- loadPandocStyle highlightStyle

    -- Generate a CSS file for the loaded pandoc style
    Rules.styleCSS pandocStyle

    -- Setup SASS handling
    Rules.sass

    -- Compile templates
    Rules.template

    -- Compile static assets
    Rules.staticAsset

    -- Compile wallpapers
    when (Config.compileWallpapers config) $
        wallpapers Pattern.wallpaperLandscape Pattern.wallpaperPortrait

    -- Compile footer
    Rules.footer

    --- Compiler utilities
    let pandocCompiler' = pandocCompilerWithStyle pandocStyle

    let siteDefaults = defaults pandocCompiler' siteContext
    let pageDefaults = siteDefaults Template.page
    let catDefaults = siteDefaults Template.category

    let providers =
            defaultProviders
                & withCompiler "None" getResourceBody
                & withCompiler "Default" pandocCompiler'
                & withCompiler "pandoc" pandocCompiler'
                & withContext "None" siteContext

    let siteSpec = spec providers
    let pageSpec = siteSpec pageDefaults
    let catSpec = siteSpec catDefaults

    let [imageCtx, videoCtx, pageCtx, categoryCtx] =
            (<> siteContext) . uncurry iconCtx
                <$> [ ("image", "white")
                    , ("video", "white")
                    , ("file", "white")
                    , ("custom-folder", "purple")
                    ]

    -- Compile image and video viewers
    Rules.pageImage $ Rules.viewer Template.image imageCtx siteContext
    Rules.pageVideo $ Rules.viewer Template.video videoCtx siteContext

    -- Compile pages
    Rules.page $ do
        Rules.layoutSingle pageCtx siteContext
        Rules.final pageSpec siteContext

    -- Compile categories
    Rules.category $ do
        Rules.layoutList categoryCtx siteContext
        Rules.final catSpec siteContext

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Char
import Data.Function
import Data.List.Utils
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
import qualified Site.Tags as Tags
import System.FilePath

main :: IO ()
main = site $ do
    -- Load git branch into a context
    branchContext <- Context.branchField "branch"

    -- Load site config
    config <- Config.load $ fromFilePath "config"

    -- Get debug mode flag
    branch <- Context.gitBranch
    let debugMode = branch /= "master"

    let siteModeContext =
            constField "modeColor" (if debugMode then "red" else "purple")

    -- Assemble global site context
    let siteContext = branchContext <> siteModeContext <> Context.children <> Context.site

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

    let getSpec = Compiler.ifCategory catSpec pageSpec
    let getCtx = Compiler.ifCategory categoryCtx pageCtx

    -- Compile pages
    Rules.page $ do
        Rules.slug getCtx
        Rules.header getCtx
        Rules.layoutSingle siteContext
        Rules.final getSpec siteContext

    -- Compile categories
    Rules.category $ do
        Rules.slug getCtx
        Rules.header getCtx
        Rules.layoutList siteContext
        Rules.final getSpec siteContext

    let makeId =
            fromFilePath
                . (<.> "md")
                . ("tags" </>)
                . replace "+" "-plus"
                . replace "#" "-sharp"
                . replace " " "-"
                . fmap toLower

    -- Build tags
    tags <- buildTagsWith Tags.getTags Pattern.pages makeId
    Rules.tagIndex tags siteContext
    Rules.tagPages makeId tags siteContext

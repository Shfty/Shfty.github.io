{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Function
import Data.Functor
import Hakyll
import Hakyll.Core
import Hakyll.Web
import qualified Site.Config as Config
import qualified Site.Context as Context
import qualified Site.Identifier as Identifier
import Site.Layout
import qualified Site.Pattern as Pattern
import qualified Site.Pattern.Directory as Directory
import qualified Site.Pattern.Extension as Extension
import qualified Site.Routes as Routes
import qualified Site.Template as Template
import System.Process (readProcess)

makeSlug' = makeSlug Template.slug
makeHeader' = makeHeader Template.header

-- Given final page content, lift it into the main layout, and relativize its URLs
mainCompiler ctx =
    applyLayoutTemplate ctx
        >=> relativizeUrls

providerDir dir config =
    config
        { providerDirectory = dir
        }

hakyllConfig = providerDir "./site" $ extensionlessUrls defaultConfiguration

main :: IO ()
main = hakyllWith hakyllConfig $ do
    -- Load git branch into a context
    branchCtx' <- Context.branchField "branch"

    -- Load site config
    config <- loadConfig $ fromFilePath "config"

    -- Get debug mode flag
    let debugModeCtx = boolField "debugMode" (const $ Config.debugMode config)

    -- Assemble global site context
    let siteCtx = branchCtx' <> debugModeCtx <> Context.site

    -- Load code highlighting style
    let highlightStyle = Config.highlightStyle config
    pandocStyle <- loadPandocStyle highlightStyle

    -- Create custom pandoc compiler and accompanying CSS styles
    let pandocCompiler' = pandocCompilerWithStyle pandocStyle
    makeStyleCSS Identifier.syntaxCss pandocStyle

    -- Setup SASS handling
    hotReloadSASS Pattern.mainScss Pattern.scss

    -- Compile templates
    Template.makeTemplates Pattern.template

    -- Compile static assets
    match Pattern.staticAsset $ do
        route idRoute
        compile copyFileCompiler

    -- Compile wallpapers
    when (Config.compileWallpapers config) $
        wallpapers Pattern.wallpaperLandscape Pattern.wallpaperPortrait

    -- Compile footer
    match (fromList [Identifier.footer]) $
        compile getResourceBody

    --- Baseline compilation
    let providers =
            defaultProviders
                & withCompiler "None" getResourceBody
                & withCompiler "Default" pandocCompiler'
                & withCompiler "pandoc" pandocCompiler'
                & withContext "None" siteCtx

    let spec a b c = CompilerSpec providers $ CompilerDefaults a b c

    -- Compile images
    match (Directory.pages .&&. Extension.image .&&. hasNoVersion) $ do
        let imageCtx = iconCtx "image" "white" <> siteCtx
        makeSlug' imageCtx
        makeMenuSection siteCtx
        makeHeader' (breadcrumbCtx imageCtx <> imageCtx)

        makeViewerWith parentRoute Routes.base $
            compileViewer Template.image siteCtx
                >>= mainCompiler siteCtx

    -- Compile videos
    match (Directory.pages .&&. Extension.video .&&. hasNoVersion) $ do
        let videoCtx = iconCtx "video" "white" <> siteCtx
        makeSlug' videoCtx
        makeMenuSection siteCtx
        makeHeader' (breadcrumbCtx videoCtx <> videoCtx)

        makeViewerWith parentRoute Routes.base $
            compileViewer Template.video siteCtx
                >>= mainCompiler siteCtx

    let makeSpec = spec pandocCompiler'

    -- Compile pages
    let pageSpec = makeSpec siteCtx Template.page

    match Pattern.pages $ do
        let pageCtx = iconCtx "file" "white" <> siteCtx
        makeSlug' pageCtx
        makeMenuSection siteCtx
        makeHeader' (breadcrumbCtx pageCtx <> pageCtx)

        route Routes.base
        compile $
            overridableCompiler pageSpec
                >>= mainCompiler siteCtx

    -- Compile categories
    let loadChildren ident = loadAll $ Pattern.children ident

    let children ident = loadChildren ident >>= sortPosts :: Compiler [Item String]

    let childrenCtx ident = listField "children" Context.post (children ident) <> siteCtx

    let categoryMenuSection item = do
            ident <- getUnderlying
            let ctx = childrenCtx ident <> siteCtx
            loadAndApplyTemplate Template.menuSection ctx item

    let catSpec ident =
            makeSpec
                (childrenCtx ident)
                Template.category

    match Pattern.categories $ do
        let categoryCtx = iconCtx "custom-folder" "purple" <> siteCtx
        makeSlug' categoryCtx
        makeMenuSectionWith $ compileMenuSection siteCtx >>= categoryMenuSection
        makeHeader' (breadcrumbCtx categoryCtx <> categoryCtx)

        route Routes.base
        compile $ do
            ident <- getUnderlying
            overridableCompiler (catSpec ident)
                >>= mainCompiler siteCtx

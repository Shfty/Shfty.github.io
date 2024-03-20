{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import Hakyll
import Hakyll.Core
import Hakyll.Web
import qualified Site.Identifier as Identifier
import Site.Layout
import qualified Site.Context as Context
import qualified Site.Pattern as Pattern
import qualified Site.Routes as Routes
import qualified Site.Template as Template
import System.FilePath

main :: IO ()
main = hakyllWith (extensionlessUrls defaultConfiguration) $ do
    -- Load site config
    config <- loadConfig $ fromFilePath "config"

    -- Load code highlighting style
    let highlightPath = configMaybe "breezeDark" "highlightStyle" config
    pandocStyle <- loadPandocStyle highlightPath

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
    if configMaybe False "compileWallpapers" config
        then wallpapers Pattern.wallpaperLandscape Pattern.wallpaperPortrait
        else return ()

    -- Compile footer
    match (fromList [Identifier.footer]) $ compile getResourceBody

    --- Baseline compilation
    let providers =
            defaultProviders
                & withCompiler "None" getResourceBody
                & withCompiler "Default" pandocCompiler'
                & withCompiler "pandoc" pandocCompiler'
                & withContext "None" Context.site

    let spec a b c = CompilerSpec providers $ CompilerDefaults a b c

    let breadcrumbCtx' = breadcrumbCtx versionSlug

    let makeSlug' = makeSlug Template.slug
    let makeHeader' = makeHeader Template.header

    -- Compile images
    match (Pattern.base .&&. Pattern.imageAsset .&&. hasNoVersion) $ do
        let imageCtx = iconCtx "image" "white" <> Context.site
        makeSlug' imageCtx
        makeMenuSection Context.site
        makeHeader' (breadcrumbCtx' imageCtx <> imageCtx)

        makeViewerWith parentRoute Routes.base $
            compileViewer Template.image Context.site
                >>= applyLayoutTemplate Context.site
                >>= relativizeUrls

    -- Compile videos
    match (Pattern.base .&&. Pattern.videoAsset .&&. hasNoVersion) $ do
        let videoCtx = iconCtx "video" "white" <> Context.site
        makeSlug' videoCtx
        makeMenuSection Context.site
        makeHeader' (breadcrumbCtx' videoCtx <> videoCtx)

        makeViewerWith parentRoute Routes.base $
            compileViewer Template.video Context.site
                >>= applyLayoutTemplate Context.site
                >>= relativizeUrls

    -- Compile pages
    let pageSpec = spec pandocCompiler' Context.site Template.page

    match Pattern.pages $ do
        let pageCtx = iconCtx "file" "white" <> Context.site
        makeSlug' pageCtx
        makeMenuSection Context.site
        makeHeader' (breadcrumbCtx' pageCtx <> pageCtx)

        route Routes.base
        compile $
            overridableCompiler pageSpec
                >>= applyLayoutTemplate Context.site
                >>= relativizeUrls

    -- Compile categories
    let children ident = do
            let path = toFilePath ident
            let childPages = fromGlob (replaceFileName path "*.*") .&&. Pattern.notIndex
            let childCats = fromGlob $ replaceFileName path "*/index.*"
            let pat = (childPages .||. childCats) .&&. hasVersion versionMenuSection
            ids <- getMatches pat :: Compiler [Identifier]
            this <- loadSlug ident :: Compiler String
            loadAll (fromList ids) >>= sortPosts :: Compiler [Item String]

    let categoryMenuSection item = do
            ident <- getUnderlying
            let ctx = listField "children" Context.post (children ident) <> Context.site
            loadAndApplyTemplate "templates/menu-section.html" ctx item

    let catSpec ident =
            spec
                pandocCompiler'
                ( listField "posts" Context.post (children ident)
                    <> Context.site
                )
                Template.category

    match Pattern.categories $ do
        let categoryCtx = iconCtx "custom-folder" "purple" <> Context.site
        makeSlug' categoryCtx
        makeMenuSectionWith $ menuSection Context.site >>= categoryMenuSection
        makeHeader' (breadcrumbCtx' categoryCtx <> categoryCtx)

        route Routes.base
        compile $ do
            ident <- getUnderlying
            overridableCompiler (catSpec ident)
                >>= applyLayoutTemplate Context.site
                >>= relativizeUrls

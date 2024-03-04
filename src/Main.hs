{-# LANGUAGE OverloadedStrings #-}

import Data.FileTree

import qualified Data.Tree as T

import Hakyll
import Hakyll.Breadcrumb
import Hakyll.Compiler.Overridable
import Hakyll.Highlighting
import Hakyll.Layout
import Hakyll.Sass
import Hakyll.Wallpaper

import Control.Monad
import Data.Function
import Data.Text (splitOn, strip)
import Data.Tuple
import System.FilePath
import Text.Blaze.Html.Renderer.String
import Text.Pandoc.Highlighting

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as ByteString
import Hakyll.Commands (watch)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

--------------------------------------------------------------------------------
-- Constants

imagesPattern = "images/*"

landscapeWallpaperPattern = "images/base.png" :: Pattern
portraitWallpaperPattern = "images/base-vertical.png" :: Pattern
wallpaperPattern = landscapeWallpaperPattern .||. portraitWallpaperPattern
notWallpaperPattern = complement wallpaperPattern

imagePattern = imagesPattern .&&. notWallpaperPattern

staticAssetPattern =
    imagePattern
        .||. "posts/**.png"
        .||. "posts/**.jpeg"
        .||. "posts/**.gif"
        .||. "posts/**.mkv"
        .||. "fonts/**.woff"
        .||. "fonts/**.woff2"

metadataPattern = "**/*.metadata"
imageAssetPattern = "**/*.png" .||. "**/*.jpg" .||. "**/*.gif"
videoAssetPattern = "**/*.mkv" .||. "**/*.mp4"

assetPattern = metadataPattern .||. imageAssetPattern .||. videoAssetPattern
notAssetPattern = complement assetPattern

recursivePattern = "pages/**" .&&. notAssetPattern
indexPattern = "**/index.*"
notIndexPattern = complement indexPattern
recursiveCategoriesPattern = recursivePattern .&&. indexPattern
recursivePagesPattern = recursivePattern .&&. notIndexPattern

htmlTemplate = "templates/html.html"
footerTemplate = "templates/footer.html"
headerTemplate = "templates/content/header.html"
flexColumnTemplate = "templates/flex-column.html"
flexRowTemplate = "templates/flex-row.html"
flexScrollTemplate = "templates/flex-scroll.html"

postTemplate = "templates/post.html"
categoryTemplate = "templates/category.html"

cssTemplate = "css/*.scss"

highlightStyleDefault = breezeDark
highlightStyleJson = "style.json"

--------------------------------------------------------------------------------
-- Business logic

pop [] = []
pop xs = init xs

last' :: [a] -> a
last' [a] = a
last' (a : as) = last' as

applyFlexColumn = applyTernaryTemplate flexColumnTemplate
applyFlexRow = applyTernaryTemplate flexRowTemplate

applyLayoutTemplate :: FileTree (Identifier, Metadata) -> Context String -> Item String -> Compiler (Item String)
applyLayoutTemplate metaTree ctx = do
    let header = loadAndApplyTemplate headerTemplate ctx >=> return . itemBody
    let footer = loadAndApplyTemplate footerTemplate ctx >=> return . itemBody

    let mt =
            menuTree metaTree
                >>= makeItem . renderHtml . foldDetails

    let panel header footer ctx =
            loadAndApplyTemplate flexScrollTemplate ctx
                >=> applyFlexColumn header footer ctx

    let sidebar =
            const $
                mt
                    >>= (panel (Just header) Nothing ctx >=> return . itemBody)
    let out =
            panel (Just header) Nothing ctx
                >=> applyFlexRow (Just sidebar) Nothing ctx
                >=> applyFlexColumn Nothing (Just footer) ctx
                >=> loadAndApplyTemplate htmlTemplate ctx

    out

-- Static assets (images, fonts, etc.)
staticAssets = match staticAssetPattern $ do
    route idRoute
    compile copyFileCompiler

-- Post Context
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext

-- Templates
templates = match "templates/**.html" $ compile templateBodyCompiler

-- Hoist the provided route one directory upward
parentRoute =
    customRoute $
        replaceDirectory
            <*> joinPath . drop 1 . splitPath . takeDirectory
            <$> toFilePath

recursiveRoute = composeRoutes (setExtension "html") parentRoute

makeIdentTree fileTree = do
    let isIndex = (== "index") . takeBaseName

    let indexBranches a = case a of
            Branch a as -> Branch (a </> "index.md") (indexBranches <$> as)
            Leaf a -> Leaf a

    let fileTree' = uncurry (</>) . swap <$> tag fileTree
    let fileTree'' = filterFileTree (not . isIndex) fileTree'
    fromFilePath <$> indexBranches fileTree''

makeMetaTree =
    mapM
        ( \a -> do
            meta <- getMetadata a
            return (a, meta)
        )

foldDetails (Branch a as) = H.details H.! A.open "" $ H.toHtml a <> mconcat (foldDetails <$> as)
foldDetails (Leaf a) = H.toHtml a

main :: IO ()
main = do
    -- Hakyll composition
    hakyll $ do
        pandocStyle <- tryLoadStyle highlightStyleDefault highlightStyleJson
        let pandocCompiler' = pandocCompilerWithStyle pandocStyle

        wallpapers landscapeWallpaperPattern portraitWallpaperPattern

        staticAssets

        sassHandling pandocStyle cssTemplate

        -- Create tree structure
        fileTree <- makeFileTree "pages/**"
        -- error $ show fileTree
        let identTree = makeIdentTree fileTree
        -- error $ show identTree
        metaTree <- makeMetaTree identTree
        -- error $ show metaTree

        -- Build page breadcrumb data, compiler
        breadcrumbs <- buildTagsWith getBreadcrumb recursivePagesPattern $ fromCapture "*/index.md"
        -- error $ showTags breadcrumbs

        let pageCtx = breadcrumbField "breadcrumb" breadcrumbs `mappend` defaultContext
        let categoryCtx postPat =
                listField "posts" postCtx (loadAll postPat)
                    `mappend` breadcrumbFieldWith (getBreadcrumbWith pop) "breadcrumb" breadcrumbs
                    `mappend` defaultContext

        let pages a = case a of
                Branch a as -> concatMap pages as
                Leaf a -> [a | matches ("**/*.md" .||. "**/*.html") a]

        let children a = case a of
                Branch a as -> [a]
                Leaf a -> [a]

        let categories a = case a of
                Branch a as -> [(a, concatMap children as)] <> concatMap categories as
                Leaf a -> []

        let pageDefaults = CompilerDefaults pandocCompiler' pageCtx postTemplate
        let pageProvider =
                providers pageDefaults
                    & withCompiler "None" getResourceBody
                    & withCompiler "Default" pandocCompiler'
                    & withCompiler "pandoc" pandocCompiler'
                    & withContext "None" defaultContext

        let recursivePageCompiler ident = do
                overridableCompiler pageProvider ident
                    >>= applyLayoutTemplate metaTree pageCtx
                    >>= relativizeUrls

        mapM_
            ( \a -> do
                match (fromList [a]) $ do
                    route recursiveRoute
                    compile $ recursivePageCompiler a
            )
            (pages identTree)

        let recursiveCategoryCompiler postPat ident = do
                let catDefaults = CompilerDefaults pandocCompiler' (categoryCtx postPat) categoryTemplate
                let catProvider = pageProvider & mapDefaults (const catDefaults)

                overridableCompiler catProvider ident
                    >>= applyLayoutTemplate metaTree pageCtx
                    >>= relativizeUrls

        mapM_
            ( \(a, b) -> do
                match (fromList [a]) $ do
                    route recursiveRoute
                    compile $ recursiveCategoryCompiler (fromList b) a
            )
            (categories identTree)

        templates

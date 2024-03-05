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
import qualified Data.Map
import Data.Maybe
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
        .||. "images/**.png"
        .||. "images/**.jpeg"
        .||. "images/**.gif"
        .||. "images/**.mkv"
        .||. "fonts/**.woff"
        .||. "fonts/**.woff2"

pageAssetPattern =
    "pages/**/*.png"
        .||. "pages/**/*.jpg"
        .||. "pages/**/*.gif"
        .||. "pages/**/*.mkv"

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
slugTemplate = "templates/slug.html"
flexColumnTemplate = "templates/flex-column.html"
flexRowTemplate = "templates/flex-row.html"
flexScrollTemplate = "templates/flex-scroll.html"

postTemplate = "templates/post.html"
categoryTemplate = "templates/category.html"

cssTemplate = "css/*.scss"

highlightStyleDefault = breezeDark
highlightStyleJson = "style.json"

versionHeader = "header"
versionSlug = "slug"
versionMenu = "menu"
versionBody = "body"

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
applyLayoutTemplate metaTree ctx item = do
    let ident = itemIdentifier item

    let identHeader = setVersion (Just versionHeader) ident
    header <- loadBody identHeader :: Compiler String

    let identBody = setVersion (Just versionBody) ident
    body <- load identBody :: Compiler (Item String)

    let identMenu = setVersion (Just versionMenu) ident
    menu <- load identMenu :: Compiler (Item String)

    footer <- loadBody "footer.html"

    let panel header footer ctx =
            loadAndApplyTemplate flexScrollTemplate ctx
                >=> applyFlexColumn header footer ctx

    body' <- panel (Just header) Nothing ctx body
    menu' <- panel (Just header) Nothing ctx menu

    makeItem (itemBody body')
        >>= applyFlexRow (Just (itemBody menu')) Nothing ctx
        >>= applyFlexColumn Nothing (Just footer) ctx
        >>= loadAndApplyTemplate htmlTemplate ctx

-- Static assets (images, fonts, etc.)
staticAssets = match staticAssetPattern $ do
    route idRoute
    compile copyFileCompiler

-- Colocated page assets
pageAssets = match pageAssetPattern $ do
    route parentRoute
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

        -- wallpapers landscapeWallpaperPattern portraitWallpaperPattern

        staticAssets
        pageAssets

        sassHandling pandocStyle cssTemplate

        -- Create tree structure
        fileTree <- makeFileTree ("pages/**/*.md" .||. "pages/**/*.html")
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

        let routes a = case a of
                Branch a as -> [a] <> concatMap routes as
                Leaf a -> [a | matches ("**/*.md" .||. "**/*.html") a]

        let pages a = case a of
                Branch a as -> concatMap pages as
                Leaf a -> [a | matches ("**/*.md" .||. "**/*.html") a]

        let categories a = do
                let children a = case a of
                        Branch a as -> [a]
                        Leaf a -> [a]

                case a of
                    Branch a as -> [(a, concatMap children as)] <> concatMap categories as
                    Leaf a -> []

        -- Compile footer
        match "footer.html" $ compile getResourceBody

        -- Compile headers, menus
        match (fromList $ routes identTree) $ do
            version versionSlug $ do
                compile $ do
                    makeItem "" >>= loadAndApplyTemplate slugTemplate pageCtx

            version versionMenu $ do
                compile $ do
                    menuTree metaTree
                        >>= makeItem . renderHtml . H.nav . foldDetails

            version versionHeader $ do
                compile $ do
                    makeItem "" >>= loadAndApplyTemplate headerTemplate pageCtx

            route recursiveRoute
            compile $
                makeItem ""
                    >>= applyLayoutTemplate metaTree pageCtx
                    >>= relativizeUrls

        --- Specialized compilers
        let providers = defaultProviders
                    & withCompiler "None" getResourceBody
                    & withCompiler "Default" pandocCompiler'
                    & withCompiler "pandoc" pandocCompiler'
                    & withContext "None" defaultContext

        let spec a b c = CompilerSpec providers $ CompilerDefaults a b c

        let categories' = Data.Map.fromList $ categories identTree

        -- Compile pages
        let pageSpec = spec pandocCompiler' pageCtx postTemplate
        
        match (fromList $ pages identTree) $ do
            version versionBody $ do
                compile $
                    overridableCompiler pageSpec

        -- Compile categories
        let bodyCompiler = do
                ident <- getUnderlying
                let ident' = setVersion Nothing ident
                let pat = fromList $ fromMaybe mempty $ Data.Map.lookup ident' categories'

                let catDefaults = CompilerDefaults 
                let catSpec = spec pandocCompiler' (categoryCtx pat) categoryTemplate

                overridableCompiler catSpec

        match (fromList $ Data.Map.keys categories') $ do
            version versionBody $ do
                compile bodyCompiler

        templates

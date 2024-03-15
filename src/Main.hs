{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.FileTree

import qualified Data.Tree as T

import Hakyll
import Hakyll.Compiler.Overridable
import Hakyll.Layout
import Hakyll.Sass
import Hakyll.Wallpaper

import Control.Monad
import Data.Function
import Data.Text (splitOn, strip)
import Data.Tuple
import System.Directory (doesFileExist)
import System.FilePath
import Text.Blaze.Html.Renderer.String
import Text.Pandoc.Highlighting

import Network.Mime (defaultMimeLookup)
import Network.Wai.Application.Static (
    StaticSettings (ssGetMimeType, ssLookupFile),
    defaultFileServerSettings,
 )
import WaiAppStatic.Types (fileName, fromPiece, unsafeToPiece)

import Data.Aeson ((.:))
import qualified Data.Aeson as JSON
import Data.Aeson.Types (parseMaybe)
import Data.Bifunctor
import qualified Data.ByteString.Lazy as ByteString
import Data.List (intersperse, isInfixOf, isPrefixOf)
import Data.Map (toList)
import qualified Data.Map
import Data.Maybe
import qualified Data.Text as Text
import Data.Text.Conversions (fromText)
import Hakyll.Commands (watch)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Pandoc (runIOorExplode)
import Text.Pandoc.MIME (MimeType)
import Text.Pandoc.Options (WriterOptions (writerHighlightStyle))

--------------------------------------------------------------------------------
-- Constants

configPattern = "config"

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
    "pages/**.png"
        .||. "pages/**.jpg"
        .||. "pages/**.gif"
        .||. "pages/**.mkv"

metadataPattern = "**.metadata"
imageAssetPattern = "**.png" .||. "**.jpg" .||. "**.gif"
videoAssetPattern = "**.mkv" .||. "**.mp4"

assetPattern = metadataPattern .||. imageAssetPattern .||. videoAssetPattern
notAssetPattern = complement assetPattern

recursivePattern = "pages/**" .&&. notAssetPattern
indexPattern = "**index.*"
notIndexPattern = complement indexPattern
recursiveCategoriesPattern = recursivePattern .&&. indexPattern
recursivePagesPattern = recursivePattern .&&. notIndexPattern

templatePattern = "templates/**.html"

documentTemplate = "templates/document.html"
footerTemplate = "templates/footer.html"
headerTemplate = "templates/content/header.html"
slugTemplate = "templates/slug.html"
flexColumnTemplate = "templates/flex-column.html"
flexRowTemplate = "templates/flex-row.html"
flexScrollTemplate = "templates/flex-scroll.html"

postTemplate = "templates/post.html"
categoryTemplate = "templates/category.html"

cssTemplate = "css/main.scss"

highlightStyleDefault = "breezeDark"

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

    let [identHeader, identBody, identMenu] =
            flip setVersion ident . Just
                <$> [ versionHeader
                    , versionBody
                    , versionMenu
                    ]

    bodyHeader <- loadBody identHeader :: Compiler String
    [body, menu] <- mapM load [identBody, identMenu] :: Compiler [Item String]

    menuHeader <- makeEmptyItem >>= loadAndApplyTemplate "templates/sidebar/header.html" ctx >>= return . itemBody :: Compiler String
    footer <- loadBody "footer.html"

    let panel header footer scrollCtx columnCtx =
            loadAndApplyTemplate flexScrollTemplate scrollCtx
                >=> applyFlexColumn header footer columnCtx

    body' <- panel (Just bodyHeader) Nothing ctx (constField "class" "body" <> ctx) body
    menu' <- panel (Just menuHeader) Nothing ctx (constField "class" "menu divider-right" <> ctx) menu

    makeItem (itemBody body')
        >>= applyFlexRow (Just (itemBody menu')) Nothing ctx
        >>= applyFlexColumn Nothing (Just footer) ctx
        >>= loadAndApplyTemplate documentTemplate ctx

-- Static assets (images, fonts, etc.)
staticAssets = match staticAssetPattern $ do
    route idRoute
    compile copyFileCompiler

-- Colocated page assets
pageAssets = match pageAssetPattern $ do
    route parentRoute
    compile copyFileCompiler

-- Base Context
siteCtx = prettyUrlField "url" <> defaultContext

-- Post Context
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` siteCtx

-- Templates
templates = match templatePattern $ compile templateBodyCompiler

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

makeEmptyItem = makeItem ""

slugCompiler ctx =
    makeEmptyItem
        >>= loadAndApplyTemplate
            slugTemplate
            ctx

makeBreadcrumbs :: [Identifier] -> FileTree Identifier -> [(Identifier, [Identifier])]
makeBreadcrumbs acc (Branch a as) = [(a, acc)] <> mconcat (makeBreadcrumbs (acc <> [a]) <$> as)
makeBreadcrumbs acc (Leaf a) = [(a, acc)]

-- Create a pandoc compiler with the provided highlight style
pandocCompilerWithStyle style =
    pandocCompilerWith
        defaultHakyllReaderOptions
        defaultHakyllWriterOptions
            { writerHighlightStyle = Just style
            }

-- Custom Hakyll configuration
-- Emulates the server config of GitHub Pages
makeConfig config =
    config
        { previewSettings = \path ->
            let settings = config.previewSettings path
             in settings
                    { ssLookupFile = \pieces ->
                        case splitAt (length pieces - 1) pieces of
                            (prefix, [piece]) -> do
                                let fileName = fromPiece piece
                                if takeExtension (fromText fileName) == ""
                                    then settings.ssLookupFile $ prefix <> [unsafeToPiece $ fileName <> ".html"]
                                    else settings.ssLookupFile pieces
                            _otherwise -> settings.ssLookupFile pieces
                    , ssGetMimeType = \file ->
                        if takeExtension (Text.unpack (fromPiece file.fileName)) == ""
                            then do
                                htmlExists <- doesFileExist $ path </> Text.unpack (fromPiece file.fileName) <.> "html"
                                if htmlExists
                                    then pure "text/html"
                                    else settings.ssGetMimeType file
                            else settings.ssGetMimeType file
                    }
        }

prettyUrl :: String -> String
prettyUrl url = do
    let isLocal = "/" `isPrefixOf` url || not ("://" `isInfixOf` url)
    if isLocal
        then
            if takeFileName url == "index.html"
                then dropFileName url
                else
                    if takeExtension url == ".html"
                        then dropExtension url
                        else url
        else url

prettyUrlField :: String -> Context a
prettyUrlField key =
    field key $ \item ->
        maybe
            (fail $ "no route url found for item " ++ show item.itemIdentifier)
            (toUrl . prettyUrl)
            <$> getRoute (setVersion Nothing item.itemIdentifier)

prettifyUrls = return . fmap (withUrls prettyUrl)

main :: IO ()
main = do
    -- Hakyll composition
    hakyllWith (makeConfig defaultConfiguration) $ do
        config <- getMetadata $ fromFilePath "config" :: Rules Metadata

        let highlightPath = parseMaybe (.: "highlightStyle") config :: Maybe String

        pandocStyle <-
            preprocess $
                runIOorExplode $
                    lookupHighlightingStyle (fromMaybe highlightStyleDefault highlightPath)

        let pandocCompiler' = pandocCompilerWithStyle pandocStyle

        sassHandling pandocStyle cssTemplate "css/**.scss"

        templates
        staticAssets
        pageAssets

        -- wallpapers landscapeWallpaperPattern portraitWallpaperPattern

        -- Create tree structure
        fileTree <- makeFileTree ("pages/**.md" .||. "pages/**.html")
        -- error $ show fileTree

        let identTree_ = makeIdentTree fileTree

        -- TODO: Lift into Rules / Compiler monad and sort by metadata
        --       Hakyll likely has some machinery to help with this
        --       (ex. order-by-date functionality)
        let pred a b = case (a, b) of
                (Branch a as, Leaf b) -> LT
                (Leaf a, Branch b bs) -> GT
                (Branch a as, Branch b bs) -> compare a b
                (Leaf a, Leaf b) -> compare a b

        let identTree = sortFileTreeBy pred identTree_

        -- error $ show identTree

        metaTree <- makeMetaTree identTree
        -- error $ show metaTree

        let breadcrumbs_ = makeBreadcrumbs [] identTree
        -- error $ mconcat $ intersperse "\n" $ show <$> breadcrumbs_

        let breadcrumbs = Data.Map.fromList breadcrumbs_

        -- Compilation
        let routes a = case a of
                Branch a as -> [a] <> concatMap routes as
                Leaf a -> [a | matches ("**.md" .||. "**.html") a]

        let pages a = case a of
                Branch a as -> concatMap pages as
                Leaf a -> [a | matches ("**.md" .||. "**.html") a]

        let categories a = do
                let children a = case a of
                        Branch a as -> [a]
                        Leaf a -> [a]

                case a of
                    Branch a as -> [(a, concatMap children as)] <> concatMap categories as
                    Leaf a -> []

        -- Compile footer
        match "footer.html" $ compile getResourceBody

        let fieldMaybeIcon def =
                field
                    "icon"
                    ( \a -> do
                        let id = itemIdentifier a
                        icon <- getMetadataField id "icon"
                        return $ fromMaybe def icon
                    )

        let fieldMaybeIconColor def =
                field
                    "icon-color"
                    ( \a -> do
                        let id = itemIdentifier a
                        color <- getMetadataField id "icon-color"
                        return $ fromMaybe def color
                    )

        let pageCtx =
                fieldMaybeIcon "file"
                    <> fieldMaybeIconColor "white"
                    <> siteCtx

        let breadcrumbCtx =
                listFieldWith
                    "breadcrumb"
                    pageCtx
                    ( \a -> do
                        let id = setVersion Nothing $ itemIdentifier a
                        let ids = fromMaybe (fail $ "No breadcrumb entry for " ++ show id) $ Data.Map.lookup id breadcrumbs
                        let ids' = setVersion (Just "slug") <$> ids
                        mapM load ids' :: Compiler [Item String]
                    )

        let categoryCtx =
                fieldMaybeIcon "custom-folder"
                    <> fieldMaybeIconColor "purple"
                    <> siteCtx

        -- TODO:
        -- \* Add a tag to the current page's menu entry so it can be highlighted
        -- \* Lift foldDetails into Compiler monad so details tags
        --   outside of the current breadcrumb can default to closed
        match (fromList $ routes identTree) $ do
            -- Menu panel
            version versionMenu $ do
                compile $ do
                    menuTree metaTree
                        >>= makeItem . renderHtml . H.nav . foldDetails

            -- Header line
            version versionHeader $ do
                compile $ do
                    makeEmptyItem >>= loadAndApplyTemplate headerTemplate (breadcrumbCtx <> pageCtx)

            -- Main page
            route recursiveRoute
            compile $
                makeEmptyItem
                    >>= applyLayoutTemplate metaTree pageCtx
                    >>= relativizeUrls
                    >>= prettifyUrls

        --- Specialized compilers
        let providers =
                defaultProviders
                    & withCompiler "None" getResourceBody
                    & withCompiler "Default" pandocCompiler'
                    & withCompiler "pandoc" pandocCompiler'
                    & withContext "None" siteCtx

        let spec a b c = CompilerSpec providers $ CompilerDefaults a b c

        let categories' = Data.Map.fromList $ categories identTree

        -- Compile pages
        let pageSpec = spec pandocCompiler' pageCtx postTemplate

        match (fromList $ pages identTree) $ do
            version versionBody $ do
                compile $
                    overridableCompiler pageSpec

            version versionSlug $
                compile $
                    slugCompiler pageCtx

        -- Compile categories
        let bodyCompiler = do
                ident <- getUnderlying
                let ident' = setVersion Nothing ident
                let pat = fromList $ fromMaybe mempty $ Data.Map.lookup ident' categories'

                let catDefaults = CompilerDefaults
                let catSpec =
                        spec
                            pandocCompiler'
                            ( listField "posts" postCtx (loadAll pat)
                                <> categoryCtx
                            )
                            categoryTemplate

                overridableCompiler catSpec

        match (fromList $ Data.Map.keys categories') $ do
            version versionBody $ do
                compile bodyCompiler

            version versionSlug $ do
                compile $
                    slugCompiler categoryCtx

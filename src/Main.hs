{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Tree as T

import Data.Traversable

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
import Text.HTML.TagSoup
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
import Data.Functor ((<&>))
import Data.List (intersperse, isInfixOf, isPrefixOf, isSuffixOf, sortBy)
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
        .||. "images/**.jpg"
        .||. "images/**.jpeg"
        .||. "images/**.gif"
        .||. "images/**.mkv"
        .||. "fonts/**.woff"
        .||. "fonts/**.woff2"

pageAssetPattern =
    "pages/**.png"
        .||. "pages/**.jpg"
        .||. "pages/**.jpeg"
        .||. "pages/**.gif"
        .||. "pages/**.mkv"

metadataPattern = "**.metadata"
notMetadataPattern = complement metadataPattern

imageAssetPattern = "**.png" .||. "**.jpeg" .||. "**.jpg" .||. "**.gif"
videoAssetPattern = "**.mkv" .||. "**.mp4" .||. "**.webm"

assetPattern = imageAssetPattern .||. videoAssetPattern
notAssetPattern = complement assetPattern

basePattern = "pages/**"

indexPattern = "**/index.*"
notIndexPattern = complement indexPattern

categoriesPattern = basePattern .&&. indexPattern .&&. hasNoVersion
pagesPattern = basePattern .&&. notIndexPattern .&&. notAssetPattern .&&. notMetadataPattern .&&. hasNoVersion

templatePattern = "templates/**.html"

documentTemplate = "templates/document.html"
footerTemplate = "templates/footer.html"
headerTemplate = "templates/content/header.html"
imageTemplate = "templates/image.html"
videoTemplate = "templates/video.html"
slugTemplate = "templates/slug.html"
flexColumnTemplate = "templates/flex-column.html"
flexRowTemplate = "templates/flex-row.html"
flexScrollTemplate = "templates/flex-scroll.html"

pageTemplate = "templates/post.html"
categoryTemplate = "templates/category.html"

cssTemplate = "css/main.scss"

highlightStyleDefault = "breezeDark"

versionSlug = "slug"
versionMenuSection = "menuSection"
versionHeader = "header"

--------------------------------------------------------------------------------
-- Business logic

applyFlexColumn = applyTernaryTemplate flexColumnTemplate
applyFlexRow = applyTernaryTemplate flexRowTemplate

collapseMenu :: String -> Tag String -> Tag String
collapseMenu url a = case a of
    TagOpen a as -> do
        let as' = Data.Map.fromList as
        case Data.Map.lookup "data-url" as' of
            Just dataUrl -> do
                let withAttr k v =
                        if dataUrl == "/./" || dataUrl `isPrefixOf` ("/" ++ url)
                            then Data.Map.insert k v
                            else id
                TagOpen a $
                    Data.Map.toList $
                        Data.Map.delete "data-url" $
                            ( case a of
                                "details" -> withAttr "open" ""
                                "summary" -> withAttr "id" "active"
                                "a" -> withAttr "id" "active"
                                _otherwise -> id
                            )
                                as'
            _otherwise -> TagOpen a as
    _otherwise -> a

applyLayoutTemplate :: Context String -> Item String -> Compiler (Item String)
applyLayoutTemplate ctx item = do
    let ident = itemIdentifier item

    let [identHeader] =
            flip setVersion ident . Just
                <$> [versionHeader]

    bodyHeader <- loadBody identHeader :: Compiler String

    menuBody <- loadBody (setVersion (Just versionMenuSection) (fromFilePath "pages/index.md"))

    route <- getRoute $ setVersion Nothing ident
    let route' = fromMaybe (fail ("No route for " ++ show ident)) route
    let menuBody' = withTags (collapseMenu route') menuBody

    menu <- makeItem ("<nav>" ++ menuBody' ++ "</nav>")

    menuHeader <-
        ( makeEmptyItem
                >>= loadAndApplyTemplate "templates/sidebar/header.html" ctx
            )
            <&> itemBody

    footer <- loadBody "footer.html"

    let panel header footer scrollCtx columnCtx =
            loadAndApplyTemplate flexScrollTemplate scrollCtx
                >=> applyFlexColumn header footer columnCtx

    body' <- panel (Just bodyHeader) Nothing ctx (constField "class" "body" <> ctx) item
    menu' <- panel (Just menuHeader) Nothing ctx (constField "class" "menu divider-right" <> ctx) menu

    makeItem (itemBody body')
        >>= applyFlexRow (Just (itemBody menu')) Nothing ctx
        >>= applyFlexColumn Nothing (Just footer) ctx
        >>= loadAndApplyTemplate documentTemplate ctx

-- Given a value type and a map from a key type to lists of that value type,
-- find the key corresponding to the set containing the provided value
lookupSetKey :: (Eq v) => v -> Data.Map.Map k [v] -> Maybe k
lookupSetKey val =
    Data.Map.foldrWithKey
        ( \k next acc ->
            if val `elem` next then Just k else acc
        )
        Nothing

-- Map from filetype name to file extensions
filetypeMap =
    Data.Map.fromList
        [ ("markdown", [".md"])
        , ("html", [".html"])
        , ("image", [".png", ".jpg", ".jpeg", ".gif"])
        , ("video", [".mkv", ".mp4", ".webm"])
        ]

-- Filetype context field
filetypeField fts =
    field
        "filetype"
        ( \item -> do
            let ident = itemIdentifier item
            let path = toFilePath ident
            case lookupSetKey (takeExtension path) fts of
                Just t -> return t
                Nothing -> fail "Unknown extension"
        )

-- Base Context
siteCtx = prettyUrlField "url" <> filetypeField filetypeMap <> defaultContext

-- Post Context
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` siteCtx

-- Hoist the provided route one directory upward
parentRoute =
    customRoute $
        replaceDirectory
            <*> joinPath . drop 1 . splitPath . takeDirectory
            <$> toFilePath

baseRoute = setExtension "html" `composeRoutes` parentRoute

makeEmptyItem = makeItem ""

-- Map a specific field to its metadata value, or a default if it is not present.
maybeMetadataField f d =
    field
        f
        ( \a -> do
            let id = itemIdentifier a
            v <- getMetadataField id f
            return $ fromMaybe d v
        )

maybeIconField = maybeMetadataField "icon"
maybeIconColorField = maybeMetadataField "icon-color"

slugCompiler ctx =
    makeEmptyItem >>= loadAndApplyTemplate slugTemplate ctx

-- Create a pandoc compiler with the provided highlight style
pandocCompilerWithStyle style =
    pandocCompilerWith
        defaultHakyllReaderOptions
        defaultHakyllWriterOptions
            { writerHighlightStyle = Just style
            }

-- Custom Hakyll configuration
-- Emulates the server config of GitHub Pages
extensionlessUrls config =
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

sortPosts :: [Item String] -> Compiler [Item String]
sortPosts posts = do
    postMeta <-
        mapM
            ( \a -> do
                let ident = itemIdentifier a
                meta <- getMetadata ident
                route <- getRoute ident
                return (a, meta, fromMaybe (fail "No route for " ++ show ident) route)
            )
            posts
    return $
        ( fmap (\(a, b, c) -> a)
            . sortBy
                ( \(a, am, ar) (b, bm, br) -> do
                    compare ("/index." `isInfixOf` br) ("/index." `isInfixOf` ar)
                )
            . sortBy
                ( \(a, am, ar) (b, bm, br) -> do
                    let icon = lookupString "icon"
                    compare (icon am) (icon bm)
                )
            . sortBy
                ( \(a, am, ar) (b, bm, br) -> do
                    compare a.itemIdentifier b.itemIdentifier
                )
        )
            postMeta

main :: IO ()
main = do
    hakyllWith (extensionlessUrls defaultConfiguration) $ do
        -- Load site config
        config <- getMetadata $ fromFilePath "config" :: Rules Metadata

        -- Extract highlight path
        let highlightPath =
                fromMaybe highlightStyleDefault $
                    parseMaybe (.: "highlightStyle") config

        -- Load pandoc style
        pandocStyle <-
            preprocess $
                runIOorExplode $
                    lookupHighlightingStyle highlightPath

        -- Create custom pandoc compiler
        let pandocCompiler' = pandocCompilerWithStyle pandocStyle

        -- Setup SASS handling
        sassHandling pandocStyle cssTemplate "css/**.scss"

        -- Compile templates
        match templatePattern $ compile templateBodyCompiler

        -- Compile static assets
        match staticAssetPattern $ do
            route idRoute
            compile copyFileCompiler

        -- Compile wallpapers
        -- wallpapers landscapeWallpaperPattern portraitWallpaperPattern

        -- Create contices
        let breadcrumbCtx =
                listFieldWith
                    "breadcrumb"
                    siteCtx
                    ( \item -> do
                        let ident = itemIdentifier item
                        let path = toFilePath ident
                        let path' =
                                if "/index." `isInfixOf` path
                                    then takeDirectory $ takeDirectory path
                                    else takeDirectory path

                        let dirs = splitDirectories path'
                        if dirs == ["."]
                            then return []
                            else do
                                let (path, breadcrumb) = foldr (\a (path, acc) -> (a, acc <> [last acc </> a])) (path, [""]) $ reverse dirs
                                let breadcrumb' =
                                        tail
                                            ( (setVersion (Just versionSlug) . fromFilePath)
                                                . (</> "index" <.> "md")
                                                <$> breadcrumb
                                            )
                                mapM load breadcrumb' :: Compiler [Item String]
                    )

        -- Compile footer
        match "footer.html" $ compile getResourceBody

        --- Baseline compilation
        let providers =
                defaultProviders
                    & withCompiler "None" getResourceBody
                    & withCompiler "Default" pandocCompiler'
                    & withCompiler "pandoc" pandocCompiler'
                    & withContext "None" siteCtx

        let spec a b c = CompilerSpec providers $ CompilerDefaults a b c

        let makeIconCtx icon color = maybeIconField icon <> maybeIconColorField color
        let makeSlug = version versionSlug . compile . slugCompiler

        let menuSection ctx = do
                ident <- getUnderlying
                item <- loadBody $ setVersion (Just versionSlug) ident
                makeItem item
                    >>= loadAndApplyTemplate "templates/link.html" ctx

        let makeMenuSectionWith = version versionMenuSection . compile
        let makeMenuSection = makeMenuSectionWith . menuSection

        let makeHeader ctx = version versionHeader $ do
                compile $ do
                    makeEmptyItem
                        >>= loadAndApplyTemplate headerTemplate ctx

        let liftPath =
                mapContextBy (== "path") (\a -> "/" ++ joinPath (tail $ splitDirectories a))

        -- Compile images
        match (basePattern .&&. imageAssetPattern .&&. hasNoVersion) $ do
            let iconCtx = makeIconCtx "image" "white"
            makeSlug (iconCtx <> siteCtx)
            makeMenuSection siteCtx
            makeHeader (breadcrumbCtx <> iconCtx <> siteCtx)

            version "image" $ do
                route parentRoute
                compile copyFileCompiler

            route baseRoute
            compile $
                makeEmptyItem
                    >>= loadAndApplyTemplate
                        imageTemplate
                        (liftPath siteCtx)
                    >>= applyLayoutTemplate siteCtx
                    >>= relativizeUrls

        -- Compile videos
        match (basePattern .&&. videoAssetPattern .&&. hasNoVersion) $ do
            let iconCtx = makeIconCtx "video" "white"
            makeSlug (iconCtx <> siteCtx)
            makeMenuSection siteCtx
            makeHeader (breadcrumbCtx <> iconCtx <> siteCtx)

            version "video" $ do
                route parentRoute
                compile copyFileCompiler

            route baseRoute
            compile $
                makeEmptyItem
                    >>= loadAndApplyTemplate
                        videoTemplate
                        (liftPath siteCtx)
                    >>= applyLayoutTemplate siteCtx
                    >>= relativizeUrls

        -- Compile pages
        let pageSpec = spec pandocCompiler' siteCtx pageTemplate

        match pagesPattern $ do
            let iconCtx = makeIconCtx "file" "white"
            makeSlug (iconCtx <> siteCtx)
            makeMenuSection siteCtx
            makeHeader (breadcrumbCtx <> iconCtx <> siteCtx)

            route baseRoute
            compile $
                overridableCompiler pageSpec
                    >>= applyLayoutTemplate siteCtx
                    >>= relativizeUrls

        -- Compile categories
        let children ident = do
                let path = toFilePath ident
                let childPages = fromGlob (replaceFileName path "*.*") .&&. notIndexPattern
                let childCats = fromGlob $ replaceFileName path "*/index.*"
                let pat = (childPages .||. childCats) .&&. hasVersion versionMenuSection
                ids <- getMatches pat :: Compiler [Identifier]
                this <- loadBody $ setVersion (Just "slug") ident :: Compiler String
                loadAll (fromList ids) >>= sortPosts :: Compiler [Item String]

        let categoryMenuSection item = do
                ident <- getUnderlying
                let ctx = listField "children" postCtx (children ident) <> siteCtx
                loadAndApplyTemplate "templates/menu-section.html" ctx item

        let catSpec ident =
                spec
                    pandocCompiler'
                    ( listField "posts" postCtx (children ident)
                        <> siteCtx
                    )
                    categoryTemplate

        match categoriesPattern $ do
            let iconCtx = makeIconCtx "custom-folder" "purple"
            makeSlug (iconCtx <> siteCtx)
            makeMenuSectionWith $ menuSection siteCtx >>= categoryMenuSection
            makeHeader (breadcrumbCtx <> iconCtx <> siteCtx)

            route baseRoute
            compile $ do
                ident <- getUnderlying
                overridableCompiler (catSpec ident)
                    >>= applyLayoutTemplate siteCtx
                    >>= relativizeUrls

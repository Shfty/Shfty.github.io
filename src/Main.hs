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

indexPattern = "**index.*"
notIndexPattern = complement indexPattern

categoriesPattern = basePattern .&&. indexPattern
pagesPattern = basePattern .&&. notIndexPattern .&&. notAssetPattern .&&. notMetadataPattern

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
                TagOpen a
                    $ Data.Map.toList
                    $ Data.Map.delete "data-url"
                    $ ( case a of
                            "details" -> withAttr "open" ""
                            "summary" -> withAttr "id" "active"
                            "a" -> withAttr "id" "active"
                            _otherwise -> id
                      )
                    $ as'
            _otherwise -> TagOpen a as
    _otherwise -> a

applyLayoutTemplate :: Context String -> Item String -> Compiler (Item String)
applyLayoutTemplate ctx item = do
    let ident = itemIdentifier item

    route <- getRoute $ setVersion Nothing ident
    let route' = fromMaybe (fail ("No route for " ++ show ident)) route

    let [identHeader] =
            flip setVersion ident . Just
                <$> [versionHeader]

    bodyHeader <- loadBody identHeader :: Compiler String

    menuBody <- loadBody (setVersion (Just versionMenuSection) (fromFilePath "pages/index.md"))

    let menuBody' = withTags (collapseMenu route') menuBody

    menu <- makeItem ("<nav>" ++ menuBody' ++ "</nav>")

    menuHeader <- makeEmptyItem >>= loadAndApplyTemplate "templates/sidebar/header.html" ctx >>= return . itemBody :: Compiler String
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

-- Static assets (images, fonts, etc.)
staticAssets = match staticAssetPattern $ do
    route idRoute
    compile copyFileCompiler

-- Filetype context field
filetypeField =
    field
        "filetype"
        ( \item -> do
            let ident = itemIdentifier item
            let path = toFilePath ident
            return $ case takeExtension path of
                ".md" -> "markdown"
                ".html" -> "html"
                ".png" -> "image"
                ".jpg" -> "image"
                ".jpeg" -> "image"
                ".gif" -> "image"
                ".mkv" -> "video"
                ".mpv" -> "video"
                ".webm" -> "video"
                _ -> fail "Unknown extension"
        )

-- Base Context
siteCtx = prettyUrlField "url" <> filetypeField <> defaultContext

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

baseRoute = composeRoutes (setExtension "html") parentRoute

makeIdentTree fileTree = do
    let isIndex = (== "index") . takeBaseName

    let indexBranches a = case a of
            Branch a as -> Branch (a </> "index.md") (indexBranches <$> as)
            Leaf a -> Leaf a

    let fileTree' = uncurry (</>) . swap <$> tag fileTree
    let fileTree'' = filterFileTree (not . isIndex) fileTree'
    fromFilePath <$> indexBranches fileTree''

foldDetails (Branch a as) = H.details H.! A.open "" $ H.toHtml a <> mconcat (foldDetails <$> as)
foldDetails (Leaf a) = H.toHtml a

makeEmptyItem = makeItem ""

fieldMaybeIcon def =
    field
        "icon"
        ( \a -> do
            let id = itemIdentifier a
            icon <- getMetadataField id "icon"
            return $ fromMaybe def icon
        )

fieldMaybeIconColor def =
    field
        "icon-color"
        ( \a -> do
            let id = itemIdentifier a
            color <- getMetadataField id "icon-color"
            return $ fromMaybe def color
        )

slugCompiler icon color ctx =
    makeEmptyItem
        >>= loadAndApplyTemplate
            slugTemplate
            ( fieldMaybeIcon icon
                <> fieldMaybeIconColor color
                <> ctx
            )

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

prettifyUrls :: Item String -> Compiler (Item String)
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

        -- Contices
        let imageSlugCompiler = slugCompiler "image" "white"
        let videoSlugCompiler = slugCompiler "video" "white"
        let pageSlugCompiler = slugCompiler "file" "white"
        let categorySlugCompiler = slugCompiler "custom-folder" "purple"

        -- Tree / Breadcrumb Data

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

        let breadcrumbs_ = makeBreadcrumbs [] identTree
        -- error $ mconcat $ intersperse "\n" $ show <$> breadcrumbs_

        let breadcrumbs = Data.Map.fromList breadcrumbs_

        let breadcrumbCtx =
                listFieldWith
                    "breadcrumb"
                    siteCtx
                    ( \a -> do
                        let id = setVersion Nothing $ itemIdentifier a
                        let ids = fromMaybe (fail $ "No breadcrumb entry for " ++ show id) $ Data.Map.lookup id breadcrumbs
                        let ids' = setVersion (Just "slug") <$> ids
                        mapM load ids' :: Compiler [Item String]
                    )

        -- Baseline compilation
        match (basePattern .&&. imageAssetPattern .&&. hasNoVersion) $ do
            version versionSlug $
                compile $
                    imageSlugCompiler siteCtx

            version versionMenuSection $ do
                compile $ do
                    ident <- getUnderlying
                    item <- loadBody $ setVersion (Just "slug") ident
                    makeItem item >>= loadAndApplyTemplate "templates/link.html" siteCtx

            version versionHeader $ do
                compile $ do
                    makeEmptyItem >>= loadAndApplyTemplate headerTemplate (breadcrumbCtx <> siteCtx)
        
            version "image" $ do
                route parentRoute
                compile copyFileCompiler
            
            route baseRoute
            compile $
                makeEmptyItem
                    >>= loadAndApplyTemplate imageTemplate (mapContextBy (== "path") (\a -> "/" ++ (joinPath $ tail $ splitDirectories a)) siteCtx)
                    >>= applyLayoutTemplate siteCtx
                    >>= relativizeUrls

        match (basePattern .&&. videoAssetPattern .&&. hasNoVersion) $ do
            version versionSlug $
                compile $
                    videoSlugCompiler siteCtx

            version versionMenuSection $ do
                compile $ do
                    ident <- getUnderlying
                    item <- loadBody $ setVersion (Just "slug") ident
                    makeItem item >>= loadAndApplyTemplate "templates/link.html" siteCtx

            version versionHeader $ do
                compile $ do
                    makeEmptyItem >>= loadAndApplyTemplate headerTemplate (breadcrumbCtx <> siteCtx)

            version "video" $ do
                route parentRoute
                compile copyFileCompiler
            
            route baseRoute
            compile $
                makeEmptyItem
                    >>= loadAndApplyTemplate videoTemplate (mapContextBy (== "path") (\a -> "/" ++ (joinPath $ tail $ splitDirectories a)) siteCtx)
                    >>= applyLayoutTemplate siteCtx
                    >>= relativizeUrls

        match (pagesPattern .&&. hasNoVersion) $ do
            version versionSlug $
                compile $
                    pageSlugCompiler siteCtx

            version versionMenuSection $ do
                compile $ do
                    ident <- getUnderlying
                    item <- loadBody $ setVersion (Just "slug") ident
                    makeItem item >>= loadAndApplyTemplate "templates/link.html" siteCtx

        match (categoriesPattern .&&. hasNoVersion) $ do
            version versionSlug $
                compile $
                    categorySlugCompiler siteCtx

            version versionMenuSection $ do
                compile $ do
                    ident <- getUnderlying
                    let path = toFilePath ident
                    let childPages = fromGlob (replaceFileName path "*.*") .&&. notIndexPattern
                    let childCats = fromGlob $ replaceFileName path "*/index.*"
                    let pat = (childPages .||. childCats) .&&. hasVersion versionMenuSection
                    ids <- getMatches pat :: Compiler [Identifier]
                    this <- loadBody $ setVersion (Just "slug") ident :: Compiler String
                    let children = loadAll (fromList ids) >>= sortPosts :: Compiler [Item String]
                    let ctx = listField "children" postCtx children <> siteCtx
                    makeItem this
                        >>= loadAndApplyTemplate "templates/link.html" siteCtx
                        >>= loadAndApplyTemplate "templates/menu-section.html" ctx

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

        match ((fromList $ routes identTree) .&&. hasNoVersion) $ do
            -- Header line
            version versionHeader $ do
                compile $ do
                    makeEmptyItem >>= loadAndApplyTemplate headerTemplate (breadcrumbCtx <> siteCtx)

            -- Final page
            route baseRoute
            compile $ do
                ident <- getUnderlying
                load (setVersion (Just versionBody) ident)
                    >>= applyLayoutTemplate siteCtx
                    >>= relativizeUrls

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
        let pageSpec = spec pandocCompiler' siteCtx pageTemplate

        match ((fromList $ pages identTree) .&&. hasNoVersion) $ do
            version versionBody $ do
                compile $
                    overridableCompiler pageSpec

        -- Compile categories
        let bodyCompiler = do
                ident <- getUnderlying
                let ident' = setVersion Nothing ident
                let pat = fromList $ fromMaybe mempty $ Data.Map.lookup ident' categories'

                let catDefaults = CompilerDefaults

                let posts = loadAll pat >>= sortPosts
                let catSpec =
                        spec
                            pandocCompiler'
                            ( listField "posts" postCtx posts
                                <> siteCtx
                            )
                            categoryTemplate

                overridableCompiler catSpec

        match ((fromList $ Data.Map.keys categories') .&&. hasNoVersion) $ do
            version versionBody $ do
                compile bodyCompiler

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

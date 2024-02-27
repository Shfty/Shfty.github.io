{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Control.Exception (try)
import Control.Monad
import Control.Monad.Fail
import Control.Monad.Loops (concatM)
import qualified Data.Aeson as JSON
import Data.Bifunctor (second)
import qualified Data.ByteString.Lazy as ByteString
import Data.Foldable (traverse_)
import Data.Functor.Identity
import Data.List
import Data.List.Utils (replace)
import Data.Maybe
import Data.Monoid (mappend)
import Data.String (fromString)
import Data.Text (splitOn, strip)
import Data.Text.Conversions (fromText)
import Debug.Trace
import GHC.IO.Exception (IOException (..))
import Hakyll
import qualified Hakyll.Core.Logger as L
import Hakyll.Images (Height, Width, ensureFitCompiler, loadImage)
import Hakyll.Web.Sass (sassCompiler)
import System.FilePath
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (customAttribute, customLeaf)
import Text.Pandoc.Highlighting (Style, breezeDark, styleToCss)
import Text.Pandoc.Options (ReaderOptions (..), WriterOptions (..))

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

-------------------------------------------------------------------------------
-- Custom highlight style loading

printException (Left e) = do
    let filename = case ioe_filename e of
            Just a -> "Error reading from " ++ a
            Nothing -> "Error reading file"
    putStrLn $ filename ++ ": " ++ ioe_description e
    return Nothing
printException (Right t) = return $ Just t

tryReadFile :: FilePath -> IO (Maybe ByteString.ByteString)
tryReadFile f = try (ByteString.readFile f) >>= printException

maybeDecode (Left l) = Nothing <$ print l
maybeDecode (Right r) = return $ Just r

tryParse = maybeDecode <$> JSON.eitherDecode

maybeM d = maybe (return d)

tryParseStyle :: Style -> ByteString.ByteString -> IO Style
tryParseStyle d a = tryParse a >>= maybeM d return

tryLoadStyle :: Style -> FilePath -> Rules Style
tryLoadStyle d a = preprocess $ tryReadFile a >>= maybeM d (tryParseStyle d)

-- Create a pandoc compiler with the provided highlight style
pandocCompilerWithStyle style =
    pandocCompilerWith
        defaultHakyllReaderOptions
        defaultHakyllWriterOptions
            { writerHighlightStyle = Just style
            }

--------------------------------------------------------------------------------
-- Business logic

inits' = filter (not . null) . inits

pop [] = []
pop xs = init xs

getBreadcrumbWith :: (MonadMetadata m) => ([FilePath] -> [String]) -> Identifier -> m [String]
getBreadcrumbWith f ident = return $ stripSlash . mconcat <$> inits' (f $ (splitPath . takeDirectory . toFilePath) ident)

getBreadcrumb :: (MonadMetadata m) => Identifier -> m [String]
getBreadcrumb = getBreadcrumbWith id

isSlash a = a == '/'

stripSlash = dropWhileEnd isSlash

last' :: [a] -> a
last' [a] = a
last' (a : as) = last' as

applyTernaryTemplate template before after ctx = do
    let ctx' =
            mconcat $
                catMaybes
                    [ field "before" <$> before
                    , field "after" <$> after
                    ]
    let ctx'' = ctx' <> ctx

    loadAndApplyTemplate template ctx''

applyFlexColumn = applyTernaryTemplate flexColumnTemplate
applyFlexRow = applyTernaryTemplate flexRowTemplate

applyDefaultTemplate ctx = do
    let header = loadAndApplyTemplate headerTemplate ctx >=> return . itemBody
    let footer = loadAndApplyTemplate footerTemplate ctx >=> return . itemBody

    let panel ctx =
            loadAndApplyTemplate flexScrollTemplate ctx
                >=> applyFlexColumn (Just header) Nothing ctx

    let sidebar =
            const $
                load "menu.html"
                    >>= (panel ctx >=> return . itemBody)
    return
        >=> panel ctx
        >=> applyFlexRow (Just sidebar) Nothing ctx
        >=> applyFlexColumn Nothing (Just footer) ctx
        >=> loadAndApplyTemplate htmlTemplate ctx

recursivePages pat compiler ctx = match pat $ do
    route recursiveRoute
    compile $
        compiler
            >>= loadAndApplyTemplate postTemplate ctx
            >>= applyDefaultTemplate ctx
            >>= relativizeUrls

-- Static assets (images, fonts, etc.)
staticAssets = match staticAssetPattern $ do
    route idRoute
    compile copyFileCompiler

-- SASS hot-reloading
sassHandling style = do
    scssDependency <- makePatternDependency cssTemplate
    rulesExtraDependencies [scssDependency] $ match cssTemplate $ do
        route $ setExtension "css"
        compile (fmap compressCss <$> sassCompiler)

    create ["css/syntax.css"] $ do
        route idRoute
        compile $ makeItem $ styleToCss style

-- Post Context
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext

-- Templates
templates = match "templates/**.html" $ compile templateBodyCompiler

-- Modified version of tagsRules
-- Uses match instead of create to allow tag pages
-- to have static content
tagsRules' tags rules =
    forM_ (tagsMap tags) $ \(tag, identifiers) ->
        rulesExtraDependencies [tagsDependency tags] $
            match (fromList [tagsMakeId tags tag]) $
                rules tag $
                    fromList identifiers

breadcrumbFieldWith getTags' = tagsFieldWith' getTags' renderBreadcrumb concatBreadcrumb
breadcrumbField = breadcrumbFieldWith getBreadcrumb

concatBreadcrumb = do
    let sep = preSpace <> leftSoftDivider <> preSpace
    mconcat . intersperse sep

renderBreadcrumb :: (MonadFail m, MonadMetadata m) => Identifier -> Maybe FilePath -> m (Maybe H.Html)
renderBreadcrumb id path = do
    title <- getMetadataField' id "title"
    return
        $ Just
        $ H.a
            H.! A.title (H.stringValue ("All pages tagged '" ++ title ++ "'."))
            H.! A.href (H.toValue $ toUrl $ fromMaybe "Nothing" path)
            H.! A.rel "tag"
        $ H.toHtml title

-- Modified version of tagsFieldWith
-- Takes (Identifier -> Maybe FilePath -> Compiler (Maybe a)) in its renderLink function,
-- thus allowing the rendering code to lookup tag metadata, such as its title
tagsFieldWith' getTags' renderLink cat key tags = field key $ \item -> do
    tags' <- getTags' $ itemIdentifier item
    links <- forM tags' $ \tag -> do
        let id = tagsMakeId tags tag
        route' <- getRoute id
        renderLink id route'

    return $ renderHtml $ cat $ catMaybes links

-- Custom HTML tags
glyphTag = H.stringTag "x-glyph"
glyph = customLeaf glyphTag True
glyphTy = customAttribute "type"

leftSoftDivider = glyph H.! glyphTy "left-soft-divider"
preSpace = H.pre " "

-- Printer for Tags
showTagEntry :: (String, [Identifier]) -> String
showTagEntry (k, v) = k ++ ":\n\t" ++ mconcat ((++ "\n\t") . toFilePath <$> v)

showTagsMap :: [(String, [Identifier])] -> String
showTagsMap = mconcat . intersperse "\n" . fmap showTagEntry

showTags = showTagsMap . tagsMap

-- Generate a list of progressively smaller (width, height) tuples
wallpaperSizes :: Int -> [Int] -> Width -> Height -> [(Width, Height)]
wallpaperSizes fac range w h =
    [ (w `div` p, h `div` p)
    | p <-
        [ fac ^ y
        | y <- range
        ]
    ]

wallpaperSizes2 = wallpaperSizes 2

-----------------------------------------------------------------------------------------

makeWallpaper pat (width, height) = do
    let size = show width ++ "-" ++ show height
    match pat $ version size $ do
        route $
            customRoute
                ( \a -> do
                    let path = toFilePath a
                    let dir = takeDirectory path
                    let base = takeBaseName path
                    let ext = takeExtension path
                    let a = dir </> base ++ "-" ++ size <.> ext
                    a
                )
        compile $
            loadImage
                >>= ensureFitCompiler width height

wallpapers = do
    -- Full-size wallpapers
    match wallpaperPattern $ do
        route idRoute
        compile copyFileCompiler

    -- Generate progressively-smaller copies
    let landscapeSizes = wallpaperSizes2 [0 .. 4] 3840 2160
    let portraitSizes = wallpaperSizes2 [0 .. 4] 2160 3840
    let makeLandscape = makeWallpaper landscapeWallpaperPattern
    let makePortrait = makeWallpaper portraitWallpaperPattern

    traverse_ makeLandscape landscapeSizes
    traverse_ makePortrait portraitSizes

-- Hoist the provided route one directory upward
parentRoute =
    customRoute $
        replaceDirectory
            <*> joinPath . drop 1 . splitPath . takeDirectory
            <$> toFilePath

recursiveRoute = composeRoutes (setExtension "html") parentRoute

main :: IO ()
main = do
    -- Hakyll composition
    hakyll $ do
        pandocStyle <- tryLoadStyle highlightStyleDefault highlightStyleJson
        let pandocCompiler' = pandocCompilerWithStyle pandocStyle

        wallpapers

        staticAssets

        sassHandling pandocStyle

        let metadataPattern = "**/*.metadata"
        let imagePattern = "**/*.png" .||. "**/*.jpg" .||. "**/*.gif"
        let videoPattern = "**/*.mkv" .||. "**/*.mp4"

        let assetPattern = metadataPattern .||. imagePattern .||. videoPattern
        let notAssetPattern = complement assetPattern

        let recursivePattern = "pages/**" .&&. notAssetPattern
        let indexPattern = "**/index.*"
        let notIndexPattern = complement indexPattern
        let recursiveCategoriesPattern = recursivePattern .&&. indexPattern
        let recursivePagesPattern = recursivePattern .&&. notIndexPattern

        -- Assemble testing menu
        fileTree <- makeFileTree "pages/**"

        create ["menu.html"] $ do
            compile $ do
                let isIndex = (== "index") . takeBaseName

                let fileTree' = do
                        let fileTree'' = ("",) <$> fileTree

                        let go a = case a of
                                Branch (acc, a) as -> do
                                    let a' = (acc </>) <$> a
                                    let a'' = unNode a'
                                    let as' = fmap (runIdentity . go . fmap (\(_, b) -> (a'', b))) as
                                    Identity $ Branch a' as'
                                Leaf (acc, a) -> Identity $ Leaf ((acc </>) <$> a)

                        runIdentity $ runIdentity $ traverse go (Identity fileTree'')

                let fileTree'' = filterFileTree (not . isIndex . unNode) fileTree'

                let indexCategories a = case a of
                        Category a -> Category (a </> "index.md")
                        Page a -> Page a

                let fileTree''' = indexCategories <$> fileTree''

                mt <- menuTree fileTree'''

                let foldDetails t =
                        case t of
                            (Branch a as) ->
                                H.details H.! A.open "" $
                                    H.toHtml a <> mconcat (foldDetails <$> as)
                            (Leaf b) -> H.toHtml b

                let html = renderHtml $ foldDetails mt

                makeItem html

        -- Build page breadcrumb data, compiler
        breadcrumbs <- buildTagsWith getBreadcrumb recursivePagesPattern $ fromCapture "*/index.md"
        -- error $ showTags breadcrumbs

        let getCategory' a = do
                let path = toFilePath a
                let file = takeFileName path
                let page = takeDirectory path
                return $
                    if takeBaseName file == "index"
                        then [takeDirectory page]
                        else [page]

        let pageCtx = breadcrumbField "breadcrumb" breadcrumbs `mappend` defaultContext
        -- recursivePages recursivePagesPattern pandocCompiler' (pageCtx <> defaultContext)

        let recursivePageCompiler ident = do
                meta <- getMetadataField ident "template"
                debugCompiler $ "TEMPLATE META: " ++ show meta

                pandocCompiler'
                    >>= loadAndApplyTemplate postTemplate pageCtx
                    >>= applyDefaultTemplate pageCtx
                    >>= relativizeUrls

        -- Build category structural data, compiler
        catCats <- buildTagsWith getCategory' recursivePattern $ fromCapture "*/index.md"
        -- error $ showTags catCats

        let categoryCtx = breadcrumbFieldWith (getBreadcrumbWith pop) "breadcrumb" breadcrumbs

        let recursiveCategoryCompilier ident postPat = do
                posts <- loadAll postPat
                let indexCtx =
                        listField "posts" postCtx (return posts)
                            `mappend` categoryCtx
                            `mappend` defaultContext

                metaCompiler <- getMetadataField ident "compiler"
                let compiler = case metaCompiler of
                        Nothing -> pandocCompiler'
                        Just "pandoc" -> pandocCompiler'
                        Just "None" -> getResourceBody

                metaTemplates <- getMetadataField ident "templates"
                let applyCategoryTemplate = loadAndApplyTemplate categoryTemplate indexCtx
                let applyTemplates = case metaTemplates of
                        Nothing -> applyCategoryTemplate
                        Just a -> do
                            let templates = fromText <$> (strip <$> splitOn "," (fromString a)) :: [String]

                            let go a = case a of
                                    "None" -> return
                                    "Self" -> applyAsTemplate indexCtx
                                    "Default" -> applyCategoryTemplate
                                    a -> loadAndApplyTemplate (fromFilePath a) indexCtx

                            concatM $ map go templates
                -- Just a -> loadAndApplyTemplate (fromFilePath a) indexCtx

                compiler
                    >>= applyTemplates
                    >>= applyCategoryTemplate
                    >>= relativizeUrls

        -- Compile categories and their child pages based on generated structural data
        tagsRules'
            catCats
            ( \tag pattern -> do
                route recursiveRoute
                compile $ do
                    ident <- getUnderlying
                    recursiveCategoryCompilier ident pattern

                match (pattern .&&. notIndexPattern) $ do
                    route recursiveRoute
                    compile $ do
                        ident <- getUnderlying
                        recursivePageCompiler ident
            )

        -- Compile categories with no children
        match recursiveCategoriesPattern $ do
            route recursiveRoute
            compile $ do
                ident <- getUnderlying
                recursiveCategoryCompilier ident ""

        templates

data Node a = Category !a | Page !a deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

unNode (Category a) = a
unNode (Page a) = a

instance Applicative Node where
    pure = Page
    a <*> b = a >>= (<$> b)

instance Monad Node where
    Category a >>= f = f a
    Page a >>= f = f a

data FileTree a = Branch !a ![FileTree a] | Leaf !a deriving (Eq, Ord, Functor, Foldable, Traversable)

instance (Show a) => Show (FileTree a) where
    show (Branch a as) = replace "\n" "\n\t" $ show a ++ ":\n" ++ mconcat ((++ "\n") . show <$> as)
    show (Leaf b) = show b

menuTree :: FileTree (Node FilePath) -> Compiler (FileTree H.Html)
menuTree = do
    let href path = A.href $ H.toValue path

    let getInfo a = do
            let id = fromFilePath a
            title <- fromMaybe a <$> getMetadataField id "title"
            route <- fromMaybe (fail "No route") <$> getRoute id
            return (title, route)

    let go a = case a of
            (Category a) -> do
                (title, route) <- getInfo a
                return $
                    H.summary (H.a H.! href route $ H.toHtml title)
            (Page b) -> do
                (title, route) <- getInfo b
                return $
                    H.a H.! href route $
                        H.toHtml title

    traverse go

-- Create a chain of branches from a list
makePath [x] = Leaf (Page x)
makePath (x : xs) = Branch (Category x) [makePath xs]

makePaths items =
    makePath . splitDirectories . toFilePath <$> items

makeFileTree pat = do
    items <- getMatches pat
    let paths = makePaths items
    let [fs] = foldl' concatFileTree [] (return <$> paths)
    return $ sortFileTree fs

-- Recursively merge chains created by makeFS into a single tree
concatFileTree :: (Ord a) => [FileTree a] -> [FileTree a] -> [FileTree a]
concatFileTree [Branch a as] [Branch b bs] =
    let go = foldr (concatFileTree . (: [])) []
     in if a == b
            then [Branch a $ go (as <> bs)]
            else [Branch a $ go as, Branch b $ go bs]
concatFileTree a b = a <> b

-- Recursive sort
sortFileTree (Branch x xs) = Branch x $ sort (sortFileTree <$> xs)
sortFileTree a = a

-- Filter leaves by their value
filterFileTree f a = do
    let go (Branch x xs) = [Branch x $ mconcat $ go <$> xs]
        go (Leaf a) = ([Leaf a | f a])
    let [indices] = go a
    indices

-- Accumulate branch values, and fold over leaf values
foldFileTree f acc (Branch a xs) = Branch a $ foldFileTree f (f acc a) <$> xs
foldFileTree f acc (Leaf b) = Leaf $ f acc b

-- Accumulate and fold over branch values
foldFileTree' f acc (Branch a xs) = do
    let a' = f acc a
    Branch a' $ foldFileTree' f a' <$> xs
foldFileTree' f acc (Leaf b) = Leaf b

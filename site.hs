{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (try)
import Control.Monad.Fail
import qualified Data.Aeson as JSON
import Data.Bifunctor (second)
import qualified Data.ByteString.Lazy as ByteString
import Data.List
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend)
import Data.String (fromString)
import Data.Text (strip)
import Debug.Trace
import GHC.IO.Exception (IOException (..))
import Hakyll
import Hakyll.Web.Sass (sassCompiler)
import System.FilePath
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Pandoc.Highlighting (Style, breezeDark, styleToCss)
import Text.Pandoc.Options (ReaderOptions (..), WriterOptions (..))

--------------------------------------------------------------------------------
-- Constants

staticPagePattern = "pages/**.md"

staticAssetPattern =
    "images/*"
        .||. "posts/**.png"
        .||. "posts/**.gif"
        .||. "posts/**.mkv"
        .||. "fonts/**.woff"
        .||. "fonts/**.woff2"

blogPattern = "posts/blog/**.md"
projectsPattern = "posts/projects/**.md"

defaultTemplate = "templates/default.html"
projectsTemplate = "templates/projects.html"
blogTemplate = "templates/blog.html"
postTemplate = "templates/post.html"

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

tryLoadStyle :: Style -> FilePath -> IO Style
tryLoadStyle d a = tryReadFile a >>= maybeM d (tryParseStyle d)

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

getBreadcrumb :: (MonadMetadata m) => Identifier -> m [String]
getBreadcrumb a = do
    let path = toFilePath a
    let file = takeFileName path
    let dir = takeDirectory path
    let segments = splitPath dir
    let breadcrumb = stripSlash . mconcat <$> inits' segments
    let breadcrumb = if file == "index.md" then drop 1 breadcrumb else breadcrumb
    return breadcrumb

buildBreadcrumb ::
    (MonadMetadata m) =>
    Pattern ->
    (String -> Identifier) ->
    m Tags
buildBreadcrumb = buildTagsWith getBreadcrumb

isSlash a = a == '/'

stripSlash = dropWhileEnd isSlash

last' :: [a] -> a
last' [a] = a
last' (a : as) = last' as

simpleRenderLink' :: String -> Maybe FilePath -> Maybe H.Html
simpleRenderLink' tag path =
    Just
        $ H.a
            H.! A.title (H.stringValue ("All pages tagged '" ++ tag ++ "'."))
            H.! A.href (H.toValue $ toUrl $ fromMaybe "Nothing" path)
            H.! A.rel "tag"
        $ H.toHtml . last' . splitPath
        $ tag

recursivePages ctx = do
    match "recursive/**" $ do
        route $ setExtension "html"
        compile $
            do
                pandocCompiler
                >>= loadAndApplyTemplate postTemplate ctx
                >>= loadAndApplyTemplate defaultTemplate ctx
                >>= relativizeUrls

-- Load or default a code highlighting style,
-- and use it to create a pandoc compiler
pandocCompiler' :: IO (Style, Compiler (Item String))
pandocCompiler' = do
    pandocStyle <- tryLoadStyle highlightStyleDefault highlightStyleJson
    return (pandocStyle, pandocCompilerWithStyle pandocStyle)

-- Static assets (images, fonts, etc.)
staticAssets = do
    match staticAssetPattern $ do
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

-- Static pages
staticPages compiler = do
    match staticPagePattern $ do
        route $ composeRoutes (setExtension "html") (gsubRoute "pages" $ const ".")
        compile $
            compiler
                >>= loadAndApplyTemplate defaultTemplate defaultContext
                >>= relativizeUrls

-- Post Context
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext

-- Blog pages
blogPages compiler = do
    match blogPattern $ do
        route $ setExtension "html"
        compile $
            compiler
                >>= loadAndApplyTemplate postTemplate postCtx
                >>= loadAndApplyTemplate defaultTemplate postCtx
                >>= relativizeUrls

    create ["blog.html"] $ do
        route idRoute
        compile $ do
            posts <- loadAll blogPattern
            let blogCtx =
                    listField "posts" postCtx (return posts)
                        `mappend` constField "title" "Blog"
                        `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate blogTemplate blogCtx
                >>= loadAndApplyTemplate defaultTemplate blogCtx
                >>= relativizeUrls

-- Project pages
projectPages compiler = do
    match projectsPattern $ do
        route $ setExtension "html"
        compile $
            compiler
                >>= loadAndApplyTemplate postTemplate postCtx
                >>= loadAndApplyTemplate defaultTemplate postCtx
                >>= relativizeUrls

    create ["projects.html"] $ do
        route idRoute
        compile $ do
            posts <- loadAll projectsPattern
            let projectsCtx =
                    listField "posts" postCtx (return posts)
                        `mappend` constField "title" "Projects"
                        `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate projectsTemplate projectsCtx
                >>= loadAndApplyTemplate defaultTemplate projectsCtx
                >>= relativizeUrls

-- Root page
rootPage = do
    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- loadAll blogPattern
            let indexCtx =
                    listField "posts" postCtx (return posts)
                        `mappend` defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate defaultTemplate indexCtx
                >>= relativizeUrls

-- Templates
templates = match "templates/**.html" $ compile templateBodyCompiler

-----------------------------------------------------------------------------------------

main :: IO ()
main = do
    (pandocStyle, pandocCompiler') <- pandocCompiler'

    -- Hakyll composition
    hakyll $ do
        staticAssets

        sassHandling pandocStyle

        staticPages pandocCompiler'

        blogPages pandocCompiler'

        projectPages pandocCompiler'

        breadcrumb <- buildBreadcrumb "recursive/**" $ fromCapture "*/index.md"
        let concatHtml = mconcat . intersperse " > "
        let tagsCtx = tagsFieldWith getBreadcrumb simpleRenderLink' concatHtml "breadcrumb" breadcrumb
        recursivePages (tagsCtx <> postCtx)

        rootPage

        templates

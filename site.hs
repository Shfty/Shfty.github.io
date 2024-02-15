--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (try)
import Control.Monad.Fail
import qualified Data.Aeson as JSON
import Data.Bifunctor (second)
import qualified Data.ByteString.Lazy as ByteString
import Data.Monoid (mappend)
import Data.String (fromString)
import GHC.IO.Exception (IOException (..))
import Hakyll
import Hakyll.Web.Sass (sassCompiler)
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

main :: IO ()
main = do
    -- Load or default a code highlighting style,
    -- and use it to create a pandoc compiler
    pandocStyle <- tryLoadStyle highlightStyleDefault highlightStyleJson
    let pandocCompiler' = pandocCompilerWithStyle pandocStyle

    -- Hakyll composition
    hakyll $ do
        -- Images, Fonts
        match staticAssetPattern $ do
            route idRoute
            compile copyFileCompiler

        -- SASS hot-reloading
        scssDependency <- makePatternDependency cssTemplate
        rulesExtraDependencies [scssDependency] $ match cssTemplate $ do
            route $ setExtension "css"
            compile (fmap compressCss <$> sassCompiler)

        create ["css/syntax.css"] $ do
            route idRoute
            compile $ do
                makeItem $ styleToCss pandocStyle

        -- Static pages
        match staticPagePattern $ do
            route $ composeRoutes (setExtension "html") (gsubRoute "pages" $ const ".")
            compile $
                pandocCompiler'
                    >>= loadAndApplyTemplate defaultTemplate defaultContext
                    >>= relativizeUrls

        -- Blog pages
        match blogPattern $ do
            route $ setExtension "html"
            compile $
                pandocCompiler'
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
        match projectsPattern $ do
            route $ setExtension "html"
            compile $
                pandocCompiler'
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
        match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y"
        `mappend` defaultContext

--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    let postPattern = "posts/**.md" .||. "posts/**.md.draft"

    let defaultTemplate = "templates/default.html"
    let archiveTemplate = "templates/archive.html"
    let postTemplate = "templates/post.html"
    let imageTemplate = "images/*" .||. "posts/**.png" .||. "posts/**.gif" .||. "posts/**.mkv"
    let cssTemplate = "css/*"

    match imageTemplate $ do
        route idRoute
        compile copyFileCompiler

    match cssTemplate $ do
        route idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route $ setExtension "html"
        compile $
            pandocCompiler
                >>= loadAndApplyTemplate defaultTemplate defaultContext
                >>= relativizeUrls

    match postPattern $ do
        route $ setExtension "html"
        compile $
            pandocCompiler
                >>= loadAndApplyTemplate postTemplate postCtx
                >>= loadAndApplyTemplate defaultTemplate postCtx
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- loadAll postPattern
            let archiveCtx =
                    listField "posts" postCtx (return posts)
                        `mappend` constField "title" "Archives"
                        `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate archiveTemplate archiveCtx
                >>= loadAndApplyTemplate defaultTemplate archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- loadAll postPattern
            let indexCtx =
                    listField "posts" postCtx (return posts)
                        `mappend` defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate defaultTemplate indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y"
        `mappend` defaultContext

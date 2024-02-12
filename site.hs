--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll
import Hakyll.Web.Sass (sassCompiler)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    let postPattern = "posts/**.md" .||. "posts/**.md.draft"

    let defaultTemplate = "templates/default.html"
    let projectsTemplate = "templates/projects.html"
    let postTemplate = "templates/post.html"
    let imageTemplate = "images/*" .||. "posts/**.png" .||. "posts/**.gif" .||. "posts/**.mkv"
    let cssTemplate = "css/*.scss"

    match imageTemplate $ do
        route idRoute
        compile copyFileCompiler

    scssDependency <- makePatternDependency cssTemplate
    rulesExtraDependencies [scssDependency] $ match cssTemplate $ do
        route $ setExtension "css"
        compile (fmap compressCss <$> sassCompiler)

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

    create ["projects.html"] $ do
        route idRoute
        compile $ do
            posts <- loadAll postPattern
            let projectsCtx =
                    listField "posts" postCtx (return posts)
                        `mappend` constField "title" "Projects"
                        `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate projectsTemplate projectsCtx
                >>= loadAndApplyTemplate defaultTemplate projectsCtx
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

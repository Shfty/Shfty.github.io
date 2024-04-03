{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.List.Utils
import Data.Maybe
import Hakyll
import Hakyll.Core
import Hakyll.Web
import Hakyll.Web.Glyphs
import Site
import qualified Site.Compiler as Compiler
import qualified Site.Config as Config
import qualified Site.Context as Context
import qualified Site.Identifier as Identifier
import qualified Site.Pattern as Pattern
import qualified Site.Pattern.Directory as Directory
import qualified Site.Pattern.Extension as Extension
import qualified Site.Routes as Routes
import qualified Site.Rules as Rules
import qualified Site.Template as Template
import System.FilePath
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.String
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

main :: IO ()
main = site $ do
    -- Load git branch into a context
    branchContext <- Context.branchField "branch"

    -- Load site config
    config <- Config.load $ fromFilePath "config"

    -- Get debug mode flag
    branch <- Context.gitBranch
    let debugMode = branch /= "master"

    let siteModeContext =
            constField "modeColor" (if debugMode then "red" else "purple")

    -- Assemble global site context
    let siteContext = branchContext <> siteModeContext <> Context.children <> Context.site

    -- Load code highlighting style
    let highlightStyle = Config.highlightStyle config
    pandocStyle <- loadPandocStyle highlightStyle

    -- Generate a CSS file for the loaded pandoc style
    Rules.styleCSS pandocStyle

    -- Setup SASS handling
    Rules.sass

    -- Compile templates
    Rules.template

    -- Compile static assets
    Rules.staticAsset

    -- Compile wallpapers
    when (Config.compileWallpapers config) $
        wallpapers Pattern.wallpaperLandscape Pattern.wallpaperPortrait

    -- Compile footer
    Rules.footer

    --- Compiler utilities
    let pandocCompiler' = pandocCompilerWithStyle pandocStyle

    let siteDefaults = defaults pandocCompiler' siteContext
    let pageDefaults = siteDefaults Template.page
    let catDefaults = siteDefaults Template.category

    let providers =
            defaultProviders
                & withCompiler "None" getResourceBody
                & withCompiler "Default" pandocCompiler'
                & withCompiler "pandoc" pandocCompiler'
                & withContext "None" siteContext

    let siteSpec = spec providers
    let pageSpec = siteSpec pageDefaults
    let catSpec = siteSpec catDefaults

    let [imageCtx, videoCtx, pageCtx, categoryCtx] =
            (<> siteContext) . uncurry iconCtx
                <$> [ ("image", "white")
                    , ("video", "white")
                    , ("file", "white")
                    , ("custom-folder", "purple")
                    ]

    -- Compile image and video viewers
    Rules.pageImage $ Rules.viewer Template.image imageCtx siteContext
    Rules.pageVideo $ Rules.viewer Template.video videoCtx siteContext

    -- Compile pages
    Rules.page $ do
        Rules.layoutSingle pageCtx siteContext
        Rules.final pageSpec siteContext

    -- Compile categories
    Rules.category $ do
        Rules.layoutList categoryCtx siteContext
        Rules.final catSpec siteContext

    let makeId =
            fromFilePath
                . (<.> "md")
                . ("tags" </>)
                . replace "+" "-plus"
                . replace "#" "-sharp"
                . replace " " "-"
                . fmap toLower

    -- Build tags
    tags <- buildTagsWith getTags' Pattern.pages makeId

    tagsRules
        tags
        ( \tag pats -> do
            create [makeId tag] $ do
                Rules.header siteContext

                route $ setExtension "html"
                compile $ do
                    matches <- getMatches pats

                    items <-
                        mapM
                            ( \a -> do
                                Just route <- getRoute a
                                slug <- loadSlug a
                                return $ "<a href = \"/" ++ extensionlessUrl route ++ "\">" ++ itemBody slug ++ "</a>"
                            )
                            matches

                    makeItem
                        ( ("<h1>" ++ tag ++ "</h1>")
                            <> mconcat items
                        )
                        >>= Compiler.final siteContext
        )

    create ["tags/index.md"] $ do
        Rules.slug siteContext
        Rules.header siteContext

        route $ setExtension "html"
        compile $ do
            renderTagList' tags
                >>= makeItem
                >>= Compiler.final siteContext

renderTagList' :: Tags -> Compiler String
renderTagList' = renderTags makeLink mconcat
  where
    makeLink tag url count _ _ =
        renderHtml $
            H.li
                ( glyph
                    <> ( H.a ! A.href (toValue url) ! A.rel "tag" $
                            toHtml (tag ++ " (" ++ show count ++ ")")
                       )
                )

getTags' :: (MonadMetadata m) => Identifier -> m [String]
getTags' identifier = do
    tags <- getTags identifier
    cat <- getCategory' identifier
    return $ cat <> tags

getCategory' :: (MonadMetadata m) => Identifier -> m [String]
getCategory' identifier = do
    let path = takeDirectory (toFilePath identifier) </> "index.md"
    title <- getMetadataField (fromFilePath path) "title"
    return $ if path == "pages/index.md" then [] else maybeToList title

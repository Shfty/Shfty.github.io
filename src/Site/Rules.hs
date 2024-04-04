{-# LANGUAGE OverloadedStrings #-}

module Site.Rules where

import Hakyll (Compiler, Context, Identifier, Pattern, Rules, Tags, compile, copyFileCompiler, create, getMatches, getResourceBody, getRoute, hasNoVersion, idRoute, itemBody, makeItem, match, route, setExtension, tagsRules, templateBodyCompiler, (.&&.))
import Hakyll.Core (emptyCompiler, matchIdent, overridableCompiler, parentRoute)
import Hakyll.Web (compileListSection, extensionlessUrl, hotReloadSASS, loadSlug, rulesHeader, rulesMenuSection, rulesMenuSectionWith, rulesSlug, rulesStyleCSS, rulesViewerWith)
import Hakyll.Web.Breadcrumb (breadcrumbContext)
import qualified Site.Compiler as Compiler
import qualified Site.Identifier as Identifier
import Site.Pattern as Pattern (
    categories,
    mainScss,
    pageImages,
    pageVideos,
    pages,
    scss,
    staticAsset,
    template,
 )
import Site.Pattern.Directory as Directory ()
import Site.Pattern.Extension as Extension ()
import Site.Routes (base)
import Site.Routes as Routes (base)
import qualified Site.Tags as Tags
import qualified Site.Template as Template
import Text.Pandoc.Highlighting (Style)

copyFile :: Rules ()
copyFile = do
    route idRoute
    compile copyFileCompiler

final getSpec ctx = do
    route base
    compile $
        overridableCompiler getSpec
            >>= Compiler.final ctx

template :: Rules ()
template = match Pattern.template $ compile templateBodyCompiler

staticAsset :: Rules ()
staticAsset = match Pattern.staticAsset copyFile

pageImage :: Rules () -> Rules ()
pageImage = match Pattern.pageImages

pageVideo :: Rules () -> Rules ()
pageVideo = match Pattern.pageVideos

page :: Rules () -> Rules ()
page = match Pattern.pages

category :: Rules () -> Rules ()
category = match Pattern.categories

slug :: (Identifier -> Compiler (Context String)) -> Rules ()
slug = rulesSlug Template.slug

header :: (Identifier -> Compiler (Context String)) -> Rules ()
header getCtx = rulesHeader Template.header $ fmap (\ctx -> (breadcrumbContext ctx <> ctx)) . getCtx

viewerPage :: Identifier -> Context String -> Rules ()
viewerPage template ctx =
    rulesViewerWith parentRoute Routes.base $
        emptyCompiler template ctx
            >>= Compiler.final ctx

viewer :: Identifier -> Context String -> Context String -> Rules ()
viewer template baseCtx menuCtx = do
    slug $ const $ return baseCtx
    header $ const $ return baseCtx
    layoutSingle menuCtx
    viewerPage template baseCtx

layoutSingle :: Context String -> Rules ()
layoutSingle ctx = do
    rulesMenuSection ctx

layoutList :: Context String -> Rules ()
layoutList ctx = do
    rulesMenuSectionWith $ compileListSection ctx

footer :: Rules ()
footer =
    matchIdent Identifier.footer $
        compile getResourceBody

styleCSS :: Style -> Rules ()
styleCSS = rulesStyleCSS Identifier.syntaxCss

sass :: Rules ()
sass = hotReloadSASS Pattern.mainScss Pattern.scss

tagIndex :: Tags -> Context String -> Rules ()
tagIndex tags ctx =
    create ["tags/index.md"] $ do
        slug $ const $ return ctx
        header $ const $ return ctx

        route $ setExtension "html"
        compile $ do
            Tags.renderTagList tags
                >>= makeItem
                >>= Compiler.final ctx

tagPages :: (String -> Identifier) -> Tags -> Context String -> Rules ()
tagPages makeId tags ctx =
    tagsRules
        tags
        ( \tag pats -> do
            create [makeId tag] $ do
                header $ const $ return ctx

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
                        >>= Compiler.final ctx
        )

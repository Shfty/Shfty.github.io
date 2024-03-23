module Site.Rules where

import Hakyll (Context, Identifier, Pattern, Rules, compile, copyFileCompiler, getResourceBody, hasNoVersion, idRoute, match, route, templateBodyCompiler, (.&&.))
import Hakyll.Core (emptyCompiler, matchIdent, overridableCompiler, parentRoute)
import Hakyll.Web (compileListSection, hotReloadSASS, rulesHeader, rulesMenuSection, rulesMenuSectionWith, rulesSlug, rulesStyleCSS, rulesViewerWith)
import Hakyll.Web.Breadcrumb (breadcrumbContext)
import qualified Site.Compiler as Compiler
import qualified Site.Identifier as Identifier
import Site.Pattern as Pattern
import Site.Pattern.Directory as Directory
import Site.Pattern.Extension as Extension
import Site.Routes (base)
import Site.Routes as Routes
import qualified Site.Template as Template
import Text.Pandoc.Highlighting (Style)

copyFile :: Rules ()
copyFile = do
    route idRoute
    compile copyFileCompiler

final spec ctx = do
    route base
    compile $
        overridableCompiler spec
            >>= Compiler.final ctx

template :: Rules ()
template = match Pattern.template $ compile templateBodyCompiler

staticAsset :: Rules ()
staticAsset = match Pattern.staticAsset copyFile

pageImage :: Rules () -> Rules ()
pageImage = match (Directory.pages .&&. Extension.image .&&. hasNoVersion)

pageVideo :: Rules () -> Rules ()
pageVideo = match (Directory.pages .&&. Extension.video .&&. hasNoVersion)

page :: Rules () -> Rules ()
page = match Pattern.pages

category :: Rules () -> Rules ()
category = match Pattern.categories

slug :: Context String -> Rules ()
slug = rulesSlug Template.slug

header :: Context String -> Rules ()
header ctx = rulesHeader Template.header (breadcrumbContext ctx <> ctx)

viewerPage :: Identifier -> Context String -> Rules ()
viewerPage template ctx =
    rulesViewerWith parentRoute Routes.base $
        emptyCompiler template ctx
            >>= Compiler.final ctx

viewer :: Identifier -> Context String -> Context String -> Rules ()
viewer template baseCtx menuCtx = do
    layoutSingle baseCtx menuCtx
    viewerPage template baseCtx

layoutSingle :: Context String -> Context String -> Rules ()
layoutSingle baseCtx menuCtx = do
    slug baseCtx
    rulesMenuSection menuCtx
    header baseCtx

layoutList :: Context String -> Context String -> Rules ()
layoutList baseCtx menuCtx = do
    slug baseCtx
    rulesMenuSectionWith $ compileListSection menuCtx
    header baseCtx

footer :: Rules ()
footer =
    matchIdent Identifier.footer $
        compile getResourceBody

styleCSS :: Style -> Rules ()
styleCSS = rulesStyleCSS Identifier.syntaxCss

sass :: Rules ()
sass = hotReloadSASS Pattern.mainScss Pattern.scss

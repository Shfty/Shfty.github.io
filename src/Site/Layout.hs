module Site.Layout where

import Control.Monad ((>=>))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Hakyll (Compiler, Context, Item, constField, getRoute, itemBody, itemIdentifier, loadAndApplyTemplate, loadBody, makeItem)
import Hakyll.Core
import Hakyll.Web
import Site.Identifier as Identifier (footer)
import Site.Template as Template

applyFlexColumn = applyTernaryTemplate Template.flexColumn
applyFlexRow = applyTernaryTemplate Template.flexRow

applyLayoutTemplate :: Context String -> Item String -> Compiler (Item String)
applyLayoutTemplate ctx item = do
    let ident = itemIdentifier item

    menuBody <- loadMenuSection $ fromFilePath "pages/index.md"

    route' <-
        getRoute (setVersion Nothing ident) <&> collapseMenu . fromMaybe (fail $ "No route for " ++ show ident)
    let menuBody' = withTags route' menuBody

    menu <- makeItem ("<nav>" ++ menuBody' ++ "</nav>")

    menuHeader <-
        makeEmptyItem
            >>= loadAndApplyTemplate Template.sidebarHeader ctx
            <&> itemBody

    footer <- loadBody Identifier.footer

    let panel header footer scrollCtx columnCtx =
            loadAndApplyTemplate Template.flexScroll scrollCtx
                >=> applyFlexColumn header footer columnCtx

    bodyHeader <- loadHeader ident
    body' <- panel (Just bodyHeader) Nothing ctx (constField "class" "body" <> ctx) item
    menu' <- panel (Just menuHeader) Nothing ctx (constField "class" "menu divider-right" <> ctx) menu

    makeItem (itemBody body')
        >>= applyFlexRow (Just (itemBody menu')) Nothing ctx
        >>= applyFlexColumn Nothing (Just footer) ctx
        >>= loadAndApplyTemplate Template.document ctx

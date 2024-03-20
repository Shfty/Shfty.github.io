{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Web.Menu where

import Prelude hiding (lookup)

import Data.Binary (Binary)
import Data.Data (Typeable)
import Data.List (isPrefixOf)
import Data.Map (delete, fromList, insert, lookup, toList)
import Hakyll (Compiler, Context, Identifier, Item, Rules, compile, getUnderlying, loadAndApplyTemplate, loadBody, makeItem, setVersion, version)
import Hakyll.Web.Slug (loadSlug)
import Text.HTML.TagSoup (Tag (TagOpen))

versionMenuSection :: String
versionMenuSection = "menuSection"

menuSection :: (Typeable a, Binary a) => Context a -> Compiler (Item String)
menuSection ctx = do
    ident <- getUnderlying
    item <- loadSlug ident
    makeItem item
        >>= loadAndApplyTemplate "templates/link.html" ctx

makeMenuSectionWith :: Compiler (Item String) -> Rules ()
makeMenuSectionWith = version versionMenuSection . compile

makeMenuSection :: (Typeable a, Binary a) => Context a -> Rules ()
makeMenuSection = makeMenuSectionWith . menuSection

loadMenuSection :: (Binary a, Typeable a) => Identifier -> Compiler a
loadMenuSection = loadBody . setVersion (Just versionMenuSection)

collapseMenu :: String -> Tag String -> Tag String
collapseMenu url a = case a of
    TagOpen a as -> do
        let as' = fromList as
        case lookup "data-url" as' of
            Just dataUrl -> do
                let withAttr k v =
                        if dataUrl == "/./" || dataUrl `isPrefixOf` ("/" ++ url)
                            then insert k v
                            else id
                TagOpen a $
                    toList $
                        delete "data-url" $
                            ( case a of
                                "details" -> withAttr "open" ""
                                "summary" -> withAttr "id" "active"
                                "a" -> withAttr "id" "active"
                                _otherwise -> id
                            )
                                as'
            _otherwise -> TagOpen a as
    _otherwise -> a

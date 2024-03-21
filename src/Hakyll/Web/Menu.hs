{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Web.Menu where

import Prelude hiding (lookup)

import Control.Monad (liftM)
import Data.Binary (Binary)
import Data.Data (Typeable)
import Data.List (isPrefixOf)
import Data.Map (delete, fromList, insert, lookup, toList)
import Hakyll (Compiler, Context, Identifier, Item, Rules, compile, getUnderlying, itemBody, loadAndApplyTemplate, loadBody, makeItem, setVersion, version)
import Hakyll.Web.Slug (loadSlug)
import Text.HTML.TagSoup (Tag (TagOpen))

menuSection :: String
menuSection = "menuSection"

compileMenuSection :: Context String -> Compiler (Item String)
compileMenuSection ctx = do
    getUnderlying
        >>= loadSlug
        >>= (makeItem . itemBody)
        >>= loadAndApplyTemplate "templates/link.html" ctx

makeMenuSectionWith :: Compiler (Item String) -> Rules ()
makeMenuSectionWith = version menuSection . compile

makeMenuSection :: Context String -> Rules ()
makeMenuSection = makeMenuSectionWith . compileMenuSection

loadMenuSection :: (Binary a, Typeable a) => Identifier -> Compiler a
loadMenuSection = loadBody . setVersion (Just menuSection)

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

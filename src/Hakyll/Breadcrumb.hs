{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Breadcrumb where

import Data.List (null, intersperse, dropWhileEnd, inits)
import Data.Maybe

import Hakyll
import Hakyll.Tags
import Hakyll.Glyphs

import Text.Blaze.Internal (customLeaf, customAttribute)

import System.FilePath

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

inits' = filter (not . null) . inits

isSlash a = a == '/'

stripSlash = dropWhileEnd isSlash

getBreadcrumbWith :: (MonadMetadata m) => ([FilePath] -> [String]) -> Identifier -> m [String]
getBreadcrumbWith f ident = return $ stripSlash . mconcat <$> inits' (f $ (splitPath . takeDirectory . toFilePath) ident)

getBreadcrumb :: (MonadMetadata m) => Identifier -> m [String]
getBreadcrumb = getBreadcrumbWith id

breadcrumbFieldWith getTags' = tagsFieldWith' getTags' renderBreadcrumb concatBreadcrumb
breadcrumbField = breadcrumbFieldWith getBreadcrumb

concatBreadcrumb = do
    let sep = preSpace <> leftSoftDivider <> preSpace
    mconcat . intersperse sep

renderBreadcrumb :: (MonadFail m, MonadMetadata m) => Identifier -> Maybe FilePath -> m (Maybe H.Html)
renderBreadcrumb id path = do
    title <- getMetadataField' id "title"
    return
        $ Just
        $ H.a
            H.! A.title (H.stringValue ("All pages tagged '" ++ title ++ "'."))
            H.! A.href (H.toValue $ toUrl $ fromMaybe "Nothing" path)
            H.! A.rel "tag"
        $ H.toHtml title

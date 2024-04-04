{-# LANGUAGE OverloadedStrings #-}

module Site.Tags where

import Hakyll (Tags, Compiler, Identifier, MonadMetadata, renderTags, getTags, toFilePath, getMetadataField, fromFilePath)
import Hakyll.Web.Glyphs ( glyph )
import Text.Blaze.Html ( toHtml, ToValue(toValue), (!) )
import Text.Blaze.Html.Renderer.String ( renderHtml )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import System.FilePath ( (</>), takeDirectory )
import Data.Maybe (maybeToList)

renderTagList :: Tags -> Compiler String
renderTagList = renderTags makeLink mconcat
  where
    makeLink tag url count _ _ =
        renderHtml $
            H.li
                ( glyph
                    <> ( H.a ! A.href (toValue url) ! A.rel "tag" $
                            toHtml (tag ++ " (" ++ show count ++ ")")
                       )
                )

getTags :: (MonadMetadata m) => Identifier -> m [String]
getTags identifier = do
    tags <- Hakyll.getTags identifier
    cat <- getCategory identifier
    return $ cat <> tags

getCategory :: (MonadMetadata m) => Identifier -> m [String]
getCategory identifier = do
    let path = takeDirectory (toFilePath identifier) </> "index.md"
    title <- getMetadataField (fromFilePath path) "title"
    return $ if path == "pages/index.md" then [] else maybeToList title

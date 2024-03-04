module Hakyll.Tags where

import Control.Monad
import Data.List
import Data.Maybe

import Hakyll

import Text.Blaze.Html.Renderer.String

-- Modified version of tagsRules
-- Uses match instead of create to allow tag pages
-- to have static content
tagsRules' tags rules =
    forM_ (tagsMap tags) $ \(tag, identifiers) ->
        rulesExtraDependencies [tagsDependency tags] $
            match (fromList [tagsMakeId tags tag]) $
                rules tag $
                    fromList identifiers


-- Modified version of tagsFieldWith
-- Takes (Identifier -> Maybe FilePath -> Compiler (Maybe a)) in its renderLink function,
-- thus allowing the rendering code to lookup tag metadata, such as its title
tagsFieldWith' getTags' renderLink cat key tags = field key $ \item -> do
    tags' <- getTags' $ itemIdentifier item
    links <- forM tags' $ \tag -> do
        let id = tagsMakeId tags tag
        route' <- getRoute id
        renderLink id route'

    return $ renderHtml $ cat $ catMaybes links

-- Printer for Tags
showTagEntry :: (String, [Identifier]) -> String
showTagEntry (k, v) = k ++ ":\n\t" ++ mconcat ((++ "\n\t") . toFilePath <$> v)

showTagsMap :: [(String, [Identifier])] -> String
showTagsMap = mconcat . intersperse "\n" . fmap showTagEntry

showTags = showTagsMap . tagsMap

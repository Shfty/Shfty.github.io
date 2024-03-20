module Hakyll.Core.Item.Sort where

import Data.List (isInfixOf, sortBy)
import Data.Maybe (fromMaybe)
import Hakyll (Compiler, Item, getMetadata, getRoute, itemIdentifier)
import Hakyll.Core.Metadata (lookupString)

sortPosts :: [Item String] -> Compiler [Item String]
sortPosts posts = do
    postMeta <-
        mapM
            ( \a -> do
                let ident = itemIdentifier a
                meta <- getMetadata ident
                route <- getRoute ident
                return (a, meta, fromMaybe (fail "No route for " ++ show ident) route)
            )
            posts
    return $
        ( fmap (\(a, b, c) -> a)
            . sortBy
                ( \(a, am, ar) (b, bm, br) -> compare ("/index." `isInfixOf` br) ("/index." `isInfixOf` ar)
                )
            . sortBy
                ( \(a, am, ar) (b, bm, br) -> do
                    let icon = lookupString "icon"
                    compare (icon am) (icon bm)
                )
            . sortBy
                ( \(a, am, ar) (b, bm, br) -> compare (itemIdentifier a) (itemIdentifier b)
                )
        )
            postMeta

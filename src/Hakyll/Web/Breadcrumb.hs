module Hakyll.Web.Breadcrumb where

import Data.List (isInfixOf)
import Hakyll (Compiler, Context, Item, itemIdentifier, listFieldWith, load, makeItem, setVersion, toFilePath)
import Hakyll.Core.Identifier (fromFilePath)
import Hakyll.Web.Slug (loadSlug)
import System.FilePath (
    splitDirectories,
    takeDirectory,
    (<.>),
    (</>),
 )

breadcrumbContext :: Context String -> Context String
breadcrumbContext ctx =
    listFieldWith
        "breadcrumb"
        ctx
        ( \item -> do
            let ident = itemIdentifier item
            let path = toFilePath ident
            let path' =
                    if "/index." `isInfixOf` path
                        then takeDirectory $ takeDirectory path
                        else takeDirectory path

            let dirs = splitDirectories path'
            if dirs == ["."]
                then return []
                else do
                    let (path, breadcrumb) = foldr (\a (path, acc) -> (a, acc <> [last acc </> a])) (path, [""]) $ reverse dirs
                    let breadcrumb' =
                            tail
                                ( fromFilePath . (</> "index" <.> "md")
                                    <$> breadcrumb
                                )
                    mapM loadSlug breadcrumb'
        )

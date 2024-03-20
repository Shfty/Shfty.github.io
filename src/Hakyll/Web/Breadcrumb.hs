module Hakyll.Web.Breadcrumb where

import Data.List (isInfixOf)
import Hakyll (Compiler, Item, itemIdentifier, listFieldWith, load, setVersion, toFilePath)
import Hakyll.Core.Identifier (fromFilePath)
import System.FilePath (
    splitDirectories,
    takeDirectory,
    (<.>),
    (</>),
 )

breadcrumbCtx versionSlug ctx =
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
                                ( (setVersion (Just versionSlug) . fromFilePath)
                                    . (</> "index" <.> "md")
                                    <$> breadcrumb
                                )
                    mapM load breadcrumb' :: Compiler [Item String]
        )

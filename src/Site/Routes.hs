module Site.Routes where

import Data.List (tail)
import Hakyll (composeRoutes, mapContextBy, setExtension)
import Hakyll.Core.Routes.Parent
import System.FilePath (joinPath, splitDirectories)

base = setExtension "html" `composeRoutes` parentRoute

liftPath =
    mapContextBy (== "path") (\a -> "/" ++ joinPath (tail $ splitDirectories a))

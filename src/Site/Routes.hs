module Site.Routes where

import Data.List (tail)
import Hakyll (Context, Routes, composeRoutes, mapContextBy, setExtension)
import Hakyll.Core.Routes.Parent
import System.FilePath (joinPath, splitDirectories)

base :: Routes
base = setExtension "html" `composeRoutes` parentRoute

liftPath :: Context a -> Context a
liftPath =
    mapContextBy
        (== "path")
        (("/" ++) . joinPath . (\(a : as) -> as) . splitDirectories)

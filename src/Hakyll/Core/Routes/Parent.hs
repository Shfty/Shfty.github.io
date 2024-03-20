module Hakyll.Core.Routes.Parent where

import Hakyll (customRoute, toFilePath)
import System.FilePath (replaceDirectory, joinPath, splitPath, takeDirectory)

-- Hoist the provided route one directory upward
parentRoute =
    customRoute $
        replaceDirectory
            <*> joinPath . drop 1 . splitPath . takeDirectory
            <$> toFilePath


module Hakyll.Web.FileType where

import Data.Map (Map, fromList)
import Data.Map.Set (lookupSetKey)
import Hakyll (field, itemIdentifier, toFilePath)
import Hakyll.Web.Template.Context (Context)
import System.FilePath (takeExtension)

type FileTypes = Map String [String]

-- Map from filetype name to file extensions
fileTypes :: FileTypes
fileTypes =
    fromList
        [ ("markdown", [".md"])
        , ("html", [".html"])
        , ("image", [".png", ".jpg", ".jpeg", ".gif"])
        , ("video", [".mkv", ".mp4", ".webm"])
        ]

-- Filetype context field
fileTypeField :: FileTypes -> Context a
fileTypeField fts =
    field
        "fileType"
        ( \item -> do
            let ident = itemIdentifier item
            let path = toFilePath ident
            case lookupSetKey (takeExtension path) fts of
                Just t -> return t
                Nothing -> fail "Unknown extension"
        )

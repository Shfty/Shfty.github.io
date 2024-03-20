{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Web.Extensionless where

import Data.List (isInfixOf, isPrefixOf)
import Data.Text (unpack)
import Data.Text.Conversions (fromText)
import Hakyll (Configuration, Context, field, getRoute, previewSettings, setVersion, toUrl, itemIdentifier)
import System.Directory (doesFileExist)
import System.FilePath (dropExtension, dropFileName, takeExtension, takeFileName, (<.>), (</>))
import WaiAppStatic.Types (fromPiece, ssGetMimeType, ssLookupFile, unsafeToPiece, fileName)

-- Custom Hakyll configuration
-- Emulates the server config of GitHub Pages
extensionlessUrls :: Configuration -> Configuration
extensionlessUrls config =
    config
        { previewSettings = \path ->
            let settings = config.previewSettings path
             in settings
                    { ssLookupFile = \pieces ->
                        case splitAt (length pieces - 1) pieces of
                            (prefix, [piece]) -> do
                                let fileName = fromPiece piece
                                if takeExtension (fromText fileName) == ""
                                    then settings.ssLookupFile $ prefix <> [unsafeToPiece $ fileName <> ".html"]
                                    else settings.ssLookupFile pieces
                            _otherwise -> settings.ssLookupFile pieces
                    , ssGetMimeType = \file ->
                        if takeExtension (unpack (fromPiece file.fileName)) == ""
                            then do
                                htmlExists <- doesFileExist $ path </> unpack (fromPiece file.fileName) <.> "html"
                                if htmlExists
                                    then pure "text/html"
                                    else settings.ssGetMimeType file
                            else settings.ssGetMimeType file
                    }
        }

extensionlessUrl :: String -> String
extensionlessUrl url = do
    let isLocal = "/" `isPrefixOf` url || not ("://" `isInfixOf` url)
    if isLocal
        then
            if takeFileName url == "index.html"
                then dropFileName url
                else
                    if takeExtension url == ".html"
                        then dropExtension url
                        else url
        else url

extensionlessUrlField :: String -> Context a
extensionlessUrlField key =
    field key $ \item ->
        maybe
            (fail $ "no route url found for item " ++ show item.itemIdentifier)
            (toUrl . extensionlessUrl)
            <$> getRoute (setVersion Nothing item.itemIdentifier)

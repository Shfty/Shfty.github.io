module Hakyll.Highlighting where

import Control.Exception

import GHC.IO.Exception

import Hakyll

import Text.Pandoc.Options
import Text.Pandoc.Highlighting

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as ByteString

-- Create a pandoc compiler with the provided highlight style
pandocCompilerWithStyle style =
    pandocCompilerWith
        defaultHakyllReaderOptions
        defaultHakyllWriterOptions
            { writerHighlightStyle = Just style
            }

-------------------------------------------------------------------------------
-- Custom highlight style loading

printException (Left e) = do
    let filename = case ioe_filename e of
            Just a -> "Error reading from " ++ a
            Nothing -> "Error reading file"
    putStrLn $ filename ++ ": " ++ ioe_description e
    return Nothing
printException (Right t) = return $ Just t

tryReadFile :: FilePath -> IO (Maybe ByteString.ByteString)
tryReadFile f = try (ByteString.readFile f) >>= printException

maybeDecode (Left l) = Nothing <$ print l
maybeDecode (Right r) = return $ Just r

tryParse = maybeDecode <$> JSON.eitherDecode

maybeM d = maybe (return d)

tryParseStyle :: Style -> ByteString.ByteString -> IO Style
tryParseStyle d a = tryParse a >>= maybeM d return

tryLoadStyle :: Style -> FilePath -> Rules Style
tryLoadStyle d a = preprocess $ tryReadFile a >>= maybeM d (tryParseStyle d)

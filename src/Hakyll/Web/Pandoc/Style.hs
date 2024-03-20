module Hakyll.Web.Pandoc.Style where

import Hakyll (Identifier, Rules, compile, create, defaultHakyllReaderOptions, defaultHakyllWriterOptions, idRoute, makeItem, pandocCompilerWith, route)
import Hakyll.Core.Compiler (Compiler)
import Hakyll.Core.Item (Item)
import Hakyll.Core.Rules (preprocess)
import Text.Pandoc (writerHighlightStyle)
import Text.Pandoc.Class (runIOorExplode)
import Text.Pandoc.Highlighting (Style, lookupHighlightingStyle, styleToCss)

-- Create a pandoc compiler with the provided highlight style
pandocCompilerWithStyle :: Style -> Compiler (Item String)
pandocCompilerWithStyle style =
    pandocCompilerWith
        defaultHakyllReaderOptions
        defaultHakyllWriterOptions
            { writerHighlightStyle = Just style
            }

-- Load a pandoc style within the Rules monad
loadPandocStyle :: String -> Rules Style
loadPandocStyle = preprocess . runIOorExplode . lookupHighlightingStyle

makeStyleCSS :: Identifier -> Style -> Rules ()
makeStyleCSS path style = create [path] $ do
    route idRoute
    compile $ makeItem $ styleToCss style

module Hakyll.Web.Header where

import Data.Binary (Binary)
import Data.Data (Typeable)
import Hakyll (Compiler, Context, Identifier, Rules, compile, loadAndApplyTemplate, loadBody, setVersion, version)
import Hakyll.Core.Item.Empty (makeEmptyItem)

versionHeader :: String
versionHeader = "header"

makeHeader :: Identifier -> Context String -> Rules ()
makeHeader headerTemplate ctx = version versionHeader $ compile $ do
    makeEmptyItem
        >>= loadAndApplyTemplate headerTemplate ctx

loadHeader :: (Binary a, Typeable a) => Identifier -> Compiler a
loadHeader ident = loadBody $ setVersion (Just versionHeader) ident

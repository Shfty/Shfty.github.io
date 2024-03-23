module Hakyll.Web.Header where

import Data.Binary (Binary)
import Data.Data (Typeable)
import Hakyll (Compiler, Context, Identifier, Rules, compile, loadAndApplyTemplate, loadBody, setVersion, version)
import Hakyll.Core.Item.Empty (makeEmptyItem)

header :: String
header = "header"

rulesHeader :: Identifier -> Context String -> Rules ()
rulesHeader headerTemplate ctx = version header $ compile $ do
    makeEmptyItem
        >>= loadAndApplyTemplate headerTemplate ctx

loadHeader :: (Binary a, Typeable a) => Identifier -> Compiler a
loadHeader ident = loadBody $ setVersion (Just header) ident

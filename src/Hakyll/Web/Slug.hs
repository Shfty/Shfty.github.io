module Hakyll.Web.Slug where

import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Hakyll (Compiler, Context, Identifier, Item, Rules, compile, load, loadAndApplyTemplate, setVersion, version)
import Hakyll.Core.Item.Empty (makeEmptyItem)

slug :: String
slug = "slug"

slugCompiler :: Identifier -> Context String -> Compiler (Item String)
slugCompiler slugTemplate ctx =
    makeEmptyItem >>= loadAndApplyTemplate slugTemplate ctx

rulesSlug :: Identifier -> Context String -> Rules ()
rulesSlug slugTemplate = version slug . compile . slugCompiler slugTemplate

loadSlug :: Identifier -> Compiler (Item String)
loadSlug = load . setVersion (Just slug)

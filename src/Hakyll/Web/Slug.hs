module Hakyll.Web.Slug where

import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Hakyll (Compiler, Context, Identifier, Item, Rules, compile, loadAndApplyTemplate, loadBody, setVersion, version)
import Hakyll.Core.Item.Empty (makeEmptyItem)

versionSlug :: String
versionSlug = "slug"

slugCompiler :: Identifier -> Context String -> Compiler (Item String)
slugCompiler slugTemplate ctx =
    makeEmptyItem >>= loadAndApplyTemplate slugTemplate ctx

makeSlug :: Identifier -> Context String -> Rules ()
makeSlug slugTemplate = version versionSlug . compile . slugCompiler slugTemplate

loadSlug :: (Binary a, Typeable a) => Identifier -> Compiler a
loadSlug = loadBody . setVersion (Just versionSlug)

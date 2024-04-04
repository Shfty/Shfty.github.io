module Hakyll.Web.Slug where

import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Hakyll (Compiler, Context, Identifier, Item, Rules, compile, getUnderlying, load, loadAndApplyTemplate, setVersion, version)
import Hakyll.Core.Item.Empty (makeEmptyItem)

slug :: String
slug = "slug"

slugCompiler :: Identifier -> (Identifier -> Compiler (Context String)) -> Compiler (Item String)
slugCompiler slugTemplate getCtx = do
    ident <- getUnderlying
    ctx <- getCtx (setVersion Nothing ident)
    makeEmptyItem >>= loadAndApplyTemplate slugTemplate ctx

rulesSlug :: Identifier -> (Identifier -> Compiler (Context String)) -> Rules ()
rulesSlug slugTemplate = version slug . compile . slugCompiler slugTemplate

loadSlug :: Identifier -> Compiler (Item String)
loadSlug = load . setVersion (Just slug)

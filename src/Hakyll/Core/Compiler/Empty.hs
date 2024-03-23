module Hakyll.Core.Compiler.Empty where

import Hakyll (Compiler, Context, Identifier, Item, loadAndApplyTemplate)
import Hakyll.Core.Item.Empty (makeEmptyItem)

-- Compile an empty item with the provided template and context,
-- useful in cases where body content is created via template interpolation
emptyCompiler :: Identifier -> Context String -> Compiler (Item String)
emptyCompiler template ctx = do
    makeEmptyItem
        >>= loadAndApplyTemplate template ctx

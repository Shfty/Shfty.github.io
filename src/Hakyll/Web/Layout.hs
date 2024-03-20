module Hakyll.Web.Layout where

import Data.Maybe (catMaybes)
import Hakyll (
    Compiler,
    Context,
    Identifier,
    Item,
    constField,
    loadAndApplyTemplate,
 )

applyTernaryTemplate :: Identifier -> Maybe String -> Maybe String -> Context a -> Item a -> Compiler (Item String)
applyTernaryTemplate template before after ctx = do
    let ctx' =
            mconcat $
                catMaybes
                    [ constField "before" <$> before
                    , constField "after" <$> after
                    ]
    let ctx'' = ctx' <> ctx

    loadAndApplyTemplate template ctx''

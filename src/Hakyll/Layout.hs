module Hakyll.Layout where

import Data.Maybe

import Hakyll

applyTernaryTemplate template before after ctx = do
    let ctx' =
            mconcat $
                catMaybes
                    [ constField "before" <$> before
                    , constField "after" <$> after
                    ]
    let ctx'' = ctx' <> ctx

    loadAndApplyTemplate template ctx''


module Site.Compiler where

import Control.Monad ((>=>))
import Data.Binary (Binary)
import Data.Data (Typeable)
import Hakyll (Compiler, Context, Item, Pattern, loadAll, relativizeUrls)
import Hakyll.Core (sortPosts)
import Hakyll.Web (hasMenuSection)
import Site.Layout (applyLayoutTemplate)
import Site.Pattern as Pattern

loadChildrenWith :: (Binary a, Typeable a) => (Pattern -> Pattern) -> Compiler [Item a]
loadChildrenWith f = Pattern.children >>= loadAll . f

loadChildren :: (Binary a, Typeable a) => Compiler [Item a]
loadChildren = loadChildrenWith id

loadAndSortChildren :: Compiler [Item String]
loadAndSortChildren =
    loadChildrenWith hasMenuSection
        >>= sortPosts

-- Given final page content, lift it into the main layout, and relativize its URLs
final :: Context String -> Item String -> Compiler (Item String)
final ctx =
    applyLayoutTemplate ctx
        >=> relativizeUrls

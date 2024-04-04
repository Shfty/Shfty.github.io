module Site.Compiler where

import Control.Monad ((>=>))
import Data.Binary (Binary)
import Data.Data (Typeable)
import Hakyll (Compiler, Context, Identifier, Item, Pattern, fromGlob, getMatches, hasNoVersion, loadAll, matches, relativizeUrls, toFilePath, (.&&.))
import Hakyll.Core (sortPosts)
import Hakyll.Web (hasMenuSection)
import Site.Layout (applyLayoutTemplate)
import Site.Pattern as Pattern
import System.FilePath (takeDirectory, (</>))

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

-- Given an identifier,
-- check whether it is a category, and return the
-- corresponding value within the Compiler monad
ifCategory :: a -> a -> Identifier -> Compiler a
ifCategory true false ident = do
    cond <- isCategory ident
    return (if cond then true else false)

-- Test whether the given identifier represents a category.
--
-- A category is an index.md with sibling .md files,
-- or sibling directories containing index.md files.
isCategory :: Identifier -> Compiler Bool
isCategory ident = do
    ( if matches Pattern.categories ident
            then
                ( do
                    let dirPat = takeDirectory (toFilePath ident)

                    let siblingPat = fromGlob $ dirPat </> "*.md"
                    siblings <- getMatches $ siblingPat .&&. Pattern.notIndex .&&. hasNoVersion

                    let childPat = fromGlob $ dirPat </> "*" </> "*.md"
                    children <- getMatches $ childPat .&&. hasNoVersion

                    return (not (null siblings && null children))
                )
            else return False
        )

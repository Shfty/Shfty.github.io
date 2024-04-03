module Site.Context where

import Data.Functor ((<&>))
import Data.String.Utils (strip)
import Hakyll (Context, Rules, constField, dateField, defaultContext, functionField, listField, preprocess)
import Hakyll.Core
import Hakyll.Web
import Site.Compiler as Compiler
import Site.Routes (liftPath)
import System.Process (readProcess)

site :: Context String
site =
    liftPath $
        extensionlessUrlField "url"
            <> fileTypeField fileTypes
            <> defaultContext

post :: Context String
post = dateField "date" "%B %e, %Y" `mappend` site

gitBranch :: Rules String
gitBranch = preprocess $ strip <$> readProcess "git" ["branch", "--show-current"] ""

-- Read in the site's current git branch as a field
branchField :: String -> Rules (Context a)
branchField f = gitBranch <&> constField f

children :: Context a
children = listField "children" post Compiler.loadAndSortChildren

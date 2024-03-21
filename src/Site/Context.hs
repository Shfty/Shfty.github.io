module Site.Context where

import Data.Functor ((<&>))
import Hakyll (Context, Rules, constField, dateField, defaultContext, preprocess)
import Hakyll.Core
import Hakyll.Web
import Site.Routes (liftPath)
import System.Process (readProcess)

site :: Context String
site = liftPath $ extensionlessUrlField "url" <> fileTypeField fileTypes <> defaultContext

post :: Context String
post = dateField "date" "%B %e, %Y" `mappend` site

-- Read in the site's current git branch as a field
branchField :: String -> Rules (Context a)
branchField f = do
    preprocess (readProcess "git" [f, "--show-current"] "" <&> constField f)

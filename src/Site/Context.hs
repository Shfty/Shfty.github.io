module Site.Context where

import Hakyll (Context, defaultContext, dateField)
import Hakyll.Web
import Hakyll.Core
import Site.Routes (liftPath)

site :: Context String
site = liftPath $ extensionlessUrlField "url" <> fileTypeField fileTypes <> defaultContext

post :: Context String
post = dateField "date" "%B %e, %Y" `mappend` site

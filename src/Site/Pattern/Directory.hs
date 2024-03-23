module Site.Pattern.Directory where

import Hakyll (fromGlob)
import System.FilePath ((</>))

dirPat dir = fromGlob $ dir </> "**"

images = dirPat "images"
fonts = dirPat "fonts"
pages = dirPat "pages"
templates = dirPat "templates"
css = dirPat "css"

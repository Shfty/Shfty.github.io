{-# LANGUAGE OverloadedStrings #-}

module Site.Template where

import Hakyll (Identifier, Pattern, Rules, compile, match, templateBodyCompiler)

-- HTML document
document = "templates/document.html" :: Identifier

-- Global footer
footer = "templates/footer.html" :: Identifier

-- Content header
header = "templates/content/header.html" :: Identifier
sidebarHeader = "templates/sidebar/header.html" :: Identifier

-- Image and video viewers
image = "templates/image.html" :: Identifier
video = "templates/video.html" :: Identifier

-- Slug and menu
slug = "templates/slug.html" :: Identifier
menuSection = "templates/menu-section.html" :: Identifier

-- Pages, categories
page = "templates/post.html" :: Identifier
category = "templates/category.html" :: Identifier

-- Layout
flexColumn = "templates/flex-column.html" :: Identifier
flexRow = "templates/flex-row.html" :: Identifier
flexScroll = "templates/flex-scroll.html" :: Identifier

makeTemplates :: Pattern -> Rules ()
makeTemplates pat = match pat $ compile templateBodyCompiler

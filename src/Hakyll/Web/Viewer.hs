module Hakyll.Web.Viewer where

import Data.Binary (Binary)
import Data.Data (Typeable)
import Hakyll (Compiler, Context, Identifier, Item, Routes, Rules, Writable, compile, copyFileCompiler, loadAndApplyTemplate, relativizeUrls, route, version)
import Hakyll.Core (emptyCompiler, makeEmptyItem)

viewerContent = "viewerContent"

rulesViewerWith :: (Typeable a, Binary a, Writable a) => Routes -> Routes -> Compiler (Item a) -> Rules ()
rulesViewerWith contentRoute viewerRoute compiler = do
    version viewerContent $ do
        route contentRoute
        compile copyFileCompiler

    route viewerRoute
    compile compiler

rulesViewer :: Identifier -> Routes -> Routes -> Context String -> Rules ()
rulesViewer template contentRoute viewerRoute ctx = do
    rulesViewerWith contentRoute viewerRoute $ emptyCompiler template ctx

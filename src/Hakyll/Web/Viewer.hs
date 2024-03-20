module Hakyll.Web.Viewer where

import Data.Binary (Binary)
import Data.Data (Typeable)
import Hakyll (Compiler, Context, Identifier, Item, Routes, Rules, Writable, compile, copyFileCompiler, loadAndApplyTemplate, relativizeUrls, route, version)
import Hakyll.Core.Item.Empty (makeEmptyItem)

compileViewer :: Identifier -> Context String -> Compiler (Item String)
compileViewer template ctx = do
    makeEmptyItem
        >>= loadAndApplyTemplate template ctx

makeViewerWith :: (Typeable a, Binary a, Writable a) => Routes -> Routes -> Compiler (Item a) -> Rules ()
makeViewerWith contentRoute viewerRoute compiler = do
    version "viewerContent" $ do
        route contentRoute
        compile copyFileCompiler

    route viewerRoute
    compile compiler

makeViewer :: Identifier -> Routes -> Routes -> Context String -> Rules ()
makeViewer template contentRoute viewerRoute ctx = do
    makeViewerWith contentRoute viewerRoute $ compileViewer template ctx

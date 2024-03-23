module Hakyll.Core.Config where

import Hakyll (providerDirectory)

withProviderDirectory dir config =
    config
        { providerDirectory = dir
        }


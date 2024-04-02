module Hakyll.Core.Config where

import Hakyll (destinationDirectory, providerDirectory)

withProviderDirectory dir config =
    config
        { providerDirectory = dir
        }

withDestinationDirectory dir config =
    config
        { destinationDirectory = dir
        }

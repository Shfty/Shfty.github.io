module Hakyll.Core.Metadata.Maybe where

import Data.Maybe (fromMaybe)
import Hakyll (itemIdentifier, getMetadataField, field)

-- Map a specific field to its metadata value, or a default if it is not present.
maybeMetadataField f d =
    field
        f
        ( \a -> do
            let id = itemIdentifier a
            v <- getMetadataField id f
            return $ fromMaybe d v
        )


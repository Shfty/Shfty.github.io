module Data.Map.Set where

import Data.Map

-- Given a value type and a map from a key type to lists of that value type,
-- find the key corresponding to the set containing the provided value
lookupSetKey :: (Eq v) => v -> Map k [v] -> Maybe k
lookupSetKey val =
    foldrWithKey
        ( \k next acc ->
            if val `elem` next then Just k else acc
        )
        Nothing


module Hakyll.Core.Identifier.Pattern.Singleton where

import Hakyll (Identifier, Pattern, fromList)

fromIdent :: Identifier -> Pattern
fromIdent a = fromList [a]

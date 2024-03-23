module Hakyll.Core.Rules.Singleton where

import Hakyll (match, Identifier, Rules, create)
import Hakyll.Core.Identifier.Pattern.Singleton (fromIdent)

matchIdent :: Identifier -> Rules () -> Rules ()
matchIdent ident = match $ fromIdent ident

createIdent :: Identifier -> Rules () -> Rules ()
createIdent ident = create [ident]

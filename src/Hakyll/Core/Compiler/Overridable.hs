{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Core.Compiler.Overridable where

import Control.Monad.Loops
import Data.List
import Data.Map
import Data.Maybe
import Data.String
import Data.Text (splitOn, strip)
import Data.Text.Conversions

import Data.Aeson
import qualified Data.Aeson.Key
import qualified Data.Aeson.KeyMap
import Data.Aeson.Types
import Hakyll

data CompilerDefaults a = CompilerDefaults
    { getCompiler :: !(Compiler (Item a))
    , getContext :: !(Context a)
    , getTemplate :: !Identifier
    }

mapCompiler f defaults = defaults{getCompiler = f $ getCompiler defaults}
mapContext f defaults = defaults{getContext = f $ getContext defaults}
mapTemplate f defaults = defaults{getContext = f $ getTemplate defaults}

data CompilerProviders a = CompilerProviders
    { getCompilers :: !(Map String (Compiler (Item a)))
    , getContexts :: !(Map String (Context a))
    }

defaultProviders = CompilerProviders empty empty

mapCompilers f providers = providers{getCompilers = f $ getCompilers providers}
mapContexts f providers = providers{getContexts = f $ getContexts providers}

withCompiler k c = mapCompilers (Data.Map.insert k c)
withContext k c = mapContexts (Data.Map.insert k c)

data CompilerSpec a = CompilerSpec
    { getProviders :: !(CompilerProviders a)
    , getDefaults :: !(CompilerDefaults a)
    }

mapDefaults f spec = spec{getDefaults = f $ getDefaults spec}

splitList a = fromText <$> (strip <$> splitOn "," (fromString a))

-- Compiler that can override compilers, templates, and context from page metadata
overridableCompiler :: CompilerSpec String -> Compiler (Item String)
overridableCompiler spec = do
    ident <- getUnderlying
    meta <- getMetadata ident

    let providers = getProviders spec
    let defaults = getDefaults spec

    let defaultCompiler = getCompiler defaults
    let defaultContext = getContext defaults
    let defaultTemplate = getTemplate defaults

    let metaCompilers = parseMaybe (.: "compilers") meta :: Maybe [String]
    let metaContexts = parseMaybe (.: "contexts") meta :: Maybe [Object]

    let compiler =
            maybe
                defaultCompiler
                ( \a -> do
                    let go a = case Data.Map.lookup a $ getCompilers providers of
                            Just a -> a
                            Nothing -> error $ "Invalid compiler " ++ a
                    let compilers = Data.List.map go a
                    foldl1' ((=<<) . return) compilers
                )
                metaCompilers

    let context =
            maybe
                defaultContext
                ( \a -> do
                    mconcat $
                        ( \b -> do
                            let [(k, Object v)] = Data.Aeson.KeyMap.toList b
                            let [(ty, String b)] = Data.Aeson.KeyMap.toList v

                            case ty of
                                "glob" -> do
                                    let pat = fromGlob $ fromText b

                                    listFieldWith
                                        (Data.Aeson.Key.toString k)
                                        defaultContext
                                        (const $ loadAll pat)
                                a -> error $ "Invalid context type " ++ show a
                        )
                            <$> a
                )
                metaContexts

    let defaultApply = loadAndApplyTemplate defaultTemplate context
    let metaTemplates = parseMaybe (.: "templates") meta :: Maybe [String]
    let applyTemplates = case metaTemplates of
            Nothing -> defaultApply
            Just a -> do
                let go a = case a of
                        "None" -> return
                        "Self" -> applyAsTemplate context
                        "Default" -> defaultApply
                        a -> loadAndApplyTemplate (fromFilePath a) context

                concatM $ Data.List.map go a

    compiler >>= applyTemplates

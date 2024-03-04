{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Compiler.Overridable where

import Control.Monad.Loops
import Data.List
import Data.Map
import Data.Maybe
import Data.String
import Data.Text (splitOn, strip)
import Data.Text.Conversions

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
    { getDefaults :: !(CompilerDefaults a)
    , getCompilers :: !(Map String (Compiler (Item a)))
    , getContexts :: !(Map String (Context a))
    }

providers defaults = CompilerProviders defaults empty empty

mapDefaults f providers = providers{getDefaults = f $ getDefaults providers}
mapCompilers f providers = providers{getCompilers = f $ getCompilers providers}
mapContexts f providers = providers{getContexts = f $ getContexts providers}

withCompiler k c = mapCompilers (Data.Map.insert k c)
withContext k c = mapContexts (Data.Map.insert k c)

splitList a = fromText <$> (strip <$> splitOn "," (fromString a))

overridableCompiler :: CompilerProviders String -> Identifier -> Compiler (Item String)
overridableCompiler providers ident = do
    let defaultCompiler = getCompiler $ getDefaults providers
    metaCompiler <- getMetadataField ident "compilers"
    let compiler =
            maybe
                defaultCompiler
                ( \a -> do
                    let metaCompilers = splitList a :: [String]
                    let go a = case Data.Map.lookup a $ getCompilers providers of
                            Just a -> a
                            Nothing -> error $ "Invalid compiler " ++ a
                    let compilers = Data.List.map go metaCompilers
                    foldl1' ((=<<) . return) compilers
                )
                metaCompiler

    let defaultContext = getContext $ getDefaults providers
    metaCompiler <- getMetadataField ident "contexts"
    let context =
            maybe
                defaultContext
                ( \a -> do
                    let metaContexts = splitList a :: [String]
                    let go a = case Data.Map.lookup a $ getContexts providers of
                            Just a -> a
                            Nothing -> error $ "Invalid context " ++ a
                    let contexts = Data.List.map go metaContexts
                    foldl1' (<>) contexts
                )
                metaCompiler

    let defaultApply = loadAndApplyTemplate (getTemplate $ getDefaults providers) context
    metaTemplates <- getMetadataField ident "templates"
    let applyTemplates = case metaTemplates of
            Nothing -> defaultApply
            Just a -> do
                let templates = splitList a
                let go a = case a of
                        "None" -> return
                        "Self" -> applyAsTemplate context
                        "Default" -> defaultApply
                        a -> loadAndApplyTemplate (fromFilePath a) context

                concatM $ Data.List.map go templates

    compiler >>= applyTemplates

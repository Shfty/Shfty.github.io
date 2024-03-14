{-# LANGUAGE DeriveTraversable #-}

module Data.FileTree where

import Data.Functor
import Data.List
import Data.List.Utils
import Data.Maybe

import Control.Monad.State

import System.FilePath

import Hakyll
import Hakyll.Glyphs

import Text.Blaze.Html.Renderer.String

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data FileTree a = Branch !a ![FileTree a] | Leaf !a deriving (Eq, Ord, Functor, Foldable, Traversable)

instance (Show a) => Show (FileTree a) where
    show (Branch a as) = replace "\n" "\n\t" $ show a ++ ":\n" ++ mconcat ((++ "\n") . show <$> as)
    show (Leaf b) = show b

instance Applicative FileTree where
    pure = Leaf

    Leaf f <*> Leaf a = Leaf $ f a
    Branch f fs <*> Leaf a = Leaf $ f a
    Branch f fs <*> Branch a as = do
        let as' = zip fs as
        let as'' = uncurry (<*>) <$> as'
        Branch (f a) as''

menuTree :: FileTree (Identifier, Metadata) -> Compiler (FileTree H.Html)
menuTree a = do
    case a of
        (Branch (id, meta) as) -> do
            route <- getRoute id
            let route' = fromMaybe (error "No route") route
            slug <- loadBody $ setVersion (Just "slug") id :: Compiler String
            let html = H.summary $ H.a H.! A.href (H.toValue $ "/" ++ route') $ H.preEscapedString slug
            as' <- mapM menuTree as
            mapM menuTree as >>= return . Branch html
        (Leaf (id, meta)) -> do
            route <- getRoute id
            let route' = fromMaybe (error "No route") route
            slug <- loadBody $ setVersion (Just "slug") id :: Compiler String
            return $ Leaf $ H.a H.! A.href (H.toValue $ "/" ++ route') $ H.preEscapedString slug

tag :: FileTree FilePath -> FileTree (FilePath, FilePath)
tag tree = evalState (tagStep tree) mempty

tagStep :: FileTree FilePath -> State FilePath (FileTree (FilePath, FilePath))
tagStep (Branch a as) = do
    acc <- get
    put $ acc </> a
    as' <- mapM tagStep as
    put acc
    return $ Branch (a, acc) as'
tagStep (Leaf a) = do
    acc <- get
    return $ Leaf (a, acc)

-- Create a chain of branches from a list
makePath [x] = Leaf x
makePath (x : xs) = Branch x [makePath xs]

makePaths items =
    makePath . splitDirectories . toFilePath <$> items

makeFileTree pat = do
    items <- getMatches pat
    let paths = makePaths items
    let [fs] = foldl' concatFileTree [] (return <$> paths)
    return fs

-- Recursively merge chains created by makeFS into a single tree
concatFileTree :: (Ord a) => [FileTree a] -> [FileTree a] -> [FileTree a]
concatFileTree [Branch a as] [Branch b bs] =
    let go = foldr (concatFileTree . (: [])) []
     in if a == b
            then [Branch a $ go (as <> bs)]
            else [Branch a $ go as, Branch b $ go bs]
concatFileTree a b = a <> b

-- Recursive sort
sortFileTree a = case a of
    Branch x xs -> Branch x $ sort (sortFileTree <$> xs)
    a -> a

sortFileTreeBy f a = case a of
    Branch x xs -> Branch x $ sortBy f (sortFileTree <$> xs)
    a -> a

-- Filter leaves by their value
filterFileTree f a = do
    let go (Branch x xs) = [Branch x $ mconcat $ go <$> xs]
        go (Leaf a) = ([Leaf a | f a])
    let [indices] = go a
    indices

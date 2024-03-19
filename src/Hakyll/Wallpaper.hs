module Hakyll.Wallpaper where

import Data.Foldable

import Hakyll
import Hakyll.Images

import System.FilePath

-- Generate a list of progressively smaller (width, height) tuples
wallpaperSizes :: Int -> [Int] -> Width -> Height -> [(Width, Height)]
wallpaperSizes fac range w h =
    [ (w `div` p, h `div` p)
    | p <-
        [ fac ^ y
        | y <- range
        ]
    ]

wallpaperSizes2 = wallpaperSizes 2

-----------------------------------------------------------------------------------------

makeWallpaper pat (width, height) = do
    let size = show width ++ "-" ++ show height
    match pat $ version size $ do
        route $
            customRoute
                ( \a -> do
                    let path = toFilePath a
                    let dir = takeDirectory path
                    let base = takeBaseName path
                    let ext = takeExtension path
                    let a = dir </> base ++ "-" ++ size <.> ext
                    a
                )
        compile $
            cached size $ loadImage
                >>= ensureFitCompiler width height

wallpapers landscape portrait = do
    -- Full-size wallpapers
    match landscape $ do
        route idRoute
        compile copyFileCompiler

    match portrait $ do
        route idRoute
        compile copyFileCompiler

    -- Generate progressively-smaller copies
    let landscapeSizes = wallpaperSizes2 [0 .. 4] 3840 2160
    let portraitSizes = wallpaperSizes2 [0 .. 4] 2160 3840
    let makeLandscape = makeWallpaper landscape
    let makePortrait = makeWallpaper portrait

    traverse_ makeLandscape landscapeSizes
    traverse_ makePortrait portraitSizes


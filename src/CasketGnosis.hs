{- |
Module      : CasketGnosis
Description : Casket-SSG enhanced with Gnosis metadata-driven rendering
Copyright   : (c) 2025-2026 Jonathan D.A. Jewell
License     : PMPL-1.0-or-later
Maintainer  : hyperpolymath

Casket-SSG with Gnosis integration for 6scm metadata-driven static sites.
-}

module Main where

import System.Environment (getArgs)
import System.Directory (listDirectory, createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeBaseName, takeExtension)
import Control.Monad (forM_)

import qualified Gnosis.SixSCM as Gnosis
import qualified Gnosis.Render as Render
import qualified Gnosis.DAX as DAX
import qualified Gnosis.Types as Types

-- | Main entry point for casket-ssg with Gnosis
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["build", inputDir, outputDir] -> do
            buildSiteWithGnosis inputDir outputDir

        ["build", inputDir] -> do
            buildSiteWithGnosis inputDir "_site"

        ["--version"] ->
            putStrLn "Casket-SSG v1.0.0 with Gnosis integration"

        _ -> do
            putStrLn "Casket-SSG - Metadata-driven static site generator"
            putStrLn ""
            putStrLn "Usage:"
            putStrLn "  casket-ssg build <input-dir> [output-dir]"
            putStrLn "  casket-ssg --version"
            putStrLn ""
            putStrLn "Features:"
            putStrLn "  - 6scm metadata integration"
            putStrLn "  - (:placeholder) rendering"
            putStrLn "  - FlexiText accessibility"
            putStrLn "  - Markdown processing"

-- | Build site with Gnosis metadata integration
buildSiteWithGnosis :: FilePath -> FilePath -> IO ()
buildSiteWithGnosis inputDir outputDir = do
    putStrLn "Casket-SSG: Building site with Gnosis metadata integration"
    putStrLn $ "  Input:  " ++ inputDir
    putStrLn $ "  Output: " ++ outputDir

    -- Load 6scm context from .machine_readable/
    let scmPath = ".machine_readable"
    ctx <- Gnosis.loadAll6SCM scmPath
    putStrLn $ "  Loaded 6scm context from: " ++ scmPath

    -- Create output directory
    createDirectoryIfMissing True outputDir

    -- Find all markdown files
    files <- listDirectory inputDir
    let mdFiles = filter (\f -> takeExtension f `elem` [".md", ".markdown"]) files

    if null mdFiles
        then putStrLn "No markdown files found."
        else do
            putStrLn $ "Found " ++ show (length mdFiles) ++ " markdown files:"
            forM_ mdFiles $ \file -> do
                let inputPath = inputDir </> file
                let outputPath = outputDir </> takeBaseName file ++ ".html"
                processFileWithGnosis ctx inputPath outputPath

    putStrLn "Build complete!"

-- | Process a single file with Gnosis rendering
processFileWithGnosis :: Types.Context -> FilePath -> FilePath -> IO ()
processFileWithGnosis ctx inputPath outputPath = do
    putStrLn $ "  " ++ inputPath ++ " -> " ++ outputPath

    -- Read template file
    content <- readFile inputPath

    -- Step 1: Apply Gnosis rendering ((:placeholder))
    let withPlaceholders = Render.renderWithBadges ctx content

    -- Step 2: Process DAX features ({{#if}}, {{#for}})
    let withDAX = DAX.processTemplate ctx withPlaceholders

    -- Step 3: Wrap in simple HTML template
    let html = wrapInTemplate (takeBaseName inputPath) withDAX

    -- Write output
    writeFile outputPath html

-- | Wrap content in simple HTML template
wrapInTemplate :: String -> String -> String
wrapInTemplate title content = unlines
    [ "<!DOCTYPE html>"
    , "<html><head>"
    , "<meta charset=\"UTF-8\">"
    , "<title>" ++ title ++ "</title>"
    , "<style>"
    , "body{font-family:system-ui;max-width:800px;margin:0 auto;padding:2rem}"
    , "pre{background:#f4f4f4;padding:1rem;overflow-x:auto}"
    , "code{background:#f4f4f4;padding:0.2em 0.4em;border-radius:3px}"
    , "</style>"
    , "</head><body>"
    , "<article>"
    , content
    , "</article>"
    , "</body></html>"
    ]

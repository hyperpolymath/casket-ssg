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
import qualified Data.Map.Strict as Map

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

    -- Parse frontmatter and content
    let (frontmatter, bodyContent) = parseFrontmatter content

    -- Merge frontmatter into context (frontmatter takes precedence)
    let mergedCtx = mergeFrontmatter ctx frontmatter

    -- Get page title from frontmatter or fallback to filename
    let pageTitle = Map.findWithDefault (takeBaseName inputPath) "title" frontmatter

    -- Step 1: Process DAX features first ({{#if}}, {{#for}})
    -- This expands loops with literal values and evaluates conditionals
    let withDAX = DAX.processTemplate mergedCtx bodyContent

    -- Step 2: Apply Gnosis rendering ((:placeholder))
    -- This renders remaining 6scm placeholders and applies filters
    let withPlaceholders = Render.renderWithBadges mergedCtx withDAX

    -- Step 3: Wrap in simple HTML template
    let html = wrapInTemplate pageTitle withPlaceholders

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

-- | Parse frontmatter from content
-- Returns (frontmatter Map, remaining content)
parseFrontmatter :: String -> (Map.Map String String, String)
parseFrontmatter content
    | "---\n" `isPrefixOf` content || "---\r\n" `isPrefixOf` content =
        let afterFirst = dropWhile (/= '\n') content
            rest = drop 1 afterFirst  -- Drop first newline
            (frontmatterText, bodyWithDelim) = break isEndDelimiter (lines rest)
            body = unlines (dropWhile isEndDelimiter bodyWithDelim)
            frontmatterMap = parseFrontmatterLines frontmatterText
        in (frontmatterMap, body)
    | otherwise = (Map.empty, content)
  where
    isEndDelimiter line = line == "---" || line == "---\r"
    isPrefixOf needle haystack = take (length needle) haystack == needle

-- | Parse frontmatter lines into Map
parseFrontmatterLines :: [String] -> Map.Map String String
parseFrontmatterLines = Map.fromList . map parseLine . filter (not . null)
  where
    parseLine line =
        let (key, rest) = break (== ':') line
            value = dropWhile (== ' ') (drop 1 rest)  -- Drop ':' and spaces
        in (trim key, trim value)

    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
    isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

-- | Merge frontmatter into context (frontmatter takes precedence)
mergeFrontmatter :: Types.Context -> Map.Map String String -> Types.Context
mergeFrontmatter baseCtx frontmatter =
    let frontmatterAsContext = Map.mapWithKey (\k v -> Types.FlexiText v ("Frontmatter: " ++ k)) frontmatter
    in Map.union frontmatterAsContext baseCtx  -- frontmatter overrides base

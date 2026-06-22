-- SPDX-License-Identifier: MPL-2.0
{- |
Module      : CasketGnosis
Description : Casket-SSG enhanced with Gnosis metadata-driven rendering
Copyright   : (c) 2025-2026 Jonathan D.A. Jewell
License     : MPL-2.0
Maintainer  : hyperpolymath

Casket-SSG with Gnosis integration for 6scm metadata-driven static sites.
-}

module Main where

import System.Environment (getArgs)
import System.Directory (listDirectory, createDirectoryIfMissing, doesFileExist, doesDirectoryExist, copyFile)
import System.FilePath ((</>), takeBaseName, takeExtension)
import Control.Monad (forM_, when)
import Data.List (isPrefixOf)

import qualified Gnosis.SixSCM as Gnosis
import qualified Gnosis.Render as Render
import qualified Gnosis.DAX as DAX
import qualified Gnosis.Types as Types
import qualified Data.Map.Strict as Map
import qualified Text.Pandoc as Pandoc
import qualified Data.Text as T

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

    -- Load the page template (CWD templates/default.html, else built-in)
    template <- loadTemplate

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
                processFileWithGnosis ctx template inputPath outputPath

    -- Copy theme assets (CWD assets/, static/, ... -> outputDir/<dir>/)
    copyAssets outputDir
    -- Copy <inputDir>/public/ contents to the site root (security.txt, CNAME, ...)
    copyPublic inputDir outputDir

    putStrLn "Build complete!"

-- | Process a single file with Gnosis rendering
processFileWithGnosis :: Types.Context -> String -> FilePath -> FilePath -> IO ()
processFileWithGnosis ctx template inputPath outputPath = do
    putStrLn $ "  " ++ inputPath ++ " -> " ++ outputPath

    -- Read template file
    content <- readFile inputPath

    -- Parse frontmatter and content
    let (frontmatter, bodyContent) = parseFrontmatter content

    -- Merge frontmatter into context (frontmatter takes precedence)
    let mergedCtx = mergeFrontmatter ctx frontmatter

    -- Page title (frontmatter, else filename); brand is the site wordmark
    -- (frontmatter "site", else the page title); date is optional.
    let pageTitle = Map.findWithDefault (takeBaseName inputPath) "title" frontmatter
    let brand     = Map.findWithDefault pageTitle "site" frontmatter
    let pageDate  = Map.findWithDefault "" "date" frontmatter

    -- Step 1: Process DAX features first ({{#if}}, {{#for}})
    -- This expands loops with literal values and evaluates conditionals
    let withDAX = DAX.processTemplate mergedCtx bodyContent

    -- Step 2: Apply Gnosis rendering ((:placeholder))
    -- This renders remaining 6scm placeholders and applies filters
    let withPlaceholders = Render.renderWithBadges mergedCtx withDAX

    -- Step 3: Convert Markdown to HTML using Pandoc
    htmlContent <- markdownToHtml withPlaceholders

    -- Step 4: Wrap in the loaded theme template
    let html = applyTemplate template brand pageTitle pageDate htmlContent

    -- Write output
    writeFile outputPath html

-- | Convert Markdown to HTML using Pandoc
markdownToHtml :: String -> IO String
markdownToHtml markdown = do
    -- Enable the standard markdown extension set: raw_html (pass through inline
    -- HTML for hero/badges/cards), tables (pipe tables), fenced_code, etc.
    -- Pandoc.def has EMPTY extensions, which would escape raw HTML and ignore tables.
    let readerOpts = Pandoc.def { Pandoc.readerExtensions = Pandoc.pandocExtensions }
    let writerOpts = Pandoc.def
    result <- Pandoc.runIOorExplode $ do
        doc <- Pandoc.readMarkdown readerOpts (T.pack markdown)
        Pandoc.writeHtml5String writerOpts doc
    return (T.unpack result)

-- | Load the page template from templates/default.html (relative to CWD),
-- falling back to a minimal built-in template if the file is absent.
loadTemplate :: IO String
loadTemplate = do
    let path = "templates" </> "default.html"
    exists <- doesFileExist path
    if exists then readFile path else return builtinTemplate

-- | Minimal fallback template (used only when templates/default.html is missing).
builtinTemplate :: String
builtinTemplate = unlines
    [ "<!DOCTYPE html>"
    , "<html lang=\"en\"><head>"
    , "<meta charset=\"UTF-8\">"
    , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
    , "<title>{{title}}</title>"
    , "<link rel=\"stylesheet\" href=\"/assets/style.css\">"
    , "</head><body>"
    , "<header class=\"site-header\"><nav class=\"container nav\">"
    , "<a class=\"brand\" href=\"/\">{{brand}}</a></nav></header>"
    , "<main id=\"main\" class=\"container\"><article class=\"prose\">"
    , "{{content}}"
    , "</article></main>"
    , "<footer class=\"site-footer\"><div class=\"container\"><p class=\"muted\">"
    , "<time>{{date}}</time></p></div></footer>"
    , "</body></html>"
    ]

-- | Fill the template. {{content}} is substituted last so that arbitrary HTML
-- in the body cannot be re-processed by later substitutions.
applyTemplate :: String -> String -> String -> String -> String -> String
applyTemplate template brand title date content =
      replaceAll "{{content}}" content
    . replaceAll "{{date}}"  date
    . replaceAll "{{title}}" title
    . replaceAll "{{brand}}" brand
    $ template

-- | Replace every occurrence of a needle with a replacement.
replaceAll :: String -> String -> String -> String
replaceAll "" _ str = str
replaceAll needle replacement str = go str
  where
    nLen = length needle
    go [] = []
    go s@(c:cs)
      | needle `isPrefixOf` s = replacement ++ go (drop nLen s)
      | otherwise             = c : go cs

-- | Copy theme asset directories (relative to CWD) into the output, preserving
-- the directory name so URLs resolve as /assets/style.css etc.
copyAssets :: FilePath -> IO ()
copyAssets outputDir =
    forM_ ["assets", "static", "css", "js", "images"] $ \d -> do
        exists <- doesDirectoryExist d
        when exists $ copyDirectory d (outputDir </> d)

-- | Copy <inputDir>/public/ contents into the OUTPUT ROOT, verbatim, so
-- root-level web files ship: .well-known/security.txt, CNAME, robots.txt,
-- favicon.svg, humans.txt, ads.txt, etc.
copyPublic :: FilePath -> FilePath -> IO ()
copyPublic inputDir outputDir = do
    let publicDir = inputDir </> "public"
    exists <- doesDirectoryExist publicDir
    when exists $ do
        entries <- listDirectory publicDir
        forM_ entries $ \entry -> do
            let srcPath = publicDir </> entry
            let dstPath = outputDir </> entry
            isDir <- doesDirectoryExist srcPath
            if isDir then copyDirectory srcPath dstPath else copyFile srcPath dstPath

-- | Recursively copy a directory.
copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory src dst = do
    createDirectoryIfMissing True dst
    entries <- listDirectory src
    forM_ entries $ \entry -> do
        let srcPath = src </> entry
        let dstPath = dst </> entry
        isDir <- doesDirectoryExist srcPath
        if isDir then copyDirectory srcPath dstPath else copyFile srcPath dstPath

-- | Parse frontmatter from content. Tolerates leading blank lines and
-- single-line HTML comments (e.g. an SPDX header) before the opening `---`
-- fence. Returns (frontmatter Map, remaining content).
parseFrontmatter :: String -> (Map.Map String String, String)
parseFrontmatter content =
    case dropWhile isSkippable (lines content) of
        (delim : afterOpen) | strip delim == "---" ->
            let (fmLines, bodyWithDelim) = break (\l -> strip l == "---") afterOpen
                body = unlines (drop 1 bodyWithDelim)  -- drop the closing ---
            in (parseFrontmatterLines fmLines, body)
        _ -> (Map.empty, content)
  where
    isSkippable line = let s = strip line in null s || "<!--" `isPrefixOf` s
    strip = f . f where f = reverse . dropWhile (`elem` (" \t\r" :: String))

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

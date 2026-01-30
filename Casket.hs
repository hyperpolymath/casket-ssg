{- HakyllLite.hs - Haskell-powered static site generator

   "HakyllLite" - Functional elegance in site generation

   A simplified Hakyll-inspired SSG showcasing Haskell's:
   - Pure functions for predictable transformations
   - Pattern matching for elegant parsing
   - Lazy evaluation for efficient processing
-}

module Main where

import Data.List (isPrefixOf, isSuffixOf, foldl')
import Data.Char (isSpace)
import System.Environment (getArgs)
import System.Directory (listDirectory, createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeBaseName, takeExtension)
import Control.Monad (forM_, when)

-- ============================================================================
-- Types
-- ============================================================================

data Frontmatter = Frontmatter
  { fmTitle    :: String
  , fmDate     :: String
  , fmTags     :: [String]
  , fmDraft    :: Bool
  , fmTemplate :: String
  } deriving (Show)

emptyFrontmatter :: Frontmatter
emptyFrontmatter = Frontmatter "" "" [] False "default"

data ParserState = ParserState
  { stHtml    :: String
  , stInPara  :: Bool
  , stInCode  :: Bool
  , stInList  :: Bool
  } deriving (Show)

initState :: ParserState
initState = ParserState "" False False False

-- ============================================================================
-- String Utilities
-- ============================================================================

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

stripPrefix' :: String -> String -> String
stripPrefix' prefix str
  | prefix `isPrefixOf` str = drop (length prefix) str
  | otherwise = str

escapeHtml :: String -> String
escapeHtml = concatMap esc
  where
    esc '<' = "&lt;"
    esc '>' = "&gt;"
    esc '&' = "&amp;"
    esc '"' = "&quot;"
    esc c   = [c]

splitOn :: Char -> String -> [String]
splitOn _ "" = [""]
splitOn c s = case break (== c) s of
  (a, "")   -> [a]
  (a, _:b)  -> a : splitOn c b

-- ============================================================================
-- Frontmatter Parser
-- ============================================================================

parseFmLine :: String -> Frontmatter -> Frontmatter
parseFmLine line fm = case break (== ':') line of
  (_, "")    -> fm
  (key, _:val) ->
    let k = trim key
        v = trim val
    in case k of
      "title"    -> fm { fmTitle = v }
      "date"     -> fm { fmDate = v }
      "template" -> fm { fmTemplate = v }
      "draft"    -> fm { fmDraft = v `elem` ["true", "yes"] }
      "tags"     -> fm { fmTags = parseTags v }
      _          -> fm
  where
    parseTags s
      | "[" `isPrefixOf` s = map trim $ splitOn ',' $ init $ tail s
      | otherwise = map trim $ splitOn ',' s

parseFrontmatter :: String -> (Frontmatter, String)
parseFrontmatter content =
  let allLines = lines content
  in case allLines of
    (first:rest) | trim first == "---" -> findEnd rest emptyFrontmatter
    _ -> (emptyFrontmatter, content)
  where
    findEnd [] fm = (fm, "")
    findEnd (l:ls) fm
      | trim l == "---" = (fm, unlines ls)
      | otherwise = findEnd ls (parseFmLine l fm)

-- ============================================================================
-- Markdown Parser
-- ============================================================================

processInline :: String -> String
processInline = go False False
  where
    go _ _ [] = []
    go bold ital ('[':rest) =
      -- Try to parse markdown link [text](url)
      let (text, afterText) = span (/= ']') rest
      in case afterText of
        (']':'(':moreRest) ->
          let (url, afterUrl) = span (/= ')') moreRest
          in case afterUrl of
            (')':remaining) -> "<a href=\"" ++ url ++ "\">" ++ text ++ "</a>" ++ go bold ital remaining
            _ -> '[' : go bold ital rest
        _ -> '[' : go bold ital rest
    go bold ital ('*':'*':rest)
      | bold      = "</strong>" ++ go False ital rest
      | otherwise = "<strong>" ++ go True ital rest
    go bold ital ('*':rest)
      | ital      = "</em>" ++ go bold False rest
      | otherwise = "<em>" ++ go bold True rest
    go bold ital ('`':rest) =
      let (code, remaining) = span (/= '`') rest
      in case remaining of
        ('`':r) -> "<code>" ++ code ++ "</code>" ++ go bold ital r
        _       -> '`' : go bold ital rest
    go bold ital (c:rest) = c : go bold ital rest

processLine :: String -> ParserState -> ParserState
processLine line st =
  let trimmed = trim line
  in
    -- Code fence
    if "```" `isPrefixOf` trimmed then
      if stInCode st
        then st { stHtml = stHtml st ++ "</code></pre>\n", stInCode = False }
        else let st' = closePara $ closeList st
             in st' { stHtml = stHtml st' ++ "<pre><code>", stInCode = True }

    -- Inside code block
    else if stInCode st then
      st { stHtml = stHtml st ++ escapeHtml line ++ "\n" }

    -- Empty line
    else if null trimmed then
      closeList $ closePara st

    -- Headers (check longest first)
    else if "######" `isPrefixOf` trimmed then
      processHeader 6 "######" trimmed st
    else if "#####" `isPrefixOf` trimmed then
      processHeader 5 "#####" trimmed st
    else if "####" `isPrefixOf` trimmed then
      processHeader 4 "####" trimmed st
    else if "###" `isPrefixOf` trimmed then
      processHeader 3 "###" trimmed st
    else if "##" `isPrefixOf` trimmed then
      processHeader 2 "##" trimmed st
    else if "#" `isPrefixOf` trimmed then
      processHeader 1 "#" trimmed st

    -- List items
    else if "- " `isPrefixOf` trimmed || "* " `isPrefixOf` trimmed then
      let st' = closePara st
          st'' = if not (stInList st')
                   then st' { stHtml = stHtml st' ++ "<ul>\n", stInList = True }
                   else st'
          item = processInline $ trim $ drop 2 trimmed
      in st'' { stHtml = stHtml st'' ++ "<li>" ++ item ++ "</li>\n" }

    -- Paragraph
    else
      let st' = if not (stInPara st)
                  then st { stHtml = stHtml st ++ "<p>", stInPara = True }
                  else st { stHtml = stHtml st ++ " " }
      in st' { stHtml = stHtml st' ++ processInline trimmed }

  where
    processHeader n prefix text state =
      let st' = closeList $ closePara state
          content = processInline $ trim $ stripPrefix' prefix text
          tag = show n
      in st' { stHtml = stHtml st' ++ "<h" ++ tag ++ ">" ++ content ++ "</h" ++ tag ++ ">\n" }

closePara :: ParserState -> ParserState
closePara st
  | stInPara st = st { stHtml = stHtml st ++ "</p>\n", stInPara = False }
  | otherwise = st

closeList :: ParserState -> ParserState
closeList st
  | stInList st = st { stHtml = stHtml st ++ "</ul>\n", stInList = False }
  | otherwise = st

parseMarkdown :: String -> String
parseMarkdown content =
  let allLines = lines content
      final = foldl' (flip processLine) initState allLines
      st' = closeList $ closePara final
      st'' = if stInCode st'
               then st' { stHtml = stHtml st' ++ "</code></pre>\n" }
               else st'
  in stHtml st''

-- ============================================================================
-- Template Engine
-- ============================================================================

defaultTemplate :: String
defaultTemplate = unlines
  [ "<!DOCTYPE html>"
  , "<html><head><meta charset=\"UTF-8\"><title>{{title}}</title>"
  , "<style>body{font-family:system-ui;max-width:800px;margin:0 auto;padding:2rem}pre{background:#f4f4f4;padding:1rem}</style>"
  , "</head><body><article><h1>{{title}}</h1><time>{{date}}</time>"
  , "{{content}}"
  , "</article></body></html>"
  ]

replaceAll :: String -> String -> String -> String
replaceAll "" _ str = str
replaceAll needle replacement str = go str
  where
    nLen = length needle
    go s
      | length s < nLen = s
      | needle `isPrefixOf` s = replacement ++ go (drop nLen s)
      | otherwise = head s : go (tail s)

applyTemplate :: Frontmatter -> String -> String
applyTemplate fm html =
  let t1 = replaceAll "{{title}}" (fmTitle fm) defaultTemplate
      t2 = replaceAll "{{date}}" (fmDate fm) t1
      t3 = replaceAll "{{content}}" html t2
  in t3

-- ============================================================================
-- Tests
-- ============================================================================

testMarkdown :: IO ()
testMarkdown = do
  putStrLn "=== Test: Markdown ==="
  let md = unlines
        [ "# Hello World"
        , ""
        , "This is a **bold** test with *italic* text."
        , ""
        , "- Item 1"
        , "- Item 2"
        , ""
        , "```"
        , "code block"
        , "```"
        ]
  putStrLn $ parseMarkdown md

testFrontmatter :: IO ()
testFrontmatter = do
  putStrLn "=== Test: Frontmatter ==="
  let content = unlines
        [ "---"
        , "title: My Post"
        , "date: 2024-01-15"
        , "tags: [haskell, ssg]"
        , "draft: false"
        , "---"
        , ""
        , "Content here"
        ]
  let (fm, body) = parseFrontmatter content
  putStrLn $ "Title: " ++ fmTitle fm
  putStrLn $ "Date: " ++ fmDate fm
  putStrLn $ "Tags: " ++ show (fmTags fm)
  putStrLn $ "Draft: " ++ show (fmDraft fm)
  putStrLn $ "Body: " ++ body

testFull :: IO ()
testFull = do
  putStrLn "=== Test: Full Pipeline ==="
  let content = unlines
        [ "---"
        , "title: Welcome"
        , "date: 2024-01-15"
        , "---"
        , ""
        , "# Welcome"
        , ""
        , "This is **HakyllLite**, a Haskell SSG."
        , ""
        , "- Pure functional"
        , "- Type safe"
        , "- Elegant"
        ]
  let (fm, body) = parseFrontmatter content
  let html = parseMarkdown body
  let output = applyTemplate fm html
  putStrLn output

-- ============================================================================
-- Build System
-- ============================================================================

-- | Process a single markdown file
processFile :: FilePath -> FilePath -> IO ()
processFile inputPath outputPath = do
  content <- readFile inputPath
  let (fm, body) = parseFrontmatter content
  -- Skip draft posts
  when (not $ fmDraft fm) $ do
    let html = parseMarkdown body
    let output = applyTemplate fm html
    writeFile outputPath output
    putStrLn $ "  " ++ inputPath ++ " -> " ++ outputPath

-- | Build site from input directory to output directory
buildSite :: FilePath -> FilePath -> IO ()
buildSite inputDir outputDir = do
  putStrLn $ "Building site: " ++ inputDir ++ " -> " ++ outputDir
  createDirectoryIfMissing True outputDir

  files <- listDirectory inputDir
  let mdFiles = filter (\f -> takeExtension f == ".md") files

  if null mdFiles
    then putStrLn "No markdown files found."
    else do
      putStrLn $ "Found " ++ show (length mdFiles) ++ " markdown files:"
      forM_ mdFiles $ \file -> do
        let inputPath = inputDir </> file
        let outputPath = outputDir </> takeBaseName file ++ ".html"
        processFile inputPath outputPath

  putStrLn "Build complete!"

-- | Generate an index page listing all posts (only if no index.md exists)
generateIndex :: FilePath -> FilePath -> IO ()
generateIndex inputDir outputDir = do
  files <- listDirectory inputDir
  let mdFiles = filter (\f -> takeExtension f == ".md") files

  -- Don't generate if index.md exists (it will have its own index.html)
  let hasIndexMd = "index.md" `elem` mdFiles
  when hasIndexMd $
    putStrLn "Skipping index generation (index.md exists)"

  when (not hasIndexMd) $ do
    -- Read frontmatter from each file to build index
    entries <- mapM (\f -> do
      content <- readFile (inputDir </> f)
      let (fm, _) = parseFrontmatter content
      return (takeBaseName f, fm)
      ) mdFiles

    let validEntries = filter (not . fmDraft . snd) entries
    let listItems = map (\(name, fm) ->
          "<li><a href=\"" ++ name ++ ".html\">" ++
          (if null (fmTitle fm) then name else fmTitle fm) ++
          "</a>" ++
          (if null (fmDate fm) then "" else " <small>(" ++ fmDate fm ++ ")</small>") ++
          "</li>") validEntries

    let indexHtml = unlines
          [ "<!DOCTYPE html>"
          , "<html><head><meta charset=\"UTF-8\"><title>Index</title>"
          , "<style>body{font-family:system-ui;max-width:800px;margin:0 auto;padding:2rem}a{color:#0066cc}</style>"
          , "</head><body>"
          , "<h1>Site Index</h1>"
          , "<ul>"
          ] ++ unlines listItems ++ unlines
          [ "</ul>"
          , "</body></html>"
          ]

    writeFile (outputDir </> "index.html") indexHtml
    putStrLn $ "Generated index.html with " ++ show (length validEntries) ++ " entries"

-- ============================================================================
-- Main
-- ============================================================================

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["build", inputDir, outputDir] -> do
      buildSite inputDir outputDir
      generateIndex inputDir outputDir
    ["build", inputDir] -> do
      buildSite inputDir "_site"
      generateIndex inputDir "_site"
    ["test-markdown"]    -> testMarkdown
    ["test-frontmatter"] -> testFrontmatter
    ["test-full"]        -> testFull
    _ -> do
      putStrLn "casket-ssg - Pure functional static site generator in Haskell"
      putStrLn ""
      putStrLn "Usage:"
      putStrLn "  casket-ssg build <input-dir> [output-dir]  Build site (default output: _site)"
      putStrLn "  casket-ssg test-markdown                   Test markdown parser"
      putStrLn "  casket-ssg test-frontmatter                Test frontmatter parser"
      putStrLn "  casket-ssg test-full                       Test full pipeline"

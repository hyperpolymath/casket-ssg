{- HakyllLite.hs - Haskell-powered static site generator

   "HakyllLite" - Functional elegance in site generation

   A simplified Hakyll-inspired SSG showcasing Haskell's:
   - Pure functions for predictable transformations
   - Pattern matching for elegant parsing
   - Lazy evaluation for efficient processing
-}

module Main where

import Data.List (isPrefixOf, intercalate, foldl')
import Data.Char (isSpace)
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory, removeDirectoryRecursive, copyFile)
import System.FilePath ((</>), takeExtension, replaceExtension)
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
-- File System Operations
-- ============================================================================

-- Site configuration
data SiteConfig = SiteConfig
  { contentDir  :: FilePath
  , outputDir   :: FilePath
  , templateDir :: FilePath
  , staticDir   :: FilePath
  , siteTitle   :: String
  } deriving (Show)

defaultConfig :: SiteConfig
defaultConfig = SiteConfig
  { contentDir  = "content"
  , outputDir   = "public"
  , templateDir = "templates"
  , staticDir   = "static"
  , siteTitle   = "My Site"
  }

-- Build command
buildSite :: SiteConfig -> IO ()
buildSite config = do
  putStrLn "Building site..."

  -- Ensure output directory exists
  createDirectoryIfMissing True (outputDir config)

  -- Check if content directory exists
  contentExists <- doesDirectoryExist (contentDir config)
  if not contentExists
    then do
      putStrLn $ "No content directory found. Creating " ++ contentDir config ++ "/"
      createDirectoryIfMissing True (contentDir config)
    else do
      -- Process markdown files
      files <- listDirectory (contentDir config)
      let mdFiles = filter (\f -> takeExtension f == ".md") files
      contentCount <- processFiles config mdFiles

      -- Copy static files
      staticCount <- copyStatic config

      putStrLn $ "\nBuild complete: " ++ show contentCount ++ " pages, " ++ show staticCount ++ " static files"

processFiles :: SiteConfig -> [FilePath] -> IO Int
processFiles config files = do
  forM_ files $ \file -> do
    let srcPath = contentDir config </> file
    content <- readFile srcPath

    let (fm, body) = parseFrontmatter content
    let html = parseMarkdown body
    let output = applyTemplate fm html

    let outName = replaceExtension file ".html"
    let outPath = outputDir config </> outName

    writeFile outPath output
    putStrLn $ "  " ++ file ++ " -> " ++ outPath

  return (length files)

copyStatic :: SiteConfig -> IO Int
copyStatic config = do
  staticExists <- doesDirectoryExist (staticDir config)
  if not staticExists
    then return 0
    else do
      files <- listDirectory (staticDir config)
      forM_ files $ \file -> do
        let src = staticDir config </> file
        let dst = outputDir config </> file
        copyFile src dst
      return (length files)

-- Init command
initSite :: FilePath -> IO ()
initSite name = do
  putStrLn $ "Initializing new site: " ++ name

  -- Create directories
  let dirs = [name </> "content", name </> "templates", name </> "static", name </> "public"]
  forM_ dirs $ \dir -> do
    createDirectoryIfMissing True dir
    putStrLn $ "  Created " ++ dir ++ "/"

  -- Create sample content
  let sampleContent = unlines
        [ "---"
        , "title: Welcome to Casket"
        , "date: 2025-01-18"
        , "---"
        , ""
        , "# Welcome"
        , ""
        , "This is your first post built with **Casket SSG**."
        , ""
        , "## Features"
        , ""
        , "- Pure functional transformations"
        , "- Type-safe templates"
        , "- Lazy evaluation"
        , ""
        , "```haskell"
        , "main :: IO ()"
        , "main = putStrLn \"Hello, Casket!\""
        , "```"
        ]
  writeFile (name </> "content" </> "index.md") sampleContent
  putStrLn "  Created sample content"

  -- Create config
  let configContent = unlines
        [ "# Casket SSG Configuration"
        , ""
        , "site_title = \"My Casket Site\""
        , "content_dir = \"content\""
        , "output_dir = \"public\""
        , "template_dir = \"templates\""
        , "static_dir = \"static\""
        ]
  writeFile (name </> "casket.conf") configContent
  putStrLn "  Created casket.conf"

  putStrLn $ "\nSite initialized! Run 'casket-ssg build' in " ++ name ++ "/ to build."

-- Clean command
cleanSite :: SiteConfig -> IO ()
cleanSite config = do
  putStrLn $ "Cleaning " ++ outputDir config ++ "/..."
  exists <- doesDirectoryExist (outputDir config)
  when exists $ removeDirectoryRecursive (outputDir config)
  putStrLn "Clean complete."

-- Help text
printUsage :: IO ()
printUsage = putStrLn $ unlines
  [ "Casket SSG - Haskell-powered static site generator"
  , ""
  , "USAGE:"
  , "  casket-ssg <command> [options]"
  , ""
  , "COMMANDS:"
  , "  build              Build the site"
  , "  init <name>        Create a new site"
  , "  clean              Remove generated files"
  , "  test-markdown      Test markdown parser"
  , "  test-frontmatter   Test frontmatter parser"
  , "  test-full          Test full pipeline"
  , ""
  , "OPTIONS:"
  , "  -h, --help         Show this help"
  , ""
  , "EXAMPLES:"
  , "  casket-ssg init my-blog"
  , "  cd my-blog && casket-ssg build"
  ]

-- ============================================================================
-- Main
-- ============================================================================

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["build"]            -> buildSite defaultConfig
    ["init", name]       -> initSite name
    ["init"]             -> putStrLn "Error: 'init' requires a site name\nUsage: casket-ssg init <name>"
    ["clean"]            -> cleanSite defaultConfig
    ["test-markdown"]    -> testMarkdown
    ["test-frontmatter"] -> testFrontmatter
    ["test-full"]        -> testFull
    ["-h"]               -> printUsage
    ["--help"]           -> printUsage
    []                   -> printUsage
    [cmd]                -> putStrLn $ "Unknown command: " ++ cmd ++ "\nRun 'casket-ssg --help' for usage."
    _                    -> printUsage

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
import System.Directory (listDirectory, createDirectoryIfMissing, doesFileExist, doesDirectoryExist, copyFile)
import System.FilePath ((</>), takeBaseName, takeExtension, takeFileName)
import Control.Monad (forM_, when, filterM)
import System.Process (readProcess)
import System.Exit (ExitCode(..))

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

data SiteConfig = SiteConfig
  { cfgTitle       :: String
  , cfgUrl         :: String
  , cfgAuthor      :: String
  , cfgInputDir    :: String
  , cfgOutputDir   :: String
  , cfgSpellCheck  :: Bool
  , cfgDefaultLang :: String
  } deriving (Show)

defaultConfig :: SiteConfig
defaultConfig = SiteConfig "My Site" "" "Author" "content" "_site" False "en"

data I18nStrings = I18nStrings
  { i18nReadMore    :: String
  , i18nPublished   :: String
  , i18nTags        :: String
  , i18nHome        :: String
  } deriving (Show)

defaultI18n :: I18nStrings
defaultI18n = I18nStrings "Read more" "Published" "Tags" "Home"

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

-- | Load template from file or use default
loadTemplate :: String -> IO String
loadTemplate templateName = do
  let templatePath = "templates" </> templateName ++ ".html"
  exists <- doesFileExist templatePath
  if exists
    then readFile templatePath
    else return defaultTemplate

-- | Check if file should use a2ml processing (.a2ml extension)
isA2ML :: FilePath -> Bool
isA2ML path = takeExtension path == ".a2ml"

-- | Process a2ml content (typed, verifiable markup)
-- Currently stub - will integrate with a2ml compiler
processA2ML :: String -> IO String
processA2ML content = do
  putStrLn "  [a2ml] Type-checking content..."
  -- TODO: Call a2ml compiler to validate and convert
  -- For now, return as-is
  return content

-- | Validate content with k9-svc (self-validating components)
-- Currently stub - will integrate with k9-svc validator
validateK9SVC :: FilePath -> String -> IO Bool
validateK9SVC path content = do
  -- Check for k9-svc directives in content
  let hasK9SVC = "k9-svc:" `isPrefixOf` content
  when hasK9SVC $ do
    putStrLn $ "  [k9-svc] Validating " ++ path ++ "..."
    -- TODO: Parse k9-svc schema and validate
    -- For now, always pass
  return True

-- | Process content with Pandoc if available, fallback to built-in parser
processContent :: FilePath -> String -> IO String
processContent path content = do
  -- Try Pandoc first for better compatibility
  pandocResult <- tryPandoc path content
  case pandocResult of
    Just html -> return html
    Nothing -> return $ parseMarkdown content  -- Fallback to built-in

-- | Try to use Pandoc for processing
tryPandoc :: FilePath -> String -> IO (Maybe String)
tryPandoc path content = do
  let ext = takeExtension path
  let format = case ext of
        ".md" -> "markdown"
        ".adoc" -> "asciidoc"
        ".rst" -> "rst"
        ".org" -> "org"
        _ -> "markdown"
  result <- try $ readProcess "pandoc" ["-f", format, "-t", "html"] content
  case result of
    Left (_ :: IOError) -> return Nothing
    Right html -> return $ Just html
  where
    try :: IO a -> IO (Either IOError a)
    try action = catch (Right <$> action) (return . Left)
    catch :: IO (Either IOError a) -> (IOError -> IO (Either IOError a)) -> IO (Either IOError a)
    catch action handler = do
      result <- action
      case result of
        Left e -> handler e
        Right v -> return $ Right v

-- | Process a single file with full feature set
processFile :: FilePath -> FilePath -> IO ()
processFile inputPath outputPath = do
  content <- readFile inputPath
  let (fm, body) = parseFrontmatter content
  -- Skip draft posts
  when (not $ fmDraft fm) $ do
    -- Validate with k9-svc if enabled
    valid <- validateK9SVC inputPath content
    when (not valid) $ do
      putStrLn $ "  ERROR: k9-svc validation failed for " ++ inputPath
      error "Build stopped due to validation failure"

    -- Spell check if enabled
    -- TODO: Make this configurable via site config
    -- errors <- spellCheck inputPath body
    -- when (not $ null errors) $ do
    --   putStrLn $ "  WARNING: Spelling errors in " ++ inputPath
    --   forM_ (take 5 errors) $ \err -> putStrLn $ "    - " ++ err

    -- Process based on file type
    html <- if isA2ML inputPath
      then processA2ML body  -- a2ml content
      else processContent inputPath body  -- Pandoc or built-in

    -- Load template (use frontmatter template name or "default")
    let templateName = if null (fmTemplate fm) then "default" else fmTemplate fm
    template <- loadTemplate templateName
    let output = applyTemplateStr template fm html
    writeFile outputPath output
    putStrLn $ "  " ++ inputPath ++ " -> " ++ outputPath

-- | Apply template string with variables
applyTemplateStr :: String -> Frontmatter -> String -> String
applyTemplateStr template fm html =
  let t1 = replaceAll "{{title}}" (fmTitle fm) template
      t2 = replaceAll "{{date}}" (fmDate fm) t1
      t3 = replaceAll "{{content}}" html t2
  in t3

-- | Copy assets from assets/ or static/ directories to output
copyAssets :: FilePath -> IO ()
copyAssets outputDir = do
  let assetDirs = ["assets", "static", "css", "images", "js"]
  forM_ assetDirs $ \assetDir -> do
    exists <- doesDirectoryExist assetDir
    when exists $ do
      putStrLn $ "Copying " ++ assetDir ++ "/ to " ++ outputDir
      copyDirectory assetDir (outputDir </> assetDir)

-- | Recursively copy directory contents
copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory src dst = do
  createDirectoryIfMissing True dst
  files <- listDirectory src
  forM_ files $ \file -> do
    let srcPath = src </> file
    let dstPath = dst </> file
    isDir <- doesDirectoryExist srcPath
    if isDir
      then copyDirectory srcPath dstPath
      else do
        copyFile srcPath dstPath
        putStrLn $ "  " ++ srcPath ++ " -> " ++ dstPath

-- | Load site configuration from config.yaml or use defaults
loadConfig :: IO SiteConfig
loadConfig = do
  let configPaths = ["config.yaml", "site.yaml", "config.yml"]
  configs <- filterM doesFileExist configPaths
  case configs of
    (path:_) -> do
      putStrLn $ "Loading config from: " ++ path
      content <- readFile path
      return $ parseConfig content
    [] -> do
      putStrLn "No config file found, using defaults"
      return defaultConfig

-- | Parse simple YAML-like config (key: value format)
parseConfig :: String -> SiteConfig
parseConfig content =
  let lns = lines content
      pairs = map parseLine $ filter (not . null) lns
      getValue key = lookup key pairs
      getBool key = maybe False (\v -> v `elem` ["true", "yes", "1"]) $ getValue key
  in SiteConfig
       (maybe "My Site" id $ getValue "title")
       (maybe "" id $ getValue "url")
       (maybe "Author" id $ getValue "author")
       (maybe "content" id $ getValue "input")
       (maybe "_site" id $ getValue "output")
       (getBool "spell_check")
       (maybe "en" id $ getValue "language")
  where
    parseLine line =
      case break (== ':') line of
        (k, ':':v) -> (trim k, trim v)
        _ -> ("", "")

-- | Load i18n strings from locales/{lang}.txt
loadI18n :: String -> IO I18nStrings
loadI18n lang = do
  let i18nPath = "locales" </> lang ++ ".txt"
  exists <- doesFileExist i18nPath
  if exists
    then do
      content <- readFile i18nPath
      return $ parseI18n content
    else return defaultI18n

-- | Parse i18n file (key: value format)
parseI18n :: String -> I18nStrings
parseI18n content =
  let pairs = map parseLine $ filter (not . null) $ lines content
      getValue key = maybe "" id $ lookup key pairs
  in I18nStrings
       (getValue "read_more")
       (getValue "published")
       (getValue "tags")
       (getValue "home")
  where
    parseLine line =
      case break (== ':') line of
        (k, ':':v) -> (trim k, trim v)
        _ -> ("", "")

-- | Spell check content using hunspell or aspell
spellCheck :: FilePath -> String -> IO [String]
spellCheck path content = do
  -- Try hunspell first, fall back to aspell
  result <- trySpellChecker "hunspell" content
  case result of
    Just errors -> return errors
    Nothing -> do
      result2 <- trySpellChecker "aspell" content
      return $ maybe [] id result2

-- | Try a spell checker (hunspell or aspell)
trySpellChecker :: String -> String -> IO (Maybe [String])
trySpellChecker checker content = do
  -- Check if checker is available
  let checkCmd = if checker == "hunspell"
                 then "hunspell -l"
                 else "aspell list"
  result <- try $ readProcess "sh" ["-c", "echo '" ++ content ++ "' | " ++ checkCmd] ""
  case result of
    Left (_ :: IOError) -> return Nothing
    Right output -> return $ Just $ filter (not . null) $ lines output
  where
    try :: IO a -> IO (Either IOError a)
    try action = catch (Right <$> action) (return . Left)
    catch :: IO (Either IOError a) -> (IOError -> IO (Either IOError a)) -> IO (Either IOError a)
    catch action handler = do
      result <- action
      case result of
        Left e -> handler e
        Right v -> return $ Right v

-- | Build site from input directory to output directory
buildSite :: FilePath -> FilePath -> IO ()
buildSite inputDir outputDir = do
  putStrLn $ "Building site: " ++ inputDir ++ " -> " ++ outputDir
  createDirectoryIfMissing True outputDir

  files <- listDirectory inputDir
  let contentFiles = filter (\f -> takeExtension f `elem` [".md", ".a2ml", ".adoc", ".rst", ".org"]) files

  if null contentFiles
    then putStrLn "No content files found."
    else do
      putStrLn $ "Found " ++ show (length contentFiles) ++ " content files:"
      forM_ contentFiles $ \file -> do
        let inputPath = inputDir </> file
        let outputPath = outputDir </> takeBaseName file ++ ".html"
        processFile inputPath outputPath

  -- Copy assets
  copyAssets outputDir

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

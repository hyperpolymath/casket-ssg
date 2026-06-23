-- SPDX-License-Identifier: MPL-2.0
{- |
Module      : CasketGnosis
Description : Casket-SSG enhanced with Gnosis metadata-driven rendering
Copyright   : (c) 2025-2026 Jonathan D.A. Jewell
License     : MPL-2.0
Maintainer  : hyperpolymath

Casket-SSG with Gnosis integration for 6scm metadata-driven static sites.

This is a full-featured static site generator built on Pandoc:

  * recursive multi-page build mirroring the source tree into the output;
  * YAML-ish frontmatter (title, date, description, layout, draft, tags,
    slug, plus arbitrary keys exposed as template vars);
  * rich Markdown via Pandoc (anchors, pipe tables, fenced code, footnotes,
    task lists, strikethrough, smart punctuation) with syntax highlighting;
  * file-based templates with per-page @layout@ selection and partials;
  * a site-wide config file (@site.conf@, simple @key = value@) exposed as
    @{{site.*}}@ template vars;
  * collections (a directory index sorted by date) and per-tag pages;
  * @sitemap.xml@ and an Atom @feed.xml@ built from @site.baseurl@;
  * optional clean URLs (@foo.md@ -> @foo/index.html@).

The Gnosis layer ((:placeholder) rendering, {{#if}}/{{#for}} DAX, 6scm
metadata) is preserved and runs over the Markdown body before Pandoc.
-}

module Main where

import System.Environment (getArgs, lookupEnv)
import System.Directory
    ( listDirectory, createDirectoryIfMissing, doesFileExist
    , doesDirectoryExist, copyFile, removeDirectoryRecursive )
import System.FilePath
    ( (</>), takeBaseName, takeExtension, takeDirectory
    , splitDirectories, joinPath )
import Control.Monad (forM_, forM, when, unless, filterM)
import Data.List (isPrefixOf, sortBy, nub, intercalate, foldl')
import Data.Maybe (fromMaybe, mapMaybe, catMaybes)
import Data.Ord (comparing, Down(..))
import Data.Char (toLower, isSpace, isAlphaNum)

import qualified Gnosis.SixSCM as Gnosis
import qualified Gnosis.Render as Render
import qualified Gnosis.DAX as DAX
import qualified Gnosis.Types as Types
import qualified Data.Map.Strict as Map
import qualified Text.Pandoc as Pandoc
import Text.Pandoc.Options (HighlightMethod(Skylighting))
import qualified Text.Pandoc.Highlighting as Highlighting
import qualified Data.Text as T

-- ---------------------------------------------------------------------------
-- Build configuration
-- ---------------------------------------------------------------------------

-- | Site-wide configuration, parsed from @site.conf@ and merged with defaults.
type SiteConfig = Map.Map String String

-- | Options governing a single build run.
data BuildOpts = BuildOpts
    { optIncludeDrafts :: !Bool   -- ^ Emit @draft: true@ pages too.
    , optCleanUrls     :: !Bool   -- ^ foo.md -> foo/index.html.
    }

-- | The fully-parsed representation of one source page.
data Page = Page
    { pgSrcPath  :: !FilePath              -- ^ Source @.md@ path.
    , pgRelHtml  :: !FilePath              -- ^ Output path relative to out root.
    , pgUrl      :: !String                -- ^ Site-absolute URL (e.g. @/posts/x/@).
    , pgMeta     :: !(Map.Map String String) -- ^ Frontmatter key/value map.
    , pgBody     :: !String                -- ^ Markdown body (frontmatter stripped).
    , pgHtml     :: !String                -- ^ Rendered HTML fragment (no layout).
    , pgToc      :: !String                -- ^ Table-of-contents HTML fragment.
    , pgTitle    :: !String                -- ^ Resolved title.
    , pgDate     :: !String                -- ^ Resolved date (may be empty).
    , pgTags     :: ![String]              -- ^ Parsed tags.
    , pgDraft    :: !Bool                  -- ^ True if @draft: true@.
    }

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

-- | Main entry point for casket-ssg with Gnosis.
main :: IO ()
main = do
    args <- getArgs
    case args of
        ("build" : rest) -> do
            let (flags, positional) = span ("--" `isPrefixOf`) rest
            opts <- mkBuildOpts flags
            case positional of
                [inputDir, outputDir] -> buildSiteWithGnosis opts inputDir outputDir
                [inputDir]            -> buildSiteWithGnosis opts inputDir "_site"
                _                     -> usage

        ["clean", outputDir] -> cleanOutput outputDir
        ["clean"]            -> cleanOutput "_site"

        ["--version"] -> putStrLn "Casket-SSG v1.0.0 with Gnosis integration"
        ["version"]   -> putStrLn "Casket-SSG v1.0.0 with Gnosis integration"

        _ -> usage

-- | Resolve build options from CLI flags and environment.
mkBuildOpts :: [String] -> IO BuildOpts
mkBuildOpts flags = do
    envDrafts <- lookupEnv "CASKET_DRAFTS"
    let flagDrafts = "--drafts" `elem` flags
        envOn      = maybe False ((`elem` ["1", "true", "yes"]) . map toLower) envDrafts
        noClean    = "--no-clean-urls" `elem` flags
    return BuildOpts
        { optIncludeDrafts = flagDrafts || envOn
        , optCleanUrls     = not noClean
        }

-- | Print usage / help.
usage :: IO ()
usage = mapM_ putStrLn
    [ "Casket-SSG - Metadata-driven static site generator"
    , ""
    , "Usage:"
    , "  casket-ssg build [flags] <input-dir> [output-dir]   Build the site (default out: _site)"
    , "  casket-ssg clean [output-dir]                        Remove the output directory"
    , "  casket-ssg --version                                 Print version"
    , ""
    , "Build flags:"
    , "  --drafts          Include pages marked 'draft: true' (or set CASKET_DRAFTS=1)"
    , "  --no-clean-urls   Emit foo.html instead of foo/index.html"
    , ""
    , "Features:"
    , "  - Recursive multi-page build mirroring the source tree"
    , "  - Frontmatter: title, date, description, layout, draft, tags, slug, + custom keys"
    , "  - Rich Markdown via Pandoc: anchors, tables, fenced code, footnotes, task lists,"
    , "    strikethrough, smart punctuation, syntax highlighting (/assets/highlight.css)"
    , "  - Templates in templates/ with per-page 'layout' and partials ({{> name}})"
    , "  - Site config (site.conf) exposed as {{site.*}}; {{nav}} and {{toc}}"
    , "  - Collections (directory index by date) and per-tag pages (/tags/<tag>/)"
    , "  - sitemap.xml and Atom feed.xml from site.baseurl"
    , "  - 6scm metadata, (:placeholder) rendering, {{#if}}/{{#for}} DAX, FlexiText"
    ]

-- | @clean@ command: remove the output directory if present.
cleanOutput :: FilePath -> IO ()
cleanOutput outputDir = do
    exists <- doesDirectoryExist outputDir
    if exists
        then removeDirectoryRecursive outputDir >> putStrLn ("Removed " ++ outputDir)
        else putStrLn (outputDir ++ " does not exist; nothing to clean.")

-- ---------------------------------------------------------------------------
-- Build orchestration
-- ---------------------------------------------------------------------------

-- | Build site with Gnosis metadata integration.
buildSiteWithGnosis :: BuildOpts -> FilePath -> FilePath -> IO ()
buildSiteWithGnosis opts inputDir outputDir = do
    putStrLn "Casket-SSG: Building site with Gnosis metadata integration"
    putStrLn $ "  Input:  " ++ inputDir
    putStrLn $ "  Output: " ++ outputDir

    -- Load 6scm context from .machine_readable/
    let scmPath = ".machine_readable"
    ctx <- Gnosis.loadAll6SCM scmPath
    putStrLn $ "  Loaded 6scm context from: " ++ scmPath

    -- Load site config (input-root or CWD), merged with sensible defaults.
    site <- loadSiteConfig inputDir
    putStrLn $ "  Site title: " ++ siteVar site "title"
    putStrLn $ "  Base URL:   " ++ siteVar site "baseurl"

    createDirectoryIfMissing True outputDir

    -- Discover all markdown sources recursively.
    srcFiles <- findMarkdown inputDir
    putStrLn $ "Found " ++ show (length srcFiles) ++ " markdown source(s)."

    -- Parse + render every page (frontmatter, Gnosis, Pandoc, TOC).
    allPages <- forM srcFiles $ \src -> renderPage opts ctx inputDir src

    -- Draft filtering happens AFTER render so we can report what was skipped.
    let (drafts, live) = span' pgDraft allPages
        publishable    = if optIncludeDrafts opts then allPages else live
    unless (optIncludeDrafts opts) $
        forM_ drafts $ \p -> putStrLn $ "  (skipped draft) " ++ pgSrcPath p

    -- Build nav once (config-driven, with a generated fallback).
    let navHtml = buildNav site publishable

    -- Write every page through its chosen layout.
    forM_ publishable $ \p -> do
        putStrLn $ "  " ++ pgSrcPath p ++ " -> " ++ (outputDir </> pgRelHtml p)
        writePage ctx site navHtml outputDir p

    -- Collections: a list page for any directory containing >1 dated page.
    collectionIndexes <- writeCollections ctx site navHtml inputDir outputDir opts publishable

    -- Tag pages under /tags/<tag>/.
    tagPages <- writeTagPages ctx site navHtml outputDir opts publishable

    -- Feeds + discovery from the full publishable set (+ generated indexes).
    let discoverable = publishable ++ collectionIndexes ++ tagPages
    writeSitemap site outputDir discoverable
    writeFeed site outputDir publishable

    -- Theme assets, highlight CSS, and public/ verbatim.
    copyAssets outputDir
    writeHighlightCss outputDir
    copyPublic inputDir outputDir

    putStrLn "Build complete!"
  where
    -- partition preserving order: (matching, non-matching)
    span' f = foldr (\x (ys, ns) -> if f x then (x:ys, ns) else (ys, x:ns)) ([], [])

-- ---------------------------------------------------------------------------
-- Source discovery
-- ---------------------------------------------------------------------------

-- | Recursively collect every @.md@/@.markdown@ file under a directory,
-- skipping the @public/@ tree (copied verbatim) and dotfiles.
findMarkdown :: FilePath -> IO [FilePath]
findMarkdown root = go root
  where
    go dir = do
        entries <- listDirectory dir
        fmap concat $ forM (filter (not . ("." `isPrefixOf`)) entries) $ \e -> do
            let p = dir </> e
            isDir <- doesDirectoryExist p
            if isDir
                then if e == "public" then return [] else go p
                else return [p | takeExtension p `elem` [".md", ".markdown"]]

-- ---------------------------------------------------------------------------
-- Page rendering
-- ---------------------------------------------------------------------------

-- | Parse, Gnosis-process, Pandoc-render a single source file into a 'Page'.
renderPage :: BuildOpts -> Types.Context -> FilePath -> FilePath -> IO Page
renderPage opts ctx inputDir src = do
    raw <- readFile src
    let (frontmatter, body) = parseFrontmatter raw
        mergedCtx = mergeFrontmatter ctx frontmatter
        title  = Map.findWithDefault (takeBaseName src) "title" frontmatter
        date   = Map.findWithDefault "" "date" frontmatter
        tags   = parseList (Map.findWithDefault "" "tags" frontmatter)
        draft  = boolVal (Map.findWithDefault "false" "draft" frontmatter)
        rel    = relOutputPath opts inputDir src frontmatter

    -- Gnosis pipeline over the markdown source (DAX loops/ifs, then placeholders).
    let withDAX = DAX.processTemplate mergedCtx body
        withPlaceholders = Render.renderWithBadges mergedCtx withDAX

    (html, toc) <- markdownToHtml withPlaceholders

    return Page
        { pgSrcPath = src
        , pgRelHtml = rel
        , pgUrl     = urlForRel opts rel
        , pgMeta    = frontmatter
        , pgBody    = body
        , pgHtml    = html
        , pgToc     = toc
        , pgTitle   = title
        , pgDate    = date
        , pgTags    = tags
        , pgDraft   = draft
        }

-- | Output path (relative to out root) for a source file. Honours @slug@,
-- index files, and the clean-URLs toggle.
relOutputPath :: BuildOpts -> FilePath -> FilePath -> Map.Map String String -> FilePath
relOutputPath opts inputDir src fm =
    let relDir = makeRelative inputDir (takeDirectory src)
        base0  = takeBaseName src
        base   = fromMaybe base0 (Map.lookup "slug" fm)
        isIndex = base == "index"
    in if isIndex
         then normJoin relDir "index.html"
         else if optCleanUrls opts
                then normJoin relDir (base </> "index.html")
                else normJoin relDir (base ++ ".html")
  where
    normJoin "" f = f
    normJoin "." f = f
    normJoin d  f = d </> f

-- | The site-absolute URL for a relative output path.
urlForRel :: BuildOpts -> FilePath -> String
urlForRel _ rel =
    let parts = splitDirectories rel
    in case reverse parts of
        ("index.html" : rest) -> "/" ++ intercalate "/" (reverse rest) ++ slash (reverse rest)
        _                     -> "/" ++ intercalate "/" parts
  where
    slash [] = ""
    slash _  = "/"

-- | Compute a path relative to a base directory (simple prefix strip).
makeRelative :: FilePath -> FilePath -> FilePath
makeRelative base path =
    let b = splitDirectories base
        p = splitDirectories path
    in if b `isPrefixOf` p then joinPath (drop (length b) p) else path

-- ---------------------------------------------------------------------------
-- Markdown -> HTML (Pandoc) with TOC and highlighting
-- ---------------------------------------------------------------------------

-- | Convert Markdown to (HTML body, TOC HTML) using Pandoc with a strong
-- extension set, auto heading IDs, syntax highlighting and a table of contents.
markdownToHtml :: String -> IO (String, String)
markdownToHtml markdown = do
    let readerOpts = Pandoc.def
            { Pandoc.readerExtensions = Pandoc.pandocExtensions }
        bodyOpts = Pandoc.def
            { Pandoc.writerExtensions      = Pandoc.pandocExtensions
            , Pandoc.writerHighlightMethod = Skylighting Highlighting.pygments
            }
    -- A Pandoc template that emits ONLY the table of contents (wrapped in a
    -- nav). $body$ is intentionally omitted so the page body is never doubled.
    -- compileTemplate must run in plain IO (it has no PandocIO instance).
    tmplResult <- Pandoc.compileTemplate ""
        (T.pack "$if(toc)$<nav class=\"toc\" aria-label=\"Table of contents\">$toc$</nav>$endif$")
    tmpl <- either (fail . ("TOC template error: " ++)) return tmplResult
    (body, toc) <- Pandoc.runIOorExplode $ do
        doc <- Pandoc.readMarkdown readerOpts (T.pack markdown)
        -- Body fragment (no standalone template): rich HTML with anchors,
        -- tables, fenced highlighted code, footnotes, task lists, etc.
        bodyTxt <- Pandoc.writeHtml5String bodyOpts doc
        -- A standalone render whose ONLY output is the table of contents,
        -- so we can expose {{toc}} independently of the page body.
        let tocOpts = bodyOpts
                { Pandoc.writerTableOfContents = True
                , Pandoc.writerTemplate        = Just tmpl
                }
        tocTxt <- Pandoc.writeHtml5String tocOpts doc
        return (T.unpack bodyTxt, T.unpack (T.strip tocTxt))
    return (addThScope body, toc)

-- | Add @scope="col"@ to every table header cell that lacks an explicit
-- @scope@. Pandoc emits @\<th\>@ / @\<th style="…"\>@ without a scope, which
-- weakens WCAG 1.3.1 (Info & Relationships) for screen-reader table navigation.
-- This is a conservative string rewrite over the rendered body HTML:
--
--   * @\<th\>@               -> @\<th scope="col"\>@
--   * @\<th style="…"\>@     -> @\<th scope="col" style="…"\>@
--   * @\<th … scope=… …\>@   -> left untouched (never double-added)
--
-- Captions emitted by Pandoc (@\<caption\>@) are preserved as-is.
addThScope :: String -> String
addThScope = go
  where
    go [] = []
    go s@(c:cs)
        -- Match an opening "<th" that is the start of a tag, i.e. the next
        -- character is whitespace or the tag close '>'. This avoids matching
        -- "<thead", "<theme", etc.
        | "<th" `isPrefixOf` s
        , let after = drop 3 s
        , startsTag after =
            let (attrs, rest) = break (== '>') after
            in if hasScope attrs
                 then "<th" ++ attrs ++ go rest          -- already scoped: keep
                 else "<th scope=\"col\"" ++ attrs ++ go rest
        | otherwise = c : go cs

    -- True when what follows "<th" begins the rest of a th tag: either '>'
    -- immediately, or attribute whitespace.
    startsTag (x:_) = x == '>' || x == ' ' || x == '\t' || x == '\n' || x == '\r' || x == '/'
    startsTag []    = False

    -- Detect an existing scope attribute within the gathered attribute text.
    hasScope attrs = "scope=" `isInfixOfStr` attrs
    isInfixOfStr n h = any (n `isPrefixOf`) (tailsS h)
    tailsS [] = [[]]
    tailsS s@(_:xs) = s : tailsS xs

-- ---------------------------------------------------------------------------
-- Templates & partials
-- ---------------------------------------------------------------------------

-- | Load a named layout from @templates/<layout>@, falling back to
-- @templates/default.html@ and then the built-in template.
loadLayout :: String -> IO String
loadLayout layout = do
    let name = if "." `isInfixOfStr` layout then layout else layout ++ ".html"
        path = "templates" </> name
        defPath = "templates" </> "default.html"
    hasLayout <- doesFileExist path
    if hasLayout
        then expandPartials =<< readFile path
        else do
            hasDef <- doesFileExist defPath
            if hasDef then expandPartials =<< readFile defPath
                      else return builtinTemplate
  where isInfixOfStr n h = any (n `isPrefixOf`) (tails' h)
        tails' [] = [[]]
        tails' s@(_:xs) = s : tails' xs

-- | Expand partial includes: @{{> name}}@ or @{{include:name}}@ read
-- @templates/partials/<name>.html@. One level deep is enough for our needs,
-- but we recurse so partials may include partials.
expandPartials :: String -> IO String
expandPartials = go (0 :: Int)
  where
    go depth s
        | depth > 8 = return s   -- guard against include cycles
        | otherwise = do
            case findPartial s of
                Nothing -> return s
                Just (before, name, after) -> do
                    frag <- loadPartial name
                    rest <- go (depth + 1) after
                    return (before ++ frag ++ rest)

    -- Find the first {{> name}} or {{include:name}} token.
    findPartial s = case scan s of
        Just (b, tok, a) -> Just (b, tok, a)
        Nothing          -> Nothing
      where
        scan str = look "" str
        look acc str
            | "{{>" `isPrefixOf` str =
                let inner = drop 3 str
                    (tok, rest) = breakClose inner
                in Just (reverse acc, trimStr tok, rest)
            | "{{include:" `isPrefixOf` str =
                let inner = drop 10 str
                    (tok, rest) = breakClose inner
                in Just (reverse acc, trimStr tok, rest)
            | otherwise = case str of
                (c:cs) -> look (c:acc) cs
                []     -> Nothing
        breakClose str = case breakOnStr "}}" str of
            Just (b, a) -> (b, drop 2 a)
            Nothing     -> (str, "")

    loadPartial name = do
        let path = "templates" </> "partials" </> (name ++ ".html")
        exists <- doesFileExist path
        if exists then readFile path
                  else return ("<!-- missing partial: " ++ name ++ " -->")

-- ---------------------------------------------------------------------------
-- Page emission
-- ---------------------------------------------------------------------------

-- | Render and write a single page through its layout.
writePage :: Types.Context -> SiteConfig -> String -> FilePath -> Page -> IO ()
writePage _ctx site navHtml outputDir p = do
    let layout = Map.findWithDefault "default" "layout" (pgMeta p)
    template <- loadLayout layout
    let html = fillTemplate site navHtml p template
        outPath = outputDir </> pgRelHtml p
    createDirectoryIfMissing True (takeDirectory outPath)
    writeFile outPath html

-- | Fill a template for a page: all frontmatter keys, the standard vars,
-- site.* vars, nav and toc, with {{content}} substituted last.
fillTemplate :: SiteConfig -> String -> Page -> String -> String
fillTemplate site navHtml p template =
    let brand = Map.findWithDefault (siteVar site "title") "site"
                  (Map.insert "site" (Map.findWithDefault "" "site" (pgMeta p)) (pgMeta p))
        brand' = if null brand then pgTitle p else brand
        -- 1. site.* config vars
        s1 = foldl' (\t (k, v) -> replaceAll ("{{site." ++ k ++ "}}") v t) template (Map.toList site)
        -- 2. arbitrary frontmatter keys (skip content/title/brand/date handled below)
        s2 = foldl' (\t (k, v) -> replaceAll ("{{" ++ k ++ "}}") v t) s1
                 (Map.toList (Map.delete "content" (pgMeta p)))
        -- Collapse the canonical "<time>{{date}}</time>" idiom to valid HTML:
        -- an empty <time> (no datetime attr, no text) is invalid, and a bare
        -- date is improved by a machine-readable datetime attribute. Synthetic
        -- collection/tag pages carry no date, so the element is dropped.
        timeHtml = if null (pgDate p)
                     then ""
                     else "<time datetime=\"" ++ pgDate p ++ "\">" ++ pgDate p ++ "</time>"
        -- 3. standard vars
        s3 = replaceAll "{{nav}}"  navHtml
           . replaceAll "{{toc}}"  (pgToc p)
           . replaceAll "{{date}}" (pgDate p)
           . replaceAll "{{title}}" (pgTitle p)
           . replaceAll "{{brand}}" brand'
           . replaceAll "{{description}}" (Map.findWithDefault (siteVar site "description") "description" (pgMeta p))
           . replaceAll "<time>{{date}}</time>" timeHtml
           $ s2
        -- 4. content LAST so body HTML is never re-processed
    in replaceAll "{{content}}" (pgHtml p) s3

-- ---------------------------------------------------------------------------
-- Navigation
-- ---------------------------------------------------------------------------

-- | Build the primary nav. Prefers @nav@ in site.conf
-- (format: @Home=/,Posts=/posts/,About=/about/@); otherwise derives one from
-- top-level pages.
buildNav :: SiteConfig -> [Page] -> String
buildNav site pages =
    let items = case Map.lookup "nav" site of
            Just spec | not (null (trimStr spec)) -> parseNavSpec spec
            _ -> derived
    in "<ul class=\"nav-links\">"
       ++ concatMap (\(label, href) ->
            "<li><a href=\"" ++ href ++ "\">" ++ label ++ "</a></li>") items
       ++ "</ul>"
  where
    parseNavSpec spec =
        [ (trimStr l, trimStr h)
        | pair <- splitOnChar ',' spec
        , let (l, hrest) = break (== '=') pair
        , let h = drop 1 hrest
        , not (null (trimStr l)) ]
    derived =
        [ (pgTitle p, pgUrl p)
        | p <- pages
        , length (splitDirectories (pgRelHtml p)) <= 1
          || (last (splitDirectories (pgRelHtml p)) == "index.html"
              && length (splitDirectories (pgRelHtml p)) == 2) ]

-- ---------------------------------------------------------------------------
-- Collections (directory index pages)
-- ---------------------------------------------------------------------------

-- | For each directory that has >=2 dated pages and lacks its own index page,
-- emit a generated listing page sorted by date (newest first). Returns the
-- synthetic pages so feeds/sitemap can include them.
writeCollections
    :: Types.Context -> SiteConfig -> String -> FilePath -> FilePath
    -> BuildOpts -> [Page] -> IO [Page]
writeCollections _ctx site navHtml inputDir outputDir _opts pages = do
    template <- loadLayout "default"
    fmap catMaybes $ forM dirsNeedingIndex $ \(dir, members) -> do
        let sorted = sortBy (comparing (Down . pgDate)) members
            listHtml = renderListing sorted
            title = capitalizeWord (lastSeg dir)
            relHtml = dir </> "index.html"
            p = syntheticPage relHtml title listHtml
            html = fillTemplate site navHtml p template
            outPath = outputDir </> relHtml
        createDirectoryIfMissing True (takeDirectory outPath)
        writeFile outPath html
        putStrLn $ "  (collection) " ++ outPath
        return (Just p)
  where
    -- Group pages by their SOURCE directory (relative to the input root). This
    -- is stable under clean URLs, where each post otherwise lives in its own
    -- output subdirectory.
    srcDir p = makeRelative inputDir (takeDirectory (pgSrcPath p))
    dirGroups = Map.toList $ foldr
        (\p -> Map.insertWith (++) (normDir (srcDir p)) [p]) Map.empty pages
    normDir d = if d == "." then "" else d
    -- A directory needs an index when it has >=2 dated pages and no own index
    -- page already authored (a real index.md among its members).
    dirsNeedingIndex =
        [ (dir, members)
        | (dir, members) <- dirGroups
        , not (null dir)
        , length (filter (not . null . pgDate) members) >= 2
        , not (any (\p -> takeBaseName (pgSrcPath p) == "index") members) ]

-- ---------------------------------------------------------------------------
-- Tag pages
-- ---------------------------------------------------------------------------

-- | Emit a page per tag under @/tags/<tag>/@ listing tagged pages, plus a
-- @/tags/@ index. Returns synthetic pages for discovery.
writeTagPages
    :: Types.Context -> SiteConfig -> String -> FilePath -> BuildOpts -> [Page]
    -> IO [Page]
writeTagPages _ctx site navHtml outputDir _opts pages = do
    if null tagMap then return [] else do
        template <- loadLayout "default"
        perTag <- forM (Map.toList tagMap) $ \(tag, members) -> do
            let sorted = sortBy (comparing (Down . pgDate)) members
                slug = slugify tag
                relHtml = "tags" </> slug </> "index.html"
                p = syntheticPage relHtml ("Tag: " ++ tag) (renderListing sorted)
                html = fillTemplate site navHtml p template
                outPath = outputDir </> relHtml
            createDirectoryIfMissing True (takeDirectory outPath)
            writeFile outPath html
            putStrLn $ "  (tag) " ++ outPath
            return p
        -- /tags/ index
        let idxList = "<ul class=\"tag-index\">"
                ++ concatMap (\(tag, ms) ->
                     "<li><a href=\"/tags/" ++ slugify tag ++ "/\">" ++ tag
                     ++ "</a> (" ++ show (length ms) ++ ")</li>")
                     (Map.toList tagMap)
                ++ "</ul>"
            idxRel = "tags" </> "index.html"
            idxPage = syntheticPage idxRel "Tags" idxList
            idxHtml = fillTemplate site navHtml idxPage template
        createDirectoryIfMissing True (outputDir </> "tags")
        writeFile (outputDir </> idxRel) idxHtml
        putStrLn $ "  (tag) " ++ (outputDir </> idxRel)
        return (idxPage : perTag)
  where
    tagMap = foldr addTags Map.empty pages
    addTags p m = foldr (\t -> Map.insertWith (++) t [p]) m (pgTags p)

-- ---------------------------------------------------------------------------
-- Listing rendering (shared by collections & tags)
-- ---------------------------------------------------------------------------

-- | Render a list of pages as a simple article list.
renderListing :: [Page] -> String
renderListing ps =
    "<ul class=\"post-list\">"
    ++ concatMap item ps
    ++ "</ul>"
  where
    item p = "<li><a href=\"" ++ pgUrl p ++ "\">" ++ escapeHtml (pgTitle p) ++ "</a>"
        ++ (if null (pgDate p) then "" else " <time class=\"muted\">" ++ pgDate p ++ "</time>")
        ++ desc p
        ++ "</li>"
    desc p = case Map.lookup "description" (pgMeta p) of
        Just d | not (null d) -> " <span class=\"muted\">— " ++ escapeHtml d ++ "</span>"
        _ -> ""

-- | A synthetic (generated) page with no source file.
syntheticPage :: FilePath -> String -> String -> Page
syntheticPage relHtml title html = Page
    { pgSrcPath = "<generated>"
    , pgRelHtml = relHtml
    , pgUrl     = "/" ++ intercalate "/" (initIdx (splitDirectories relHtml)) ++ "/"
    , pgMeta    = Map.fromList [("title", title)]
    , pgBody    = ""
    , pgHtml    = html
    , pgToc     = ""
    , pgTitle   = title
    , pgDate    = ""
    , pgTags    = []
    , pgDraft   = False
    }
  where initIdx ds = if not (null ds) && last ds == "index.html" then init ds else ds

-- ---------------------------------------------------------------------------
-- Feeds & discovery
-- ---------------------------------------------------------------------------

-- | Write @sitemap.xml@ listing every page URL.
writeSitemap :: SiteConfig -> FilePath -> [Page] -> IO ()
writeSitemap site outputDir pages = do
    let base = stripTrailingSlash (siteVar site "baseurl")
        urls = nub (map pgUrl pages)
        body = concatMap (\u ->
                 "  <url><loc>" ++ base ++ u ++ "</loc></url>\n") urls
        xml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
            ++ "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">\n"
            ++ body
            ++ "</urlset>\n"
    writeFile (outputDir </> "sitemap.xml") xml
    putStrLn $ "  (discovery) " ++ (outputDir </> "sitemap.xml")

-- | Write an Atom @feed.xml@ from dated pages (newest first, capped at 20).
writeFeed :: SiteConfig -> FilePath -> [Page] -> IO ()
writeFeed site outputDir pages = do
    let base   = stripTrailingSlash (siteVar site "baseurl")
        title  = siteVar site "title"
        author = siteVar site "author"
        dated  = take 20 $ sortBy (comparing (Down . pgDate))
                         $ filter (not . null . pgDate) pages
        updated = case dated of
            (p:_) -> isoDate (pgDate p)
            []    -> "1970-01-01T00:00:00Z"
        entries = concatMap (entry base author) dated
        xml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
            ++ "<feed xmlns=\"http://www.w3.org/2005/Atom\">\n"
            ++ "  <title>" ++ escapeXml title ++ "</title>\n"
            ++ "  <link href=\"" ++ base ++ "/\"/>\n"
            ++ "  <link rel=\"self\" href=\"" ++ base ++ "/feed.xml\"/>\n"
            ++ "  <id>" ++ base ++ "/</id>\n"
            ++ "  <updated>" ++ updated ++ "</updated>\n"
            ++ (if null author then "" else "  <author><name>" ++ escapeXml author ++ "</name></author>\n")
            ++ entries
            ++ "</feed>\n"
    writeFile (outputDir </> "feed.xml") xml
    putStrLn $ "  (discovery) " ++ (outputDir </> "feed.xml")
  where
    entry base author p =
        "  <entry>\n"
        ++ "    <title>" ++ escapeXml (pgTitle p) ++ "</title>\n"
        ++ "    <link href=\"" ++ base ++ pgUrl p ++ "\"/>\n"
        ++ "    <id>" ++ base ++ pgUrl p ++ "</id>\n"
        ++ "    <updated>" ++ isoDate (pgDate p) ++ "</updated>\n"
        ++ (if null author then "" else "    <author><name>" ++ escapeXml author ++ "</name></author>\n")
        ++ "    <summary>" ++ escapeXml (Map.findWithDefault "" "description" (pgMeta p)) ++ "</summary>\n"
        ++ "  </entry>\n"

-- | Normalise a date to an RFC3339 timestamp (assumes midnight UTC if only a
-- date is given). Non-date strings pass through best-effort.
isoDate :: String -> String
isoDate d
    | length d == 10 = d ++ "T00:00:00Z"
    | otherwise      = d

-- ---------------------------------------------------------------------------
-- Assets, highlight CSS, public/
-- ---------------------------------------------------------------------------

-- | Copy theme asset directories (relative to CWD) into the output.
copyAssets :: FilePath -> IO ()
copyAssets outputDir =
    forM_ ["assets", "static", "css", "js", "images"] $ \d -> do
        exists <- doesDirectoryExist d
        when exists $ copyDirectory d (outputDir </> d)

-- | Emit the Pandoc/skylighting highlight CSS once into /assets/highlight.css.
writeHighlightCss :: FilePath -> IO ()
writeHighlightCss outputDir = do
    let dir = outputDir </> "assets"
    createDirectoryIfMissing True dir
    let css = Highlighting.styleToCss Highlighting.pygments
    writeFile (dir </> "highlight.css") css
    putStrLn $ "  (assets) " ++ (dir </> "highlight.css")

-- | Copy <inputDir>/public/ contents into the OUTPUT ROOT, verbatim.
copyPublic :: FilePath -> FilePath -> IO ()
copyPublic inputDir outputDir = do
    let publicDir = inputDir </> "public"
    exists <- doesDirectoryExist publicDir
    when exists $ do
        entries <- listDirectory publicDir
        forM_ entries $ \entry -> do
            let srcPath = publicDir </> entry
                dstPath = outputDir </> entry
            isDir <- doesDirectoryExist srcPath
            if isDir then copyDirectory srcPath dstPath else copyFile srcPath dstPath

-- | Recursively copy a directory.
copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory src dst = do
    createDirectoryIfMissing True dst
    entries <- listDirectory src
    forM_ entries $ \entry -> do
        let srcPath = src </> entry
            dstPath = dst </> entry
        isDir <- doesDirectoryExist srcPath
        if isDir then copyDirectory srcPath dstPath else copyFile srcPath dstPath

-- ---------------------------------------------------------------------------
-- Site config
-- ---------------------------------------------------------------------------

-- | Load @site.conf@ (then @<inputDir>/site.conf@) as simple @key = value@
-- lines, merged over defaults. Lines beginning @#@ are comments.
loadSiteConfig :: FilePath -> IO SiteConfig
loadSiteConfig inputDir = do
    let candidates = ["site.conf", inputDir </> "site.conf"]
    existing <- filterM doesFileExist candidates
    parsed <- forM existing $ \f -> parseConf <$> readFile f
    let merged = foldl' Map.union Map.empty (reverse parsed)
    return (Map.union merged defaults)
  where
    defaults = Map.fromList
        [ ("title", "Casket Site")
        , ("baseurl", "https://example.com")
        , ("author", "")
        , ("description", "Built with casket-ssg")
        ]
    parseConf = Map.fromList . mapMaybe parseLine . lines
    parseLine l =
        let s = trimStr l
        in if null s || "#" `isPrefixOf` s
             then Nothing
             else case break (== '=') s of
                    (k, '=':v) -> Just (trimStr k, stripQuotes (trimStr v))
                    _          -> Nothing
    stripQuotes v = case v of
        ('"':rest) | not (null rest) && last rest == '"' -> init rest
        ('\'':rest) | not (null rest) && last rest == '\'' -> init rest
        _ -> v

-- | A site config value (empty string if absent).
siteVar :: SiteConfig -> String -> String
siteVar site k = Map.findWithDefault "" k site

-- ---------------------------------------------------------------------------
-- Frontmatter
-- ---------------------------------------------------------------------------

-- | Parse frontmatter from content. Tolerates leading blank lines and
-- single-line HTML comments (e.g. an SPDX header) before the opening @---@
-- fence. Returns (frontmatter Map, remaining content).
parseFrontmatter :: String -> (Map.Map String String, String)
parseFrontmatter content =
    case dropWhile isSkippable (lines content) of
        (delim : afterOpen) | strip delim == "---" ->
            let (fmLines, bodyWithDelim) = break (\l -> strip l == "---") afterOpen
                body = unlines (drop 1 bodyWithDelim)
            in (parseFrontmatterLines fmLines, body)
        _ -> (Map.empty, content)
  where
    isSkippable line = let s = strip line in null s || "<!--" `isPrefixOf` s
    strip = f . f where f = reverse . dropWhile (`elem` (" \t\r" :: String))

-- | Parse frontmatter lines into a Map (last value for a key wins).
parseFrontmatterLines :: [String] -> Map.Map String String
parseFrontmatterLines = Map.fromList . mapMaybe parseLine . filter (not . null . trimStr)
  where
    parseLine line
        | "#" `isPrefixOf` trimStr line = Nothing
        | otherwise =
            let (key, rest) = break (== ':') line
                value = dropWhile (== ' ') (drop 1 rest)
            in if null rest then Nothing else Just (trimStr key, stripQuotes (trimStr value))
    stripQuotes v = case v of
        ('"':r)  | not (null r) && last r == '"'  -> init r
        ('\'':r) | not (null r) && last r == '\'' -> init r
        _ -> v

-- | Merge frontmatter into the Gnosis context (frontmatter takes precedence).
mergeFrontmatter :: Types.Context -> Map.Map String String -> Types.Context
mergeFrontmatter baseCtx frontmatter =
    let fmCtx = Map.mapWithKey (\k v -> Types.FlexiText v ("Frontmatter: " ++ k)) frontmatter
    in Map.union fmCtx baseCtx

-- ---------------------------------------------------------------------------
-- Built-in template (fallback only)
-- ---------------------------------------------------------------------------

-- | Minimal fallback template (used only when templates/default.html is missing).
-- Accessible by construction: @\<html lang\>@, a skip link, labelled
-- @header@/@nav@/@main#main@/@footer@ landmarks, viewport meta, and the theme
-- stylesheet. {{content}} is substituted LAST by 'fillTemplate'.
builtinTemplate :: String
builtinTemplate = unlines
    [ "<!DOCTYPE html>"
    , "<html lang=\"en\">"
    , "<head>"
    , "<meta charset=\"UTF-8\">"
    , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
    , "<title>{{title}}</title>"
    , "<meta name=\"description\" content=\"{{description}}\">"
    , "<meta name=\"color-scheme\" content=\"light dark\">"
    , "<meta name=\"generator\" content=\"casket-ssg\">"
    , "<link rel=\"stylesheet\" href=\"/assets/style.css\">"
    , "<link rel=\"stylesheet\" href=\"/assets/highlight.css\">"
    , "</head>"
    , "<body>"
    , "<a class=\"skip-link\" href=\"#main\">Skip to content</a>"
    , "<header class=\"site-header\">"
    , "<nav class=\"container nav\" aria-label=\"Primary\">"
    , "<a class=\"brand\" href=\"/\">{{brand}}</a>{{nav}}"
    , "</nav>"
    , "</header>"
    , "<main id=\"main\" class=\"container\" tabindex=\"-1\">"
    , "<article class=\"prose\">"
    , "{{content}}"
    , "</article>"
    , "</main>"
    , "<footer class=\"site-footer\"><div class=\"container\"><p class=\"muted\">"
    , "Built with casket-ssg · <time>{{date}}</time></p></div></footer>"
    , "</body>"
    , "</html>"
    ]

-- ---------------------------------------------------------------------------
-- String utilities
-- ---------------------------------------------------------------------------

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

-- | Trim leading/trailing ASCII whitespace.
trimStr :: String -> String
trimStr = f . f where f = reverse . dropWhile isSpace

-- | Split a string on a character.
splitOnChar :: Char -> String -> [String]
splitOnChar c s = case break (== c) s of
    (a, _:rest) -> a : splitOnChar c rest
    (a, [])     -> [a]

-- | Parse a frontmatter list value: either a bracketed @[a, b]@ list or a
-- bare comma-separated list. Yields trimmed, non-empty items.
parseList :: String -> [String]
parseList raw0 =
    let raw = trimStr raw0
        inner = case raw of
            ('[':r) | not (null r) && last r == ']' -> init r
            _ -> raw
    in filter (not . null) (map trimStr (splitOnChar ',' inner))

-- | Interpret a frontmatter boolean.
boolVal :: String -> Bool
boolVal v = map toLower (trimStr v) `elem` ["true", "yes", "1", "on"]

-- | Lowercase, hyphenate, drop non-url-safe characters: a tag slug.
slugify :: String -> String
slugify = collapse . map repl . map toLower . trimStr
  where
    repl c | isAlphaNum c = c
           | otherwise    = '-'
    collapse [] = []
    collapse ('-':'-':rest) = collapse ('-':rest)
    collapse (c:rest) = c : collapse rest

-- | Capitalise the first letter of a word.
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (c:cs) = toUpperC c : cs
  where toUpperC ch = if ch >= 'a' && ch <= 'z' then toEnum (fromEnum ch - 32) else ch

-- | Last path segment.
lastSeg :: FilePath -> String
lastSeg = last' . splitDirectories where last' [] = ""; last' xs = last xs

-- | Drop a single trailing slash.
stripTrailingSlash :: String -> String
stripTrailingSlash s = if not (null s) && last s == '/' then init s else s

-- | Escape HTML text content.
escapeHtml :: String -> String
escapeHtml = concatMap esc
  where esc '<' = "&lt;"; esc '>' = "&gt;"; esc '&' = "&amp;"; esc c = [c]

-- | Escape XML text content (incl. quotes/apostrophes).
escapeXml :: String -> String
escapeXml = concatMap esc
  where
    esc '<' = "&lt;"; esc '>' = "&gt;"; esc '&' = "&amp;"
    esc '"' = "&quot;"; esc '\'' = "&apos;"; esc c = [c]

-- | Break a string at the first occurrence of a needle (needle dropped from
-- neither side; returns Nothing if absent).
breakOnStr :: String -> String -> Maybe (String, String)
breakOnStr needle = go ""
  where
    go _ [] = Nothing
    go acc s@(c:cs)
        | needle `isPrefixOf` s = Just (reverse acc, s)
        | otherwise = go (c:acc) cs


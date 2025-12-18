;;; ROADMAP.scm — casket-ssg Development Roadmap
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;; Updated: 2025-12-18

(define roadmap-metadata
  '((project . "casket-ssg")
    (language . "Haskell")
    (last-updated . "2025-12-18")
    (maintainer . "hyperpolymath")))

;;; ============================================================================
;;; COMPLETED MILESTONES
;;; ============================================================================

(define completed-milestones
  '((v1.0-core
     (title . "Core Haskell SSG Engine")
     (status . "complete")
     (delivered
      ("Pure functional markdown parser with state machine approach")
      ("YAML-style frontmatter parsing (title, date, tags, draft, template)")
      ("Inline formatting: bold, italic, inline code")
      ("Block elements: headers H1-H6, lists, code fences, paragraphs")
      ("HTML entity escaping for security")
      ("Template engine with variable substitution")
      ("Built-in test suite (test-markdown, test-frontmatter, test-full)")))

    (v1.0-security
     (title . "Security & Compliance Infrastructure")
     (status . "complete")
     (delivered
      ("SECURITY.md with coordinated disclosure policy")
      ("security.txt in .well-known/ directory")
      ("AIBDP manifest for AI boundary declaration")
      ("Provenance tracking with consent framework")
      ("CodeQL workflow for JavaScript/TypeScript analysis")
      ("SHA-pinned GitHub Actions for RSR compliance")
      ("Language enforcement in CI (Haskell-only src/)")))

    (v1.0-ci
     (title . "CI/CD Pipeline")
     (status . "complete")
     (delivered
      ("Haskell CI with GHC 9.4 and Cabal 3.10")
      ("Automated build and test")
      ("Dependabot for GitHub Actions and npm dependencies")
      ("Language purity checks")))))

;;; ============================================================================
;;; CURRENT PHASE: v1.1 - MCP Integration
;;; ============================================================================

(define current-phase
  '((version . "v1.1")
    (title . "MCP Hub Integration")
    (status . "in-progress")
    (completion . 70)
    (tasks
     ((task . "ReScript MCP adapter scaffolding")
      (status . "complete")
      (notes . "Adapter structure in adapters/src/CasketAdapter.res"))

     ((task . "MCP tool definitions")
      (status . "complete")
      (notes . "casket_build, casket_clean, casket_version defined"))

     ((task . "Integration testing with poly-ssg-mcp hub")
      (status . "pending")
      (priority . "high"))

     ((task . "Error handling and recovery")
      (status . "pending")
      (priority . "medium")))))

;;; ============================================================================
;;; FUTURE ROADMAP
;;; ============================================================================

(define future-phases
  '((v1.2-features
     (title . "Extended Features")
     (priority . "high")
     (tasks
      ("File watching for development mode (inotify/fswatch)")
      ("RSS/Atom feed generation")
      ("Sitemap.xml generation")
      ("Custom template loading from file")
      ("Table markdown support")
      ("Image handling with lazy loading attributes")))

    (v1.3-performance
     (title . "Performance & Scalability")
     (priority . "medium")
     (tasks
      ("Incremental builds (only changed files)")
      ("Parallel file processing")
      ("Build caching with content hashes")
      ("Memory-efficient streaming for large sites")))

    (v1.4-ecosystem
     (title . "Ecosystem Integration")
     (priority . "medium")
     (tasks
      ("Nix flake for reproducible builds")
      ("Docker container image")
      ("Homebrew formula")
      ("Stack support alongside Cabal")))

    (v2.0-advanced
     (title . "Advanced Features")
     (priority . "low")
     (tasks
      ("Plugin system for extensibility")
      ("Syntax highlighting for code blocks")
      ("Asset pipeline (CSS/JS minification)")
      ("Multi-language/i18n support")
      ("Full Hakyll compatibility layer")))))

;;; ============================================================================
;;; TECHNICAL DEBT & MAINTENANCE
;;; ============================================================================

(define maintenance-items
  '((ongoing
     ("Keep GitHub Actions SHA pins updated via Dependabot")
     ("Monitor GHC compatibility with new releases")
     ("Security advisory monitoring"))

    (technical-debt
     ("Add HLint configuration for code style")
     ("Add property-based testing with QuickCheck")
     ("Add Haddock documentation"))))

;;; ============================================================================
;;; NON-GOALS (Explicitly Out of Scope)
;;; ============================================================================

(define non-goals
  '(("Rewriting in any language other than Haskell")
    ("GUI or web-based editor")
    ("Database backend")
    ("Server-side rendering / dynamic content")
    ("CMS functionality")))

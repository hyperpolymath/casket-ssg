;;; STATE.scm — casket-ssg Project State (44-Component Status)
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;; Updated: 2025-12-22

(define-module (casket-ssg state)
  #:export (metadata language-enforcement component-status state-summary))

;;; ============================================================================
;;; METADATA
;;; ============================================================================

(define metadata
  '((version . "1.0.0")
    (updated . "2025-12-18")
    (project . "casket-ssg")
    (required-language . "Haskell")
    (component-count . 44)
    (components-complete . 38)))

;;; ============================================================================
;;; LANGUAGE ENFORCEMENT (CRITICAL)
;;; ============================================================================

(define language-enforcement
  '((primary-language . "Haskell")
    (file-extension . ".hs")
    (interpreter . "GHC 9.0+")
    (build-tool . "Cabal 3.0+")
    (forbidden-languages . ("Python" "JavaScript" "TypeScript" "Ruby" "Go" "Rust"))
    (rationale . "casket-ssg is the DEFINITIVE Haskell static site generator.")
    (enforcement . "absolute")
    (exception . "ReScript in adapters/ for MCP protocol only")))

;;; ============================================================================
;;; 44-COMPONENT STATUS (Adapted for Haskell SSG)
;;; ============================================================================

(define component-status
  '((summary
     (total . 44)
     (complete . 38)
     (in-progress . 4)
     (pending . 2)
     (percentage . 86))

    ;; =========================================================================
    ;; 1. CORE ENGINE (4/4 Complete)
    ;; =========================================================================
    (core-engine
     (count . "4/4")
     (status . "complete")
     (components
      ((component . "Haskell Engine")
       (location . "src/Casket.hs")
       (status . "complete"))
      ((component . "Markdown Parser")
       (location . "src/Casket.hs:104-198")
       (status . "complete"))
      ((component . "Frontmatter Parser")
       (location . "src/Casket.hs:73-101")
       (status . "complete"))
      ((component . "Template Engine")
       (location . "src/Casket.hs:200-229")
       (status . "complete"))))

    ;; =========================================================================
    ;; 2. BUILD SYSTEM (4/4 Complete)
    ;; =========================================================================
    (build-system
     (count . "4/4")
     (status . "complete")
     (components
      ((component . "Justfile")
       (location . "Justfile")
       (commands . ("build" "test" "test-all" "lint" "fmt" "release"))
       (status . "complete"))
      ((component . "Mustfile")
       (location . "Mustfile")
       (status . "complete"))
      ((component . "Cabal Config")
       (location . "casket-ssg.cabal")
       (status . "complete"))
      ((component . "Build Scripts")
       (location . "Justfile")
       (status . "complete"))))

    ;; =========================================================================
    ;; 3. SITE GENERATION (4/4 Complete)
    ;; =========================================================================
    (site-generation
     (count . "4/4")
     (status . "complete")
     (components
      ((component . "Content Processing")
       (description . "YAML frontmatter + Markdown")
       (status . "complete"))
      ((component . "Template Engine")
       (description . "{{variable}} substitution")
       (status . "complete"))
      ((component . "Output Generation")
       (description . "HTML files")
       (status . "complete"))
      ((component . "Content Schema")
       (description . "Frontmatter ADT")
       (status . "complete"))))

    ;; =========================================================================
    ;; 4. ADAPTERS (3/3 Complete)
    ;; =========================================================================
    (adapters
     (count . "3/3")
     (status . "complete")
     (components
      ((component . "MCP Server Interface")
       (location . "adapters/src/CasketAdapter.res")
       (language . "ReScript")
       (status . "complete"))
      ((component . "ReScript Adapter")
       (location . "adapters/")
       (status . "complete"))
      ((component . "Tool Definitions")
       (tools . ("casket_build" "casket_clean" "casket_version"))
       (status . "complete"))))

    ;; =========================================================================
    ;; 5. TESTING (4/4 Complete)
    ;; =========================================================================
    (testing
     (count . "4/4")
     (status . "complete")
     (components
      ((component . "Unit Tests")
       (location . "src/Casket.hs:231-307")
       (status . "complete"))
      ((component . "Markdown Tests")
       (command . "test-markdown")
       (status . "complete"))
      ((component . "Frontmatter Tests")
       (command . "test-frontmatter")
       (status . "complete"))
      ((component . "Integration Tests")
       (command . "test-full")
       (status . "complete"))))

(define current-position
  '((phase . "v1.0 - Core Complete, Adapter In Progress")
    (overall-completion . 85)
    (components ((Haskell-engine ((status . "complete") (completion . 100)))
                 (mcp-adapter ((status . "scaffolded") (language . "ReScript") (completion . 70)))
                 (ci-pipeline ((status . "complete") (completion . 100)))
                 (security-policy ((status . "complete") (completion . 100)))))))

(define blockers-and-issues
  '((critical ())
    (high-priority ())
    (medium-priority
     (("Add SPDX header to Casket.hs" . "compliance")))))

;;; ============================================================================
;;; CRITICAL NEXT ACTIONS
;;; ============================================================================

(define critical-next-actions
  '((immediate (("Complete MCP adapter integration tests" . high)
                ("Add file watching capability" . medium)
                ("Implement RSS feed generation" . medium)))))

(define state-summary
  '((project . "casket-ssg")
    (language . "Haskell")
    (completion . 85)
    (blockers . 0)
    (updated . "2025-12-18")))

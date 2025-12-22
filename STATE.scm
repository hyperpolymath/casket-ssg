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
    (updated . "2025-12-22")
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

    ;; =========================================================================
    ;; 6. CI/CD PIPELINE (4/4 Complete)
    ;; =========================================================================
    (ci-cd
     (count . "4/4")
     (status . "complete")
     (components
      ((component . "GitHub Actions CI")
       (location . ".github/workflows/ci.yml")
       (status . "complete"))
      ((component . "CodeQL Analysis")
       (location . ".github/workflows/codeql.yml")
       (status . "complete"))
      ((component . "Dependabot")
       (location . ".github/dependabot.yml")
       (status . "complete"))
      ((component . "Language Check")
       (description . "Haskell purity enforcement")
       (status . "complete"))))

    ;; =========================================================================
    ;; 7. DOCUMENTATION (8/8 Complete)
    ;; =========================================================================
    (documentation
     (count . "8/8")
     (status . "complete")
     (components
      ((component . "README")
       (location . "README.adoc")
       (status . "complete"))
      ((component . "Contributing Guide")
       (location . "CONTRIBUTING.md")
       (status . "complete"))
      ((component . "Security Policy")
       (location . "SECURITY.md")
       (status . "complete"))
      ((component . "Code of Conduct")
       (location . "CODE_OF_CONDUCT.md")
       (status . "complete"))
      ((component . "Cookbook")
       (location . "cookbook.adoc")
       (status . "complete"))
      ((component . "Copilot Instructions")
       (location . "copilot-instructions.md")
       (status . "complete"))
      ((component . "Adapter README")
       (location . "adapters/README.md")
       (status . "complete"))
      ((component . "Claude Instructions")
       (location . ".claude/CLAUDE.md")
       (status . "complete"))))

    ;; =========================================================================
    ;; 8. CONFIGURATION (3/3 Complete)
    ;; =========================================================================
    (configuration
     (count . "3/3")
     (status . "complete")
     (components
      ((component . "Cabal Package Config")
       (location . "casket-ssg.cabal")
       (status . "complete"))
      ((component . "ReScript Config")
       (location . "adapters/rescript.json")
       (status . "complete"))
      ((component . "Git Config")
       (location . ".gitignore, .gitattributes")
       (status . "complete"))))

    ;; =========================================================================
    ;; 9. SCM FILES (7/7 Complete)
    ;; =========================================================================
    (scm-files
     (count . "7/7")
     (status . "complete")
     (components
      ((component . "STATE.scm")
       (location . "STATE.scm")
       (status . "complete"))
      ((component . "META.scm")
       (location . "META.scm")
       (status . "complete"))
      ((component . "ECOSYSTEM.scm")
       (location . "ECOSYSTEM.scm")
       (status . "complete"))
      ((component . "ROADMAP.scm")
       (location . "ROADMAP.scm")
       (status . "complete"))
      ((component . "PLAYBOOK.scm")
       (location . "PLAYBOOK.scm")
       (status . "complete"))
      ((component . "AGENTIC.scm")
       (location . "AGENTIC.scm")
       (status . "complete"))
      ((component . "NEUROSYM.scm")
       (location . "NEUROSYM.scm")
       (status . "complete"))))

    ;; =========================================================================
    ;; 10. SECURITY (5/5 Complete)
    ;; =========================================================================
    (security
     (count . "5/5")
     (status . "complete")
     (components
      ((component . "Security Policy")
       (location . "SECURITY.md")
       (status . "complete"))
      ((component . "security.txt")
       (location . ".well-known/security.txt")
       (status . "complete"))
      ((component . "AIBDP Manifest")
       (location . ".well-known/aibdp.json")
       (status . "complete"))
      ((component . "Provenance")
       (location . ".well-known/provenance.json")
       (status . "complete"))
      ((component . "AI Policy")
       (location . ".well-known/ai.txt")
       (status . "complete"))))

    ;; =========================================================================
    ;; 11. HOOKS (3/3 Complete)
    ;; =========================================================================
    (hooks
     (count . "3/3")
     (status . "complete")
     (components
      ((component . "Pre-commit Hook")
       (location . ".githooks/pre-commit")
       (status . "complete"))
      ((component . "Pre-push Hook")
       (location . ".githooks/pre-push")
       (status . "complete"))
      ((component . "Commit-msg Hook")
       (location . ".githooks/commit-msg")
       (status . "complete"))))

    ;; =========================================================================
    ;; IN PROGRESS (4 items)
    ;; =========================================================================
    (in-progress
     ((component . "MCP Hub Integration Testing")
      (status . "in-progress")
      (completion . 70))
     ((component . "Property-Based Testing")
      (status . "in-progress")
      (tool . "QuickCheck")
      (completion . 0))
     ((component . "Haddock Documentation")
      (status . "in-progress")
      (completion . 20))
     ((component . "HLint Configuration")
      (status . "in-progress")
      (location . ".hlint.yaml")
      (completion . 0)))

    ;; =========================================================================
    ;; PENDING (2 items)
    ;; =========================================================================
    (pending
     ((component . "Nix Flake")
      (status . "pending")
      (location . "flake.nix"))
     ((component . "CHANGELOG")
      (status . "pending")
      (location . "CHANGELOG.md")))))

;;; ============================================================================
;;; BLOCKERS AND ISSUES
;;; ============================================================================

(define blockers-and-issues
  '((critical ())
    (high-priority ())
    (medium-priority
     (("Add SPDX header to Casket.hs" . "compliance")))))

;;; ============================================================================
;;; CRITICAL NEXT ACTIONS
;;; ============================================================================

(define critical-next-actions
  '((immediate
     (("Complete MCP adapter integration tests" . high)
      ("Add SPDX header to src/Casket.hs" . high)))
    (short-term
     (("Add QuickCheck property tests" . medium)
      ("Create CHANGELOG.md" . medium)
      ("Add flake.nix for Nix users" . low)))
    (long-term
     (("Implement file watching" . medium)
      ("Add RSS feed generation" . medium)
      ("Add sitemap generation" . low)))))

;;; ============================================================================
;;; STATE SUMMARY
;;; ============================================================================

(define state-summary
  '((project . "casket-ssg")
    (language . "Haskell")
    (total-components . 44)
    (complete . 38)
    (in-progress . 4)
    (pending . 2)
    (completion-percentage . 86)
    (blockers . 0)
    (updated . "2025-12-22")
    (phase . "v1.0 - Core Complete, Infrastructure Enhanced")))

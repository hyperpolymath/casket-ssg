;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;;; ECOSYSTEM.scm — casket-ssg Ecosystem Position & Relationships
;; Updated: 2025-12-22

(define-module (casket-ssg ecosystem)
  #:export (identity position relationships integration standards))

;;; ============================================================================
;;; PROJECT IDENTITY
;;; ============================================================================

(define identity
  '((name . "casket-ssg")
    (version . "1.0.0")
    (type . "satellite-ssg")
    (purpose . "The DEFINITIVE Haskell static site generator")
    (tagline . "Pure functional elegance in site generation")

    (language-identity
     (primary . "Haskell")
     (enforcement . "absolute")
     (rationale . "casket-ssg exists to be THE Haskell SSG. The entire engine is written in Haskell.")
     (forbidden . ("Python" "JavaScript" "TypeScript" "Ruby" "Go" "Rust" "Java"))
     (exception . ("ReScript in adapters/ for MCP protocol bridge only")))

    (what-this-is
     ("The DEFINITIVE static site generator written in Haskell")
     ("Part of the poly-ssg satellite constellation")
     ("Demonstration of pure functional SSG design")
     ("Reference implementation for Haskell web tooling"))

    (what-this-is-not
     ("NOT a template that can be reimplemented in other languages")
     ("NOT optional about being in Haskell")
     ("NOT a Hakyll replacement (complementary)")
     ("NOT a dynamic web framework"))))

;;; ============================================================================
;;; ECOSYSTEM POSITION
;;; ============================================================================

(define position
  '((constellation . "poly-ssg")
    (role . "satellite")
    (language-slot . "Haskell")

    (description
     "Satellite SSG in the poly-ssg constellation. Each satellite is the
      definitive SSG for its language. casket-ssg fills the Haskell slot,
      providing a pure functional approach to static site generation.")

    (uniqueness
     ("Only pure-Haskell SSG in constellation")
     ("Demonstrates functional programming patterns")
     ("Strong type safety guarantees")
     ("Minimal dependencies (base only)"))))

;;; ============================================================================
;;; RELATED PROJECTS
;;; ============================================================================

(define relationships
  '((hub
     (name . "poly-ssg-mcp")
     (url . "https://github.com/hyperpolymath/poly-ssg-mcp")
     (relationship . "hub")
     (description . "Unified MCP server for 28+ SSGs - provides adapter interface")
     (integration . "via adapters/src/CasketAdapter.res"))

    (standard
     (name . "rhodium-standard-repositories")
     (url . "https://github.com/hyperpolymath/rhodium-standard-repositories")
     (relationship . "standard")
     (description . "Repository quality standards")
     (compliance . "RSR Gold target"))

    (sibling-satellites
     ((name . "ember-ssg") (language . "Elixir"))
     ((name . "forge-ssg") (language . "Rust"))
     ((name . "anvil-ssg") (language . "Go"))
     ((name . "kiln-ssg") (language . "Python"))
     ((name . "crucible-ssg") (language . "Ruby")))

    (haskell-ecosystem
     ((name . "Hakyll")
      (relationship . "complementary")
      (description . "Full-featured Haskell SSG - casket-ssg is simpler, focused"))
     ((name . "Pandoc")
      (relationship . "potential-integration")
      (description . "Universal document converter"))
     ((name . "GHC")
      (relationship . "compiler")
      (description . "Glasgow Haskell Compiler 9.0+"))
     ((name . "Cabal")
      (relationship . "build-tool")
      (description . "Haskell build system 3.0+")))))

;;; ============================================================================
;;; INTEGRATION PATTERNS
;;; ============================================================================

(define integration
  '((mcp-hub
     (protocol . "MCP (Model Context Protocol)")
     (adapter-location . "adapters/src/CasketAdapter.res")
     (tools-exposed
      ("casket_build" . "Build the site")
      ("casket_clean" . "Clean build cache")
      ("casket_version" . "Get version info")
      ("casket_test" . "Run test suite"))
     (connection-method . "Child process execution via Cabal"))

    (ci-cd
     (platform . "GitHub Actions")
     (triggers . ("push to main" "pull requests" "weekly schedule"))
     (checks
      ("Haskell build" . "cabal build all")
      ("Test suite" . "cabal test all")
      ("Language purity" . "No Python/JS/TS in src/")
      ("CodeQL security" . "JavaScript/TypeScript analysis")))

    (dependency-management
     (haskell . "Cabal with version bounds")
     (adapter . "npm for ReScript dependencies")
     (automation . "Dependabot for updates"))))

;;; ============================================================================
;;; STANDARDS COMPLIANCE
;;; ============================================================================

(define standards
  '((rsr
     (name . "Rhodium Standard Repository")
     (target . "Gold")
     (requirements
      ("SHA-pinned GitHub Actions" . "implemented")
      ("SPDX headers" . "implemented")
      ("Security policy" . "implemented")
      ("Comprehensive documentation" . "implemented")
      ("CI/CD pipeline" . "implemented")
      ("Dependabot" . "implemented")))

    (security
     (policy . "SECURITY.md")
     (contact . "security@hyperpolymath.org")
     (disclosure . "Coordinated disclosure, 90-day window")
     (scanning . "CodeQL weekly"))

    (licensing
     (spdx . "AGPL-3.0-or-later")
     (copyright . "2025 Jonathan D.A. Jewell")
     (headers . "Required on all source files"))

    (ai-boundaries
     (protocol . "AIBDP v0.2")
     (manifest . ".well-known/aibdp.json")
     (training . "conditional with attribution")
     (fine-tuning . "encouraged for SSG tools"))))

;;; ============================================================================
;;; CONTRIBUTION FLOW
;;; ============================================================================

(define contribution-flow
  '((from-community
     (fork . "Fork on GitHub")
     (branch . "Create feature/fix branch")
     (develop . "Implement in Haskell")
     (test . "just test-all")
     (lint . "just lint && just fmt")
     (submit . "Pull request to main"))

    (to-hub
     (adapter-updates . "Update CasketAdapter.res")
     (version-bump . "Update version in package.json")
     (test-integration . "Test with poly-ssg-mcp locally")
     (coordinate . "PR to poly-ssg-mcp if needed"))

    (language-boundary
     (enforced-by . ("CLAUDE.md" "CI language-check" "META.scm"))
     (violation-handling . "Reject PR, explain Haskell requirement")
     (exception-process . "None - this is absolute"))))

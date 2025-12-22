;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;;; META.scm — casket-ssg Architecture & Design Metadata
;; Updated: 2025-12-22

(define-module (casket-ssg meta)
  #:export (architecture-decisions development-practices design-rationale
            language-rules quality-attributes technical-stack))

;;; ============================================================================
;;; LANGUAGE RULES (CRITICAL - ABSOLUTE ENFORCEMENT)
;;; ============================================================================

(define language-rules
  '((mandatory-language . "Haskell")
    (enforcement-level . "absolute")
    (rationale . "Each SSG satellite is the DEFINITIVE implementation for its language. casket-ssg IS the Haskell SSG.")

    (allowed-locations
     ((location . "src/")
      (language . "Haskell")
      (extensions . (".hs"))
      (purpose . "Core SSG engine"))
     ((location . "adapters/")
      (language . "ReScript")
      (extensions . (".res" ".resi"))
      (purpose . "MCP protocol bridge ONLY")))

    (violations
     ("Python implementation" . "FORBIDDEN")
     ("JavaScript/TypeScript in src/" . "FORBIDDEN")
     ("Ruby/Go/Rust in src/" . "FORBIDDEN")
     ("Any non-Haskell language in core" . "FORBIDDEN - defeats satellite purpose"))

    (correct-implementation
     (interpreter . "GHC 9.0+")
     (build-tool . "Cabal 3.0+")
     (package-format . ".cabal")
     (mcp-adapter . "adapters/ in ReScript"))))

;;; ============================================================================
;;; ARCHITECTURE DECISIONS
;;; ============================================================================

(define architecture-decisions
  '((adr-001
     (title . "Haskell-Only Implementation")
     (status . "accepted")
     (date . "2025-12-16")
     (context . "SSG satellites must be in their target language to serve as definitive examples")
     (decision . "casket-ssg is written entirely in Haskell, with only the MCP adapter in ReScript")
     (consequences
      ("Language-specific features fully utilized")
      ("Idiomatic functional patterns")
      ("Strong type safety guarantees")
      ("Compile-time verification")))

    (adr-002
     (title . "RSR Compliance")
     (status . "accepted")
     (date . "2025-12-15")
     (context . "Part of hyperpolymath ecosystem with standardized repository structure")
     (decision . "Follow Rhodium Standard Repository guidelines")
     (consequences
      ("RSR Gold target compliance")
      ("SHA-pinned GitHub Actions")
      ("SPDX headers on all files")
      ("Comprehensive security policy")))

    (adr-003
     (title . "Pure Functional Core")
     (status . "accepted")
     (date . "2025-12-16")
     (context . "Haskell enables pure functional programming")
     (decision . "All transformations are pure functions; IO at edges only")
     (consequences
      ("Referential transparency")
      ("Easy testing and reasoning")
      ("Parallelization potential")
      ("No hidden state")))

    (adr-004
     (title . "State Machine Markdown Parser")
     (status . "accepted")
     (date . "2025-12-16")
     (context . "Need predictable, maintainable parsing")
     (decision . "Use explicit state machine via fold with ParserState record")
     (consequences
      ("Clear state transitions")
      ("Easy to extend")
      ("Debuggable")))

    (adr-005
     (title . "MCP Integration via ReScript Adapter")
     (status . "accepted")
     (date . "2025-12-17")
     (context . "poly-ssg hub requires MCP protocol support")
     (decision . "Thin ReScript adapter for MCP, all logic in Haskell")
     (consequences
      ("Clean separation of concerns")
      ("Haskell purity preserved")
      ("Protocol flexibility")))))

;;; ============================================================================
;;; TECHNICAL STACK
;;; ============================================================================

(define technical-stack
  '((core
     (language . "Haskell")
     (compiler . "GHC 9.0+")
     (build-tool . "Cabal 3.0+")
     (standard . "Haskell2010"))

    (adapter
     (language . "ReScript")
     (version . "11.1+")
     (runtime . "Node.js")
     (purpose . "MCP protocol bridge"))

    (ci-cd
     (platform . "GitHub Actions")
     (runners . ("ubuntu-latest"))
     (tools . ("GHC 9.4" "Cabal 3.10" "HLint" "CodeQL")))

    (quality
     (linter . "HLint")
     (formatter . "fourmolu")
     (type-checker . "GHC")
     (security . "CodeQL"))

    (automation
     (task-runner . "just")
     (requirements . "Mustfile")
     (dependency-updates . "Dependabot"))))

;;; ============================================================================
;;; DEVELOPMENT PRACTICES
;;; ============================================================================

(define development-practices
  '((code-style
     (language . "Haskell")
     (guide . "HLint suggestions")
     (formatter . "fourmolu")
     (imports . "explicit qualified"))

    (testing
     (unit . "Built-in test suite")
     (integration . "test-full command")
     (property . "QuickCheck (planned)")
     (golden . "tasty-golden (planned)"))

    (documentation
     (format . "Haddock")
     (user-docs . "AsciiDoc")
     (api-docs . "auto-generated"))

    (security
     (sast . "CodeQL for workflow scanning")
     (credentials . "Environment variables only")
     (secrets . "Never committed")
     (disclosure . "Coordinated via SECURITY.md"))

    (versioning
     (scheme . "SemVer 2.0.0")
     (changelog . "CHANGELOG.md")
     (tags . "vX.Y.Z format"))))

;;; ============================================================================
;;; QUALITY ATTRIBUTES
;;; ============================================================================

(define quality-attributes
  '((correctness
     (description . "Produce valid HTML from valid Markdown")
     (verification . "Comprehensive test suite")
     (measure . "All tests pass"))

    (security
     (description . "Prevent code injection via escaping")
     (verification . "escapeHtml on all user content")
     (measure . "No XSS vulnerabilities"))

    (performance
     (description . "Efficient processing of content")
     (verification . "Lazy evaluation, streaming")
     (target . "< 100ms per page"))

    (maintainability
     (description . "Easy to understand and modify")
     (verification . "Pure functions, clear types")
     (measure . "HLint score, type coverage"))

    (reliability
     (description . "Consistent behavior across runs")
     (verification . "Deterministic pure functions")
     (measure . "No flaky tests"))))

;;; ============================================================================
;;; DESIGN RATIONALE
;;; ============================================================================

(define design-rationale
  '((why-haskell
     "casket-ssg is THE Haskell SSG. No other language is acceptable.
      This is not a limitation but a feature - it demonstrates Haskell's
      strengths for content transformation: strong types, pure functions,
      and elegant composition.")

    (why-pure-functional
     "Pure functional design enables:
      - Referential transparency for easy reasoning
      - Trivial parallelization
      - Comprehensive testing
      - No hidden state or side effects")

    (why-simple-dependencies
     "Only base library required. This demonstrates that Haskell's
      standard library is sufficient for many tasks, and reduces
      dependency management overhead.")

    (why-state-machine-parser
     "Explicit state machine via ParserState record makes parsing
      behavior visible and debuggable. State transitions are clear
      and the parser is easy to extend.")))

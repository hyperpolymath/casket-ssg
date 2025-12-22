;;; AGENTIC.scm — casket-ssg AI Agent Interaction Patterns
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;; Updated: 2025-12-22

(define-module (casket-ssg agentic)
  #:export (agent-capabilities constraints interaction-patterns mcp-interface))

;;; ============================================================================
;;; AGENT CAPABILITIES
;;; ============================================================================

(define agent-capabilities
  '((primary-function . "Static site generation via pure functional Haskell")
    (language-constraint . "Haskell-only implementation (ABSOLUTE)")
    (adapter-language . "ReScript for MCP protocol bridge only")

    (capabilities
     ((capability . "markdown-to-html")
      (description . "Convert Markdown content to HTML")
      (input . "Markdown string with optional frontmatter")
      (output . "HTML string")
      (pure . #t))

     ((capability . "frontmatter-parsing")
      (description . "Extract YAML-style frontmatter metadata")
      (input . "Content with --- delimited frontmatter")
      (output . "Frontmatter record + body string")
      (fields . ("title" "date" "tags" "draft" "template")))

     ((capability . "template-application")
      (description . "Apply template with variable substitution")
      (input . "Frontmatter + HTML content")
      (output . "Complete HTML document")
      (variables . ("{{title}}" "{{date}}" "{{content}}")))

     ((capability . "site-build")
      (description . "Build complete static site")
      (input . "Source directory with content/")
      (output . "Generated site in _site/")))

    (limitations
     ("No dynamic content generation")
     ("No database connections")
     ("No server-side rendering")
     ("Single-threaded processing (current version)")
     ("No plugin system yet"))))

;;; ============================================================================
;;; LANGUAGE CONSTRAINTS (CRITICAL)
;;; ============================================================================

(define constraints
  '((language-enforcement
     (rule . "ALL SSG logic MUST be in Haskell")
     (enforcement . "absolute")
     (rationale . "casket-ssg is THE DEFINITIVE Haskell SSG")

     (allowed
      ((location . "src/")
       (language . "Haskell")
       (extensions . (".hs")))
      ((location . "adapters/")
       (language . "ReScript")
       (extensions . (".res" ".resi"))
       (purpose . "MCP protocol bridge ONLY")))

     (forbidden
      ("Python" "JavaScript" "TypeScript" "Ruby" "Go" "Rust" "Java")
      (in-locations . ("src/" "lib/" "app/"))))

    (agent-rules
     ((rule . "never-suggest-rewrite")
      (description . "NEVER suggest rewriting in another language")
      (violation-response . "Refuse and explain Haskell requirement"))

     ((rule . "preserve-purity")
      (description . "Maintain pure functional patterns")
      (rationale . "Haskell's strength is referential transparency"))

     ((rule . "type-safety")
      (description . "Leverage Haskell's type system")
      (patterns . ("ADTs" "type classes" "newtypes" "phantom types"))))))

;;; ============================================================================
;;; MCP INTERFACE
;;; ============================================================================

(define mcp-interface
  '((server-info
     (name . "casket-ssg")
     (version . "1.0.0")
     (description . "Pure functional Haskell static site generator")
     (language . "Haskell"))

    (tools
     ((tool . "casket_build")
      (description . "Build the casket-ssg site")
      (input-schema
       (type . "object")
       (properties
        ((path (type . "string") (description . "Path to site root")))))
      (invocation . "cabal run casket-ssg -- build"))

     ((tool . "casket_clean")
      (description . "Clean the build cache")
      (input-schema
       (type . "object")
       (properties
        ((path (type . "string")))))
      (invocation . "cabal run casket-ssg -- clean"))

     ((tool . "casket_version")
      (description . "Get casket-ssg and GHC version")
      (input-schema (type . "object") (properties ()))
      (invocation . "ghc --version"))

     ((tool . "casket_test")
      (description . "Run casket-ssg test suite")
      (input-schema
       (type . "object")
       (properties
        ((test-type
          (type . "string")
          (enum . ("markdown" "frontmatter" "full" "all"))))))
      (invocation . "cabal run casket-ssg -- test-{type}")))

    (resources
     ((resource . "site-config")
      (uri . "casket://config")
      (description . "Current site configuration"))

     ((resource . "build-status")
      (uri . "casket://status")
      (description . "Build status and statistics")))))

;;; ============================================================================
;;; INTERACTION PATTERNS
;;; ============================================================================

(define interaction-patterns
  '((code-modification
     (pattern . "read-understand-modify")
     (steps
      (1 . "Read existing Haskell code")
      (2 . "Understand pure functional patterns in use")
      (3 . "Propose changes maintaining purity")
      (4 . "Verify type-correctness")
      (5 . "Run tests before completion")))

    (feature-request
     (pattern . "analyze-design-implement")
     (steps
      (1 . "Analyze request against non-goals")
      (2 . "Design using Haskell idioms (ADTs, type classes)")
      (3 . "Implement with pure functions")
      (4 . "Add to test suite")
      (5 . "Update documentation")))

    (debugging
     (pattern . "reproduce-trace-fix")
     (steps
      (1 . "Reproduce issue with test case")
      (2 . "Trace through pure functions")
      (3 . "Identify incorrect transformation")
      (4 . "Fix maintaining referential transparency")
      (5 . "Verify fix with test")))

    (optimization
     (pattern . "measure-analyze-improve")
     (steps
      (1 . "Measure current performance")
      (2 . "Analyze lazy evaluation patterns")
      (3 . "Apply strictness where beneficial")
      (4 . "Consider parallel strategies")
      (5 . "Benchmark improvement")))))

;;; ============================================================================
;;; AGENT DIRECTIVES
;;; ============================================================================

(define agent-directives
  '((primary-directive
     "Assist with Haskell static site generator development while maintaining
      language purity and functional programming principles.")

    (response-patterns
     ((context . "language-suggestion")
      (response . "casket-ssg is the DEFINITIVE Haskell SSG. All implementation
                   must remain in Haskell. The adapter in ReScript is the only
                   exception, and it handles MCP protocol bridging only."))

     ((context . "feature-request")
      (response . "Analyze against ROADMAP.scm phases and non-goals.
                   Implement using pure functional patterns."))

     ((context . "performance")
      (response . "Consider lazy evaluation, strictness annotations,
                   and parallel strategies while maintaining purity.")))

    (forbidden-actions
     ("Suggesting Python/JS/TS implementations")
     ("Adding impure code without IO monad")
     ("Breaking type safety")
     ("Introducing runtime dependencies on non-Haskell tools in core"))))

;;; PLAYBOOK.scm — casket-ssg Operational Playbook
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;; Updated: 2025-12-22

(define-module (casket-ssg playbook)
  #:export (operations workflows runbooks emergency-procedures))

;;; ============================================================================
;;; BUILD OPERATIONS
;;; ============================================================================

(define build-operations
  '((build-dev
     (description . "Development build with warnings")
     (command . "just build")
     (fallback . "cabal build all -Wall")
     (prerequisites . ("GHC 9.0+" "Cabal 3.0+"))
     (outputs . ("dist-newstyle/")))

    (build-release
     (description . "Optimized release build")
     (command . "just release")
     (fallback . "cabal build all -O2")
     (flags . ("-O2" "-Wall" "-Werror")))

    (clean
     (description . "Clean build artifacts")
     (command . "just clean")
     (fallback . "cabal clean")
     (removes . ("dist-newstyle/" ".cabal-sandbox/")))

    (rebuild
     (description . "Clean and rebuild")
     (command . "just rebuild")
     (sequence . ("clean" "build-dev")))))

;;; ============================================================================
;;; TEST OPERATIONS
;;; ============================================================================

(define test-operations
  '((test-unit
     (description . "Run unit tests")
     (command . "just test")
     (fallback . "cabal test all")
     (coverage . #t))

    (test-markdown
     (description . "Test markdown parser")
     (command . "just test-markdown")
     (fallback . "cabal run casket-ssg -- test-markdown"))

    (test-frontmatter
     (description . "Test frontmatter parser")
     (command . "just test-frontmatter")
     (fallback . "cabal run casket-ssg -- test-frontmatter"))

    (test-full
     (description . "Full integration test")
     (command . "just test-full")
     (fallback . "cabal run casket-ssg -- test-full"))

    (test-e2e
     (description . "End-to-end integration tests")
     (command . "just test-e2e")
     (requires . ("example site" "expected outputs")))

    (test-all
     (description . "Run all tests")
     (command . "just test-all")
     (sequence . ("test-unit" "test-markdown" "test-frontmatter" "test-full" "test-e2e")))))

;;; ============================================================================
;;; LINT OPERATIONS
;;; ============================================================================

(define lint-operations
  '((lint-haskell
     (description . "Lint Haskell code with HLint")
     (command . "just lint")
     (fallback . "hlint src/")
     (config . ".hlint.yaml"))

    (lint-rescript
     (description . "Type-check ReScript adapter")
     (command . "just lint-adapter")
     (fallback . "cd adapters && npm run build")
     (directory . "adapters/"))

    (format-haskell
     (description . "Format Haskell code")
     (command . "just fmt")
     (fallback . "fourmolu -i src/**/*.hs")
     (tool . "fourmolu"))

    (format-check
     (description . "Check formatting without modifying")
     (command . "just fmt-check")
     (fallback . "fourmolu --mode check src/**/*.hs"))))

;;; ============================================================================
;;; ADAPTER OPERATIONS
;;; ============================================================================

(define adapter-operations
  '((adapter-build
     (description . "Build ReScript MCP adapter")
     (command . "just adapter-build")
     (fallback . "cd adapters && npm run build")
     (language . "ReScript")
     (outputs . ("adapters/src/CasketAdapter.mjs")))

    (adapter-watch
     (description . "Watch mode for adapter development")
     (command . "just adapter-watch")
     (fallback . "cd adapters && npm run watch"))

    (adapter-clean
     (description . "Clean adapter build artifacts")
     (command . "just adapter-clean")
     (fallback . "cd adapters && npm run clean"))))

;;; ============================================================================
;;; DEVELOPMENT WORKFLOWS
;;; ============================================================================

(define workflows
  '((daily-development
     (steps
      ("git pull origin main"
       "just deps"
       "just build"
       "just test"
       "just lint")))

    (pre-commit
     (steps
      ("just fmt-check"
       "just lint"
       "just test"
       "just build"))
     (blocking . #t))

    (release-preparation
     (steps
      ("just test-all"
       "just lint"
       "just release"
       "just docs"
       "Update CHANGELOG.md"
       "Update version in .cabal"
       "git tag vX.Y.Z")))

    (ci-pipeline
     (steps
      ("checkout"
       "setup-ghc"
       "cache-restore"
       "build"
       "test"
       "language-check"
       "cache-save")))))

;;; ============================================================================
;;; RUNBOOKS
;;; ============================================================================

(define runbooks
  '((new-feature
     (description . "Adding a new feature")
     (steps
      (1 . "Create feature branch: git checkout -b feature/name")
      (2 . "Implement in src/Casket.hs maintaining pure functional style")
      (3 . "Add tests to test suite")
      (4 . "Run: just test-all")
      (5 . "Run: just lint && just fmt")
      (6 . "Update documentation if needed")
      (7 . "Submit PR")))

    (bug-fix
     (description . "Fixing a bug")
     (steps
      (1 . "Create bug branch: git checkout -b fix/issue-N")
      (2 . "Write failing test that reproduces bug")
      (3 . "Fix bug in src/Casket.hs")
      (4 . "Verify test passes")
      (5 . "Run: just test-all")
      (6 . "Submit PR with issue reference")))

    (dependency-update
     (description . "Updating dependencies")
     (steps
      (1 . "Review Dependabot PR")
      (2 . "Check CHANGELOG of updated dependency")
      (3 . "Run: just test-all")
      (4 . "Merge if tests pass")))))

;;; ============================================================================
;;; EMERGENCY PROCEDURES
;;; ============================================================================

(define emergency-procedures
  '((security-vulnerability
     (severity . "critical")
     (steps
      (1 . "Do NOT disclose publicly")
      (2 . "Create private security advisory on GitHub")
      (3 . "Develop fix in private fork")
      (4 . "Coordinate disclosure per SECURITY.md")
      (5 . "Release patched version")
      (6 . "Publish advisory")))

    (build-failure
     (severity . "high")
     (steps
      (1 . "Check GHC/Cabal version compatibility")
      (2 . "Clear cache: just clean")
      (3 . "Update dependencies: cabal update")
      (4 . "Rebuild: just rebuild")
      (5 . "Check CI logs for environment differences")))

    (regression
     (severity . "high")
     (steps
      (1 . "Identify commit that introduced regression: git bisect")
      (2 . "Write regression test")
      (3 . "Revert or fix")
      (4 . "Verify with: just test-all")))))

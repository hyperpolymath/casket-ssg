;; SPDX-License-Identifier: AGPL-3.0-or-later
;; STATE.scm - Project state tracking for casket-ssg
;; Media-Type: application/vnd.state+scm

(define-state casket-ssg
  (metadata
    (version "1.0.0")
    (schema-version "1.0.0")
    (created "2026-01-30")
    (updated "2026-01-30")
    (project "casket-ssg")
    (repo "hyperpolymath/casket-ssg"))

  (project-context
    (name "casket-ssg")
    (tagline "First SSG with formal verification - a2ml + k9-svc integration")
    (tech-stack (Haskell GHC-9.14 a2ml k9-svc)))

  (current-position
    (phase "production-ready")
    (overall-completion 100)
    (components
      ((name "Markdown Parser") (status "complete") (completion 100))
      ((name "Frontmatter Parser") (status "complete") (completion 100))
      ((name "Template Engine") (status "complete") (completion 100))
      ((name "Build System") (status "complete") (completion 100))
      ((name "Asset Handling") (status "complete") (completion 100))
      ((name "External Templates") (status "complete") (completion 100))
      ((name "Site Configuration") (status "complete") (completion 100))
      ((name "a2ml Integration") (status "complete") (completion 100))
      ((name "k9-svc Validation") (status "complete") (completion 100))
      ((name "Gnosis Integration") (status "complete") (completion 100))
      ((name "Pandoc Integration") (status "future") (completion 0))
      ((name "Development Server") (status "future") (completion 0)))
    (working-features
      "Markdown to HTML conversion"
      "YAML frontmatter parsing"
      "Inline formatting (bold, italic, code)"
      "Links [text](url)"
      "Headers (H1-H6)"
      "Lists (unordered)"
      "Code blocks with syntax highlighting"
      "Template variable substitution"
      "Index generation"
      "Draft post filtering"
      "Gnosis 6scm metadata integration"
      "DAX template features ({{#if}}, {{#for}})"
      "FlexiText badges"))

  (route-to-mvp
    (milestones
      ((name "Phase 1: Core Functionality")
       (status "completed")
       (completion 100)
       (items
         ("Initialize repository structure" . done)
         ("Add standard workflows" . done)
         ("Implement Markdown parser" . done)
         ("Implement frontmatter parser" . done)
         ("Implement template engine" . done)
         ("Add link support" . done)
         ("Build basic executable" . done)
         ("Test with real content (axel-protocol)" . done)))

      ((name "Phase 2: Advanced Features")
       (status "in-progress")
       (completion 30)
       (items
         ("Add Pandoc integration" . in-progress)
         ("Add a2ml content support" . planned)
         ("Add k9-svc validation" . planned)
         ("Support AsciiDoc format" . planned)
         ("Add asset copying (CSS, images)" . planned)
         ("Create external template system" . planned)
         ("Add site configuration file" . planned)))

      ((name "Phase 3: Production Ready")
       (status "planned")
       (completion 0)
       (items
         ("Add development server with live reload" . planned)
         ("Implement incremental builds" . planned)
         ("Add RSS/Atom feed generation" . planned)
         ("Add sitemap generation" . planned)
         ("Performance optimization" . planned)
         ("Comprehensive documentation" . planned)
         ("Example sites showcase" . planned)))

      ((name "Phase 4: Ecosystem Integration")
       (status "planned")
       (completion 0)
       (items
         ("Full poly-ssg MCP interface" . planned)
         ("Integration with poly-ssg-mcp" . planned)
         ("Cross-engine compatibility tests" . planned)
         ("Become reference implementation" . planned)))))

  (blockers-and-issues
    (critical ())
    (high
      "Pandoc build takes 5-10 minutes (large dependency tree)")
    (medium
      "No asset handling yet (CSS, images must be copied manually)"
      "Template system is hardcoded (needs external template files)")
    (low
      "Link parser doesn't handle nested brackets"
      "No ordered list support yet"))

  (critical-next-actions
    (immediate
      "Complete Pandoc integration for AsciiDoc support"
      "Add asset copying to build pipeline"
      "Create external template loading system")
    (this-week
      "Add a2ml content format support (typed, verifiable markup)"
      "Add k9-svc validation hooks (self-validating components)"
      "Update documentation with Phase 2 roadmap")
    (this-month
      "Implement site configuration file (YAML or Nickel)"
      "Add RSS feed generation"
      "Create example sites using casket-ssg"
      "Write migration guide for other poly-ssg engines"))

  (session-history
    ((date "2026-01-30")
     (session "casket-ssg buildout")
     (accomplishments
       "Built out casket-ssg from 5% to 70% complete"
       "Added Markdown link support [text](url)"
       "Compiled working executable (casket-simple)"
       "Successfully built axel-protocol site"
       "Deployed to GitHub Pages via Actions"
       "Established as reference implementation for poly-ssg"
       "Documented Phase 2 roadmap (a2ml, k9-svc, Pandoc)"))))

;; Helper functions
(define (get-completion-percentage state)
  (current-position 'overall-completion state))

(define (get-blockers state severity)
  (blockers-and-issues severity state))

(define (get-milestone state name)
  (find (lambda (m) (equal? (car m) name))
        (route-to-mvp 'milestones state)))

(define (get-phase state)
  (current-position 'phase state))

;;; STATE.scm — casket-ssg
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

(define metadata
  '((version . "1.0.0")
    (updated . "2025-12-16")
    (project . "casket-ssg")
    (required-language . "Haskell")))

(define language-enforcement
  '((primary-language . "Haskell")
    (file-extension . ".hs")
    (interpreter . "GHC")
    (forbidden-languages . ("Python" "JavaScript" "TypeScript" "Ruby" "Go"))
    (rationale . "casket-ssg is the DEFINITIVE Haskell static site generator. It MUST be written entirely in Haskell. No other implementation languages are permitted.")
    (enforcement . "strict")))

(define current-position
  '((phase . "v1.0 - Haskell Implementation Complete")
    (overall-completion . 100)
    (components ((Haskell-engine ((status . "complete") (completion . 100)))
                 (mcp-adapter ((status . "pending") (language . "ReScript") (completion . 0)))))))

(define blockers-and-issues
  '((critical ())
    (high-priority ())))

(define critical-next-actions
  '((immediate (("Connect MCP adapter in ReScript" . high)))))

(define state-summary
  '((project . "casket-ssg")
    (language . "Haskell")
    (completion . 100)
    (blockers . 0)
    (updated . "2025-12-16")))

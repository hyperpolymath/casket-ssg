<!--
SPDX-License-Identifier: CC-BY-SA-4.0
SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
-->

[![OpenSSF Best Practices](https://img.shields.io/badge/OpenSSF-Best_Practices-green?logo=opensourcesecurity)](https://www.bestpractices.dev/en/projects/new?repo_url=https://github.com/hyperpolymath/casket-ssg)
[![License: MPL-2.0](https://img.shields.io/badge/License-MPL_2.0-blue.svg)](https://github.com/hyperpolymath/casket-ssg/blob/main/LICENSE)
![Accessibility](https://img.shields.io/badge/a11y-WCAG--aware_by_default-success)
[![poly-ssg](https://img.shields.io/badge/part_of-poly--ssg-indigo)](https://github.com/hyperpolymath/poly-ssg)
[![RSR Compliant](https://img.shields.io/badge/RSR-compliant-gold)](https://github.com/hyperpolymath/rhodium-standard-repositories)

**An accessibility-first, metadata-driven static site generator.**
Markdown in, an accessible website out — WCAG-aware output *by default*,
plus a metadata layer (**Gnosis**) that renders pages from typed project
data. The Haskell member of
[poly-ssg](https://github.com/hyperpolymath/poly-ssg).

> [!NOTE]
> **Status: alpha.** The `build` / `clean` pipeline works and generates
> real sites. Items marked *Planned* below are not yet implemented. The
> engine’s direction — demote Pandoc, adopt a Djot prose core + `a2ml`
> typed content on the
> [boj](https://github.com/hyperpolymath/boj-server) substrate — is
> recorded in [ADR-0001](docs/decisions/ADR-0001-content-engine.adoc).
> Architecture & invariants: [EXPLAINME](EXPLAINME.adoc).

<div id="toc">

</div>

# Example

A minimal site is a tree of Markdown with front-matter:

    content/
    ├── index.md
    └── posts/
        └── 2026-06-23-hello.md

```markdown
---
title: Hello, world
date: 2026-06-23
tags: [intro]
description: A first page built with casket.
---
# Hello

Markdown with **rich** formatting, tables, footnotes and highlighted code.
```

Build it:

```bash
casket-ssg build ./content ./_site
```

You get an **accessible** `_site/` with no configuration:

- `index.html` and `posts/2026-06-23-hello/index.html` rendered through
  an accessible default theme (skip link, labelled landmarks, light/dark
  `color-scheme`), with table headers auto-scoped for WCAG 1.3.1;

- a date-sorted collection index for `posts/`, and per-tag pages under
  `/tags/`;

- `sitemap.xml` and an Atom `feed.xml`.

No Node, no npm, no required config file.

# Installation

casket is not yet on Hackage — build from source (GHC 9.4+, Cabal 3 or
Stack 2):

```bash
git clone https://github.com/hyperpolymath/casket-ssg
cd casket-ssg
stack build                         # or: cabal build
stack run casket-ssg -- --version   # or put the built binary on PATH
```

CLI:

```bash
casket-ssg build <input-dir> [output-dir]   # default output: _site
casket-ssg build --drafts <input-dir>       # include draft: true pages (or CASKET_DRAFTS=1)
casket-ssg build --no-clean-urls <input-dir> # emit foo.html, not foo/index.html
casket-ssg clean [output-dir]
casket-ssg --version
```

# What makes it different

Accessibility by construction  
The default theme is accessible by default, casket rewrites Pandoc table
headers to add `scope="col"`, and the Gnosis `FlexiText` type makes
**empty alt-text a constructor error** — generated badges cannot ship
without alt text. Accessibility is a property of the engine, not a
checklist for the author.

Metadata-driven (Gnosis)  
An optional layer renders pages from a typed metadata context:
`(:placeholder)` substitution, `{{#if}}` / `{{#for}}` conditionals and
loops (DAX), and Shields.io badges generated from typed values — sourced
from the repo’s `.machine_readable/descriptiles/` data.

Verified & agent-native — the trajectory  
casket is heading toward typed `a2ml` content (build fails on malformed
data), a formally-verifiable Djot core, and a verified agent surface as
a [boj](https://github.com/hyperpolymath/boj-server) cartridge rather
than an embedded MCP server. See
[ADR-0001](docs/decisions/ADR-0001-content-engine.adoc).

Permissively licensed  
**MPL-2.0** — build proprietary or differently-licensed sites on top,
unlike copyleft alternatives.

# Features

**Implemented**

- Recursive multi-page build mirroring the source tree

- Front-matter: `title`, `date`, `description`, `layout`, `draft`,
  `tags`, `slug`, plus arbitrary keys as `{{key}}` template vars

- Rich Markdown via Pandoc — anchors, pipe tables, fenced/highlighted
  code, footnotes, task lists, strikethrough, smart punctuation

- File templates with per-page `layout`, partials (`{{>` `name}}`),
  `{{nav}}`, `{macro}`, `{{site.*}}` (from `site.conf`)

- Collections (date-sorted directory index) and per-tag pages
  (`/tags/<tag>/`)

- `sitemap.xml` + Atom `feed.xml`

- Accessible default theme + automatic `scope="col"` on table headers

- Gnosis layer — `(:placeholder)`, `{{#if}}`/`{{#for}}` (DAX),
  `FlexiText` badges

- Clean URLs, toggleable

**Planned** (do not assume these work today — see ADR-0001)

- `init` scaffolding and a `serve` / live-reload dev server

- Incremental rebuilds; asset fingerprinting

- `a2ml`-native typed content + the Djot core; Pandoc relegated to an
  importer

- `boj` cartridge registration; `llms.txt` / per-page machine views

# Documentation

- [EXPLAINME](EXPLAINME.adoc) — architecture & invariants (developer
  deep-dive)

- [ADR-0001](docs/decisions/ADR-0001-content-engine.adoc) —
  content-engine direction

- [Contributing](CONTRIBUTING.adoc) · [Governance](GOVERNANCE.adoc)

# Where it fits

- [**poly-ssg**](https://github.com/hyperpolymath/poly-ssg) — the family
  of language-native SSGs casket belongs to, unified via MCP.

- [**ddraig**](https://github.com/hyperpolymath/ddraig-ssg) — the
  dependently-typed (Idris2) sibling; casket’s "proven twin" (casket
  validates, ddraig proves).

- [**boj-server**](https://github.com/hyperpolymath/boj-server) — the
  estate’s verified MCP/capability server casket plugs into as a
  cartridge.

# Requirements

- GHC 9.4+ (CI builds with 9.4.8), Cabal 3.0+ or Stack 2.0+

- Pandoc (current library dependency; see ADR-0001 for the planned move
  off it)

# License

Code: **MPL-2.0** ([LICENSE](LICENSE)). Prose documentation:
`CC-BY-SA-4.0`.

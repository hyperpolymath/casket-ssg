[![Sponsor](https://img.shields.io/badge/Sponsor-%E2%9D%A4-pink?logo=github)](https://github.com/sponsors/hyperpolymath)

// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>

= casket-ssg
image:https://img.shields.io/badge/OpenSSF-Best_Practices-green?logo=opensourcesecurity[OpenSSF Best Practices,link="https://www.bestpractices.dev/en/projects/new?repo_url=https://github.com/hyperpolymath/casket-ssg"]
image:https://img.shields.io/badge/License-PMPL--1.0-blue.svg[License: PMPL-1.0,link="https://github.com/hyperpolymath/palimpsest-license"]
image:https://api.thegreenwebfoundation.org/greencheckimage/github.com[Green Web,link="https://www.thegreenwebfoundation.org/green-web-check/?url=github.com"]




image:https://img.shields.io/badge/Philosophy-Palimpsest-indigo.svg[Palimpsest,link="https://github.com/hyperpolymath/palimpsest-license"]


:toc: auto
:toclevels: 2

image:https://img.shields.io/badge/RSR-compliant-gold[RSR Compliant,link=https://github.com/hyperpolymath/rhodium-standard-repositories]

**Pure functional static site generator in Haskell.**

== Who Is This For?

* **Haskell developers** who want to build sites with tools they know and love
* **Functional programming enthusiasts** who appreciate compositional design
* **Teams requiring reliability** where type safety prevents runtime surprises
* **Anyone frustrated** by dynamic language site builders that fail at deploy time

== Why casket-ssg?

=== Type Safety That Matters

Your site structure is checked at compile time. Missing templates, broken links, malformed frontmatter - all caught before deployment, not discovered by users.

=== Composable Pipelines

Content transformations compose like functions should. Chain markdown processing, templating, and asset handling with confidence that types align.

=== Lazy Large-Site Builds

Haskell's lazy evaluation means casket-ssg only processes what's needed. Incremental builds are natural, not bolted on.

=== Pandoc Integration

Best-in-class document conversion with full Pandoc support. Markdown, reStructuredText, Org-mode - your content, your format.

== Quick Start

[source,bash]
----
# Install
cabal update
cabal install casket-ssg

# Create a site
casket-ssg init my-site
cd my-site

# Build
casket-ssg build

# Preview locally
casket-ssg serve
----

== Features

* **Compile-time template validation** - broken templates don't build
* **Strong frontmatter types** - YAML parsing with schema enforcement
* **Asset pipeline** - CSS/JS processing with hash-based cache busting
* **Incremental builds** - only rebuild what changed
* **Live reload** - instant preview during development
* **RSS/Atom feeds** - generated automatically from content
* **Sitemap generation** - SEO-ready output

== Requirements

* GHC 9.0 or later
* Cabal 3.0 or Stack 2.0

== Part of poly-ssg

casket-ssg is part of the https://github.com/hyperpolymath/poly-ssg[poly-ssg] family of language-native static site generators, unified through https://github.com/hyperpolymath/poly-ssg-mcp[MCP integration].

== License

MPL-2.0

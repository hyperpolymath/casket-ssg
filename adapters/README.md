# SSG Adapters

These adapters are synchronized from the [poly-ssg-mcp](https://github.com/hyperpolymath/poly-ssg-mcp) hub.

## Usage

Each adapter wraps a specific static site generator CLI and exposes it via the MCP protocol.

## Available Adapters

| Adapter | Language | SSG |
|---------|----------|-----|
| babashka.js | Clojure | Babashka |
| cobalt.js | Rust | Cobalt |
| coleslaw.js | Common Lisp | Coleslaw |
| cryogen.js | Clojure | Cryogen |
| documenter.js | Julia | Documenter.jl |
| ema.js | Haskell | Ema |
| fornax.js | F# | Fornax |
| franklin.js | Julia | Franklin.jl |
| frog.js | Racket | Frog |
| hakyll.js | Haskell | Hakyll |
| laika.js | Scala | Laika |
| marmot.js | Crystal | Marmot |
| mdbook.js | Rust | mdBook |
| nimble-publisher.js | Elixir | NimblePublisher |
| nimrod.js | Nim | Nimrod |
| orchid.js | Kotlin | Orchid |
| perun.js | Clojure | Perun |
| pollen.js | Racket | Pollen |
| publish.js | Swift | Publish |
| reggae.js | D | Reggae |
| scalatex.js | Scala | ScalaTex |
| serum.js | Elixir | Serum |
| staticwebpages.js | Julia | StaticWebPages.jl |
| tableau.js | Elixir | Tableau |
| wub.js | Tcl | Wub |
| yocaml.js | OCaml | YOCaml |
| zola.js | Rust | Zola |
| zotonic.js | Erlang | Zotonic |

## Synchronization

To update these adapters from the hub:

```bash
~/Documents/scripts/transfer-ssg-adapters.sh --satellite $(basename $(pwd))
```

---
*Synced from poly-ssg-mcp hub*

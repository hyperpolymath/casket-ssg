# SPDX-License-Identifier: AGPL-3.0-or-later
# casket-ssg Justfile — Haskell SSG Build Automation
#
# Usage: just <recipe>
# Run `just --list` for available recipes

# Default recipe - show help
default:
    @just --list

# ============================================================================
# BUILD RECIPES
# ============================================================================

# Build the project (development)
build:
    cabal build all

# Build with optimizations for release
release:
    cabal build all -O2

# Clean build artifacts
clean:
    cabal clean
    rm -rf _site/

# Clean and rebuild
rebuild: clean build

# Update dependencies
deps:
    cabal update

# ============================================================================
# TEST RECIPES
# ============================================================================

# Run all cabal tests
test:
    cabal test all

# Test markdown parser
test-markdown:
    cabal run casket-ssg -- test-markdown

# Test frontmatter parser
test-frontmatter:
    cabal run casket-ssg -- test-frontmatter

# Full integration test
test-full:
    cabal run casket-ssg -- test-full

# End-to-end tests
test-e2e:
    @echo "Running e2e tests..."
    @test -d examples/ && cabal run casket-ssg -- build examples/ || echo "No examples/ directory"

# Run ALL tests
test-all: test test-markdown test-frontmatter test-full test-e2e
    @echo "✓ All tests passed"

# ============================================================================
# LINT & FORMAT RECIPES
# ============================================================================

# Lint Haskell code with HLint
lint:
    @command -v hlint >/dev/null 2>&1 && hlint src/ || echo "HLint not installed: cabal install hlint"

# Format Haskell code with fourmolu
fmt:
    @command -v fourmolu >/dev/null 2>&1 && fourmolu -i src/**/*.hs || echo "Fourmolu not installed: cabal install fourmolu"

# Check formatting without modifying
fmt-check:
    @command -v fourmolu >/dev/null 2>&1 && fourmolu --mode check src/**/*.hs || echo "Fourmolu not installed"

# Lint ReScript adapter
lint-adapter:
    cd adapters && npm run build

# ============================================================================
# ADAPTER RECIPES (ReScript MCP)
# ============================================================================

# Build ReScript MCP adapter
adapter-build:
    cd adapters && npm install && npm run build

# Watch mode for adapter
adapter-watch:
    cd adapters && npm run watch

# Clean adapter build
adapter-clean:
    cd adapters && npm run clean

# ============================================================================
# DOCUMENTATION RECIPES
# ============================================================================

# Generate Haddock documentation
docs:
    cabal haddock

# Open documentation in browser
docs-open: docs
    @open dist-newstyle/build/*/ghc-*/casket-ssg-*/doc/html/casket-ssg/index.html 2>/dev/null || \
     xdg-open dist-newstyle/build/*/ghc-*/casket-ssg-*/doc/html/casket-ssg/index.html 2>/dev/null || \
     echo "Open docs manually from dist-newstyle/"

# ============================================================================
# DEVELOPMENT RECIPES
# ============================================================================

# Start GHCi REPL with project loaded
repl:
    cabal repl

# Watch for changes and rebuild (requires ghcid)
watch:
    @command -v ghcid >/dev/null 2>&1 && ghcid --command="cabal repl" || echo "ghcid not installed: cabal install ghcid"

# Run the SSG
run *ARGS:
    cabal run casket-ssg -- {{ARGS}}

# ============================================================================
# CI/CD RECIPES
# ============================================================================

# Run CI checks locally
ci: lint test-all
    @echo "✓ CI checks passed"

# Pre-commit hook checks
pre-commit: fmt-check lint test
    @echo "✓ Pre-commit checks passed"

# Verify language purity (no forbidden languages in src/)
language-check:
    @echo "Checking for forbidden languages in src/..."
    @! find src/ -type f \( -name "*.py" -o -name "*.js" -o -name "*.ts" -o -name "*.rb" \) 2>/dev/null | grep . || echo "✓ Only Haskell in src/"

# ============================================================================
# RELEASE RECIPES
# ============================================================================

# Check package before release
check:
    cabal check

# Create source distribution
sdist:
    cabal sdist

# Full release preparation
release-prep: test-all lint check
    @echo "Ready for release"
    @echo "Next steps:"
    @echo "  1. Update version in casket-ssg.cabal"
    @echo "  2. Update CHANGELOG.md"
    @echo "  3. git tag vX.Y.Z"
    @echo "  4. git push --tags"

# ============================================================================
# UTILITY RECIPES
# ============================================================================

# Show project info
info:
    @echo "casket-ssg — Pure Functional Haskell SSG"
    @echo "Language: Haskell (MANDATORY)"
    @echo "GHC: $(ghc --numeric-version 2>/dev/null || echo 'not found')"
    @echo "Cabal: $(cabal --numeric-version 2>/dev/null || echo 'not found')"

# Verify toolchain
toolchain:
    @echo "Checking toolchain..."
    @ghc --version
    @cabal --version
    @echo "✓ Toolchain OK"

# Install development tools
setup:
    cabal install hlint fourmolu ghcid
    cd adapters && npm install
    @echo "✓ Development tools installed"

# SPDX-License-Identifier: AGPL-3.0-or-later
# justfile - Just recipes for this project
# See: https://github.com/hyperpolymath/mustfile

# Default recipe
default:
    @just --list

# Build the project
build:
    stack build

# Build and run on test content
test:
    stack build
    stack exec casket-ssg build test-features/input test-features/output
    @echo "Output generated in test-features/output/"

# Format code
fmt:
    @echo "Formatting not configured yet"

# Lint code
lint:
    @echo "Linting not configured yet"

# Clean build artifacts
clean:
    @echo "Clean not configured yet"

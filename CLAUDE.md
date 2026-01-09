# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

STLC++ is a functional programming language implementation focused on teaching programming language design. It implements System F (polymorphic lambda calculus) with extensions including algebraic data types, IO monad, and custom syntax extensions.

## Build Commands

```bash
# Build the project
cargo build

# Run tests
cargo test

# Run integration tests (requires Nix)
nix-build -A tests

# Build WebAssembly target
wasm-pack build --target web

# Web Playground (Monaco Editor + Vite + Tree-sitter)
cd playground
npm install          # First time only
npm run dev          # Development server at http://localhost:8000
npm run build        # Production build to dist/
npm run preview      # Preview production build

# Build playground with Nix
nix-build -A packages.playground     # Build production bundle
nix-build -A packages.run-playground # Build and run server

# Tree-sitter grammar is fetched from GitHub and built automatically by Nix
# The grammar repo: https://github.com/aalto-opencs/tree-sitter-stlcpp
# To update the grammar version, change the rev and sha256 in default.nix
```

## Running the REPL

```bash
# Start interactive REPL with prelude
cargo run

# Load a module file into REPL
cargo run -- examples/simple.stlc

# Evaluate expression (pipe to stdin)
echo "1 + 2" | cargo run

# Evaluate expression in a module's context
echo "42" | cargo run -- examples/simple.stlc

# Execute module's main function (for IO programs)
cargo run -- --exec examples/io.stlc
```

## Architecture

### Processing Pipeline

The language processing follows this pipeline:
1. **Parsing** → Surface tokens with custom syntax (`Token<Surface>`)
2. **Desugaring** → Core tokens without custom syntax (`Token<Desugared>`)
3. **Type Checking** → Produces `NamedType` (De Bruijn indexed types)
4. **Conversion to Term** → Core evaluation terms
5. **Evaluation** → Multi-step reduction with environment

### Key Types

- **Term tokens** (`src/term/tokens.rs`): AST from parser, parameterized by `Surface` or `Desugared` type state
- **Core Term** (`src/term.rs`): Evaluated lambda calculus terms
- **NamedType** (`src/type/named_type.rs`): De Bruijn indexed types with human-readable names for display
- **Type tokens** (`src/type/tokens.rs`): Parser-level type AST

### Module System

- `Module` contains declarations, type aliases, imports, and syntax extensions
- `ModuleTree` represents the import hierarchy with recursive resolution
- The prelude (`std/prelude.stlc`) is automatically loaded
- `STLCPP_PATH` environment variable controls standard library location

### Custom Syntax

Syntax extensions (`src/syntax.rs`) allow defining custom operators:
```stlc
infixr:5 x :: xs = cons x xs    -- right-associative cons
prefix:8 # xs = length xs       -- prefix length operator
```

Surface tokens (`Token::Infix`, `Token::Prefix`) are desugared into core terms via `desugar()`.

### Type System

Types use De Bruijn indices internally but retain names for display. The `NamedType::Abs` constructor represents `forall` types. Type checking is bidirectional with hole (`_`) support for type inference hints.

### Evaluation

- `Term::step()` performs single-step reduction
- `Term::multistep()` reduces to a value
- `Term::exec()` executes IO actions
- Environment maps variable names to their term values

## File Structure

- `bin/main.rs` - CLI entry point and REPL
- `src/term/` - Term representation, parsing, evaluation, display
- `src/type/` - Type representation, checking, display
- `src/module/` - Module system and imports
- `src/syntax/` - Custom syntax extension system
- `std/` - Standard library modules
- `examples/` - Example programs
- `playground/` - Web playground (Monaco Editor + Vite)

## Web Playground

The web playground uses Monaco Editor (VSCode's editor engine) with tree-sitter for accurate syntax highlighting and a modern Vite-based build system.

### Architecture

- **Editor**: Monaco Editor loaded from CDN
- **Syntax Highlighting**: Hybrid approach
  - Monaco Monarch tokenizer (pattern-based, immediate)
  - Tree-sitter parser (semantic highlighting, async via web-tree-sitter@0.24.7)
  - Graceful fallback to Monarch if tree-sitter fails
- **Compiler**: STLC++ WASM module (built with wasm-pack)
- **Build System**: Vite with WASM plugin
- **Features**: Run code, save/load files, share URLs, example snippets

### File Structure

```
playground/
├── index.html          # Entry point (loads Monaco from CDN)
├── package.json        # npm dependencies (vite, vite-plugin-wasm, web-tree-sitter)
├── vite.config.js      # Vite configuration
├── src/
│   ├── main.js        # Application logic (file management, URL sharing)
│   ├── editor.js      # Monaco setup + tree-sitter integration
│   └── style.css      # Playground styling
├── public/                     # WASM files copied here during build (gitignored)
│   ├── tree-sitter-stlcpp.wasm  # Tree-sitter STLC++ grammar WASM (ABI v14)
│   ├── tree-sitter.wasm         # Tree-sitter runtime WASM (from web-tree-sitter)
│   └── highlights.scm           # Tree-sitter highlighting queries
├── pkg/               # Symlink to ../pkg (WASM compiler)
├── dist/              # Build output (gitignored)
└── README.md          # Development documentation
```

### Syntax Highlighting

**Implementation**: Hybrid tree-sitter + Monarch tokenizer
- Tree-sitter provides semantic highlighting using the official grammar
- Monarch tokenizer provides immediate pattern-based highlighting
- Uses web-tree-sitter@0.24.7 (imported from npm, not CDN)
- Graceful degradation if tree-sitter initialization fails
- See `playground/README.md` for details

### Building the Playground with Nix

```bash
# Build playground (production bundle)
nix-build -A packages.playground

# Run playground with HTTP server
nix-build -A packages.run-playground
./result/bin/run-playground
# Opens at http://localhost:8000
```

### Development Workflow

```bash
# Development
cd playground
npm install
npm run dev          # http://localhost:8000

# Production
npm run build        # Creates dist/ folder
npm run preview      # Preview production build

# The old Python server is deprecated:
# python server.py   # Use npm run dev instead
```

### Features

- **Code Editor**: Monaco with STLC++ syntax highlighting
- **Run Code**: Execute STLC++ code via WASM (Ctrl+Enter)
- **Local Storage**: Save and load files in browser
- **URL Sharing**: Share code via gzip-compressed URLs
- **Examples**: Built-in code snippets (Simple, Factorial, Church Numerals, etc.)
- **File Management**: Create, delete, rename files

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

# Serve web playground
python playground/server.py
```

## Running the REPL

```bash
# Start REPL with prelude
cargo run

# Load a module file
cargo run -- examples/simple.stlc

# Evaluate expression
cargo run -- --eval "1 + 2"

# Execute module's main function
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

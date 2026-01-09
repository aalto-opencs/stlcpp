# STLC++ Playground with Monaco Editor & Tree-Sitter

This playground uses Monaco Editor (VSCode's editor) with the tree-sitter-stlcpp grammar for accurate syntax highlighting.

## Development

```bash
# Install dependencies
npm install

# Run development server (http://localhost:8000)
npm run dev

# Build for production
npm run build

# Preview production build
npm run preview
```

## Architecture

- **Monaco Editor**: Modern code editor with rich features (loaded from CDN)
- **Tree-Sitter**: Parser-based syntax highlighting using the official tree-sitter-stlcpp grammar (web-tree-sitter@0.24.7)
- **Vite**: Fast build tool and dev server with WASM support
- **WASM**: Both the STLC++ compiler and tree-sitter parser run in WebAssembly

## Syntax Highlighting

The playground uses a **hybrid highlighting approach**:

1. **Monaco Monarch Tokenizer** - Fast pattern-based highlighting that works immediately
2. **Tree-Sitter Parser** - Semantic highlighting using the official grammar (initialized async)

If tree-sitter fails to initialize, the playground gracefully falls back to Monarch tokenizer.

## Files

- `src/editor.js` - Monaco + tree-sitter integration
- `src/main.js` - Application logic (URL sharing, file management, examples)
- `src/style.css` - Styling
- `public/tree-sitter-stlcpp.wasm` - Tree-sitter parser for STLC++
- `public/highlights.scm` - Syntax highlighting rules (tree-sitter queries)
- `pkg/` - WASM build of STLC++ compiler (symlink to ../pkg)

## Features

- **Accurate syntax highlighting** using tree-sitter parser with Monarch fallback
- **URL sharing** with gzip compression (codez query parameter)
- **Local storage** for saving files
- **Example snippets** (Simple, Factorial, Church Numerals, and more)
- **Keyboard shortcuts**: Ctrl+Enter to run code
- **File management**: Create, delete, and switch between files

## Building the STLC++ Compiler WASM

The STLC++ compiler WASM is built from the parent directory:

```bash
cd ..
wasm-pack build --target web
cd playground
npm run dev
```

## Building Tree-Sitter Grammar

The tree-sitter grammar is built in the vendor directory:

```bash
cd ../vendor/tree-sitter-stlcpp
tree-sitter generate
tree-sitter build-wasm
cp tree-sitter-stlcpp.wasm ../../playground/public/
```

## Migration Notes

This playground was migrated from CodeMirror 5 (with Haskell syntax highlighting) to Monaco Editor with proper STLC++ tree-sitter support. The old files are preserved as:

- `index.html.old` - Original CodeMirror-based HTML
- `index.js` - Original application logic (functionality migrated to `src/main.js`)

Tree-sitter integration was completed by using the npm package (web-tree-sitter@0.24.7) instead of the CDN version, which resolved previous ABI compatibility issues.

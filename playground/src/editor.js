// Monaco is loaded via script tag and available as window.monaco

// Tree-sitter variables (loaded asynchronously)
let Parser = null;
let parser = null;
let highlightQuery = null;
let stlcppLanguage = null;

/**
 * Initialize tree-sitter parser for STLC++
 * Returns true if successful, false otherwise
 */
export async function initTreeSitter() {
  try {
    // Dynamic import of web-tree-sitter
    const TreeSitterModule = await import('web-tree-sitter');
    Parser = TreeSitterModule.default || TreeSitterModule;

    // Initialize with the WASM file location
    // Use import.meta.env.BASE_URL for correct path in production
    const baseUrl = import.meta.env.BASE_URL || '/';
    await Parser.init({
      locateFile: (filename) => {
        if (filename === 'tree-sitter.wasm') {
          return `${baseUrl}tree-sitter.wasm`;
        }
        return filename;
      }
    });

    parser = new Parser();

    // Load STLC++ language
    stlcppLanguage = await Parser.Language.load(`${baseUrl}tree-sitter-stlcpp.wasm`);
    parser.setLanguage(stlcppLanguage);

    // Load highlight queries
    const highlightsResponse = await fetch(`${baseUrl}highlights.scm`);
    const highlightsText = await highlightsResponse.text();
    highlightQuery = stlcppLanguage.query(highlightsText);

    console.log('Tree-sitter initialized successfully');
    return true;
  } catch (e) {
    console.warn('Failed to initialize tree-sitter:', e);
    console.warn('Using Monarch tokenizer for syntax highlighting');
    return false;
  }
}

/**
 * Wait for Monaco to be loaded on the window object
 */
async function waitForMonaco(maxWaitMs = 10000) {
  const startTime = Date.now();
  while (typeof window.monaco === 'undefined') {
    if (Date.now() - startTime > maxWaitMs) {
      throw new Error('Monaco failed to load within timeout');
    }
    await new Promise(resolve => setTimeout(resolve, 100));
  }
  return window.monaco;
}

/**
 * Register STLC++ language with Monaco (simple token-based highlighting)
 */
export async function registerStlcppLanguage() {
  const monaco = await waitForMonaco();

  // Register language
  monaco.languages.register({ id: 'stlcpp' });

  // Define language configuration (brackets, comments, etc.)
  monaco.languages.setLanguageConfiguration('stlcpp', {
    comments: {
      lineComment: '//',
      blockComment: ['/*', '*/'],
    },
    brackets: [
      ['{', '}'],
      ['[', ']'],
      ['(', ')'],
    ],
    autoClosingPairs: [
      { open: '{', close: '}' },
      { open: '[', close: ']' },
      { open: '(', close: ')' },
      { open: '"', close: '"' },
      { open: "'", close: "'" },
    ],
    surroundingPairs: [
      { open: '{', close: '}' },
      { open: '[', close: ']' },
      { open: '(', close: ')' },
      { open: '"', close: '"' },
      { open: "'", close: "'" },
    ],
  });

  // Register simple token-based syntax highlighting
  monaco.languages.setMonarchTokensProvider('stlcpp', {
    keywords: [
      'fun', 'let', 'in', 'if', 'then', 'else', 'of', 'forall', 'case', 'lcase',
      'nil', 'cons', 'inl', 'inr', 'fix', 'true', 'false', 'Type', 'import',
      'infixl', 'infixr', 'prefix', 'postfix'
    ],
    typeKeywords: ['Bool', 'Integer', 'Int', 'Char', 'String', 'Unit', 'List', 'IO'],
    operators: [
      '->', '=>', '::', '<|', '|>', '.', '+', '-', '*', '/', '==', '!=', '<', '>', '<=', '>='
    ],
    symbols: /[=><!~?:&|+\-*\/\^%]+/,
    escapes: /\\(?:[abfnrtv\\"']|x[0-9A-Fa-f]{1,4}|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})/,

    tokenizer: {
      root: [
        // identifiers and keywords
        [/[a-z_][\w']*/, {
          cases: {
            '@keywords': 'keyword',
            '@default': 'identifier'
          }
        }],
        [/[A-Z][\w']*/, {
          cases: {
            '@typeKeywords': 'type',
            '@default': 'type.identifier'
          }
        }],

        // whitespace
        { include: '@whitespace' },

        // delimiters and operators
        [/[{}()\[\]]/, '@brackets'],
        [/[;,.]/, 'delimiter'],
        [/@symbols/, {
          cases: {
            '@operators': 'operator',
            '@default': ''
          }
        }],

        // numbers
        [/\d+/, 'number'],

        // strings
        [/"([^"\\]|\\.)*$/, 'string.invalid'],
        [/"/, { token: 'string.quote', bracket: '@open', next: '@string' }],

        // characters
        [/'[^\\']'/, 'string'],
        [/(')(@escapes)(')/, ['string', 'string.escape', 'string']],
        [/'/, 'string.invalid']
      ],

      comment: [
        [/[^\/*]+/, 'comment'],
        [/\/\*/, 'comment', '@push'],
        ['\\*/', 'comment', '@pop'],
        [/[\/*]/, 'comment']
      ],

      string: [
        [/[^\\"]+/, 'string'],
        [/@escapes/, 'string.escape'],
        [/\\./, 'string.escape.invalid'],
        [/"/, { token: 'string.quote', bracket: '@close', next: '@pop' }]
      ],

      whitespace: [
        [/[ \t\r\n]+/, 'white'],
        [/\/\*/, 'comment', '@comment'],
        [/\/\/.*$/, 'comment'],
      ],
    },
  });

  console.log('STLC++ language registered with Monaco (Monarch tokenizer)');
}

// Map tree-sitter capture names to Monaco semantic token types
const TOKEN_TYPE_MAP = {
  'keyword': 'keyword',
  'boolean': 'keyword',
  'variable': 'variable',
  'variable.definition': 'variable',
  'type': 'type',
  'type.definition': 'type',
  'type.identifier': 'type',
  'number': 'number',
  'string': 'string',
  'comment': 'comment',
  'operator': 'operator',
  'function.call': 'function',
  'function.builtin': 'function',
  'punctuation.delimiter': 'delimiter',
  'punctuation.bracket': 'bracket',
};

// Semantic token types for Monaco
const SEMANTIC_TOKEN_TYPES = [
  'keyword', 'variable', 'type', 'number', 'string', 'comment',
  'operator', 'function', 'delimiter', 'bracket'
];

/**
 * Register tree-sitter based semantic highlighting with Monaco
 */
export async function registerTreeSitterHighlighting() {
  if (!parser || !highlightQuery) {
    console.warn('Tree-sitter not initialized, skipping semantic highlighting');
    return;
  }

  const monaco = await waitForMonaco();

  // Register semantic tokens legend
  monaco.languages.registerDocumentSemanticTokensProvider('stlcpp', {
    getLegend() {
      return {
        tokenTypes: SEMANTIC_TOKEN_TYPES,
        tokenModifiers: []
      };
    },

    provideDocumentSemanticTokens(model) {
      const code = model.getValue();
      const tree = parser.parse(code);
      const captures = highlightQuery.captures(tree.rootNode);

      // Sort captures by position
      captures.sort((a, b) => {
        if (a.node.startPosition.row !== b.node.startPosition.row) {
          return a.node.startPosition.row - b.node.startPosition.row;
        }
        return a.node.startPosition.column - b.node.startPosition.column;
      });

      const data = [];
      let prevLine = 0;
      let prevChar = 0;

      for (const capture of captures) {
        const node = capture.node;
        const captureName = capture.name;
        const tokenType = TOKEN_TYPE_MAP[captureName];

        if (!tokenType) continue;

        const tokenTypeIndex = SEMANTIC_TOKEN_TYPES.indexOf(tokenType);
        if (tokenTypeIndex === -1) continue;

        const line = node.startPosition.row;
        const char = node.startPosition.column;
        const length = node.endPosition.column - node.startPosition.column;

        // Skip multi-line tokens
        if (node.startPosition.row !== node.endPosition.row) continue;

        const deltaLine = line - prevLine;
        const deltaChar = deltaLine === 0 ? char - prevChar : char;

        data.push(deltaLine, deltaChar, length, tokenTypeIndex, 0);

        prevLine = line;
        prevChar = char;
      }

      return {
        data: new Uint32Array(data),
        resultId: null
      };
    },

    releaseDocumentSemanticTokens() {}
  });

  console.log('Tree-sitter semantic highlighting registered');
}

/**
 * Create and configure Monaco editor instance
 */
export async function createEditor(container, options = {}) {
  const monaco = await waitForMonaco();

  // Define custom theme with token colors (both Monarch and semantic)
  monaco.editor.defineTheme('stlcpp-light', {
    base: 'vs',
    inherit: true,
    rules: [
      // Monarch tokenizer rules (fallback)
      { token: 'keyword', foreground: '0000FF' },
      { token: 'type', foreground: '267F99' },
      { token: 'type.identifier', foreground: '267F99' },
      { token: 'string', foreground: 'A31515' },
      { token: 'number', foreground: '098658' },
      { token: 'comment', foreground: '008000' },
      { token: 'operator', foreground: 'D2691E' },  // Orange
      { token: 'delimiter', foreground: '000000' },
      { token: 'delimiter.bracket', foreground: '000000' },
    ],
    colors: {},
  });

  const editor = monaco.editor.create(container, {
    value: options.value || '',
    language: 'stlcpp',
    theme: 'stlcpp-light',
    automaticLayout: true,
    minimap: { enabled: false },
    fontSize: 14,
    lineNumbers: 'on',
    scrollBeyondLastLine: false,
    wordWrap: 'off',
    ...options,
  });

  return editor;
}

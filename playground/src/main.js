import init, { run } from '../pkg/stlcpp.js';
import { initTreeSitter, registerStlcppLanguage, registerTreeSitterHighlighting, createEditor } from './editor.js';
import './style.css';

const UNTITLED_BASENAME = 'Untitled';

// ============================================================================
// URL Encoding/Decoding Functions
// ============================================================================

function stringToBytes(str) {
  return new TextEncoder().encode(str);
}

function bytesToString(bytes) {
  return new TextDecoder().decode(bytes);
}

function base64ToUrlSafe(base64) {
  return base64.replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');
}

function urlSafeToBase64(urlSafe) {
  let base64 = urlSafe.replace(/-/g, '+').replace(/_/g, '/');
  const pad = base64.length % 4;
  if (pad) {
    base64 += '='.repeat(4 - pad);
  }
  return base64;
}

function encodeCodeGzipped(code) {
  try {
    const bytes = stringToBytes(code);
    const compressed = window.pako.gzip(bytes);
    const base64 = btoa(String.fromCharCode.apply(null, compressed));
    return base64ToUrlSafe(base64);
  } catch (e) {
    console.error('Failed to encode code (gzipped):', e);
    return null;
  }
}

function decodeCodeGzipped(encoded) {
  try {
    const base64 = urlSafeToBase64(encoded);
    const binary = atob(base64);
    const bytes = new Uint8Array(binary.length);
    for (let i = 0; i < binary.length; i++) {
      bytes[i] = binary.charCodeAt(i);
    }
    const decompressed = window.pako.ungzip(bytes);
    return bytesToString(decompressed);
  } catch (e) {
    console.error('Failed to decode code (gzipped):', e);
    return null;
  }
}

function decodeCodePlain(encoded) {
  try {
    // Handle both URL-safe base64 and regular base64 where + became space
    // First convert spaces back to + (in case + was decoded as space by URL parsing)
    let base64 = encoded.replace(/ /g, '+');
    // Then apply URL-safe to standard base64 conversion
    base64 = urlSafeToBase64(base64);
    const binary = atob(base64);
    const bytes = new Uint8Array(binary.length);
    for (let i = 0; i < binary.length; i++) {
      bytes[i] = binary.charCodeAt(i);
    }
    return bytesToString(bytes);
  } catch (e) {
    console.error('Failed to decode code (plain):', e);
    return null;
  }
}

function getCodeFromUrl() {
  const params = new URLSearchParams(window.location.search);

  // Priority: ?codez= (gzipped) > ?code= (plain base64)
  const gzipped = params.get('codez');
  if (gzipped) return decodeCodeGzipped(gzipped);

  const plain = params.get('code');
  if (plain) return decodeCodePlain(plain);

  return null;
}

// ============================================================================
// Local Storage Functions
// ============================================================================

function saveToLocalStorage(name, code) {
  const key = `stlcpp-file:${name}`;
  localStorage.setItem(key, code);
}

function loadFromLocalStorage(name) {
  const key = `stlcpp-file:${name}`;
  return localStorage.getItem(key);
}

function deleteFromLocalStorage(name) {
  const key = `stlcpp-file:${name}`;
  localStorage.removeItem(key);
}

function getLocalStorageFiles() {
  const files = [];
  for (let i = 0; i < localStorage.length; i++) {
    const key = localStorage.key(i);
    if (key && key.startsWith('stlcpp-file:')) {
      const name = key.substring('stlcpp-file:'.length);
      files.push(name);
    }
  }
  return files.sort();
}

function getActiveFileName() {
  return localStorage.getItem('stlcpp-active-file') || null;
}

function setActiveFileName(name) {
  if (name) {
    localStorage.setItem('stlcpp-active-file', name);
  } else {
    localStorage.removeItem('stlcpp-active-file');
  }
}

// ============================================================================
// Example Snippets
// ============================================================================

const EXAMPLES = {
  Simple: `// favorite number
a : Int
a = 123

// favorite function
f : Int -> Int
f = fun x : Int, a
`,
  "List Operations": `main : [Integer]
main =
  let l = 1 :: 2 :: 3 :: 4 :: 5 :: nil Integer in
  map Integer Integer (fun x : Integer, x * 2) l
`,
  "List Operations 2": `a : Integer
a = 5

product : [Integer] -> Integer
product = fun xs : [Integer],
    lcase xs of
    | nil => 1
    | cons x xs => x * product xs

main : Integer
main = product (3 :: 4 :: 5 :: nil Integer)
`,
  Factorial: `fact : Integer -> Integer
fact = fun n : Integer,
  if n == 0 then 1
  else n * fact (n - 1)

main : Integer
main = fact 10
`,
  Polymorphism: `id' : forall A, A -> A
id' = fun A, fun x : A, x

main : Integer
main = id' Integer 42
`,
  Syntax: `infixr f $ x = f x

add1 : Int -> Int
add1 = fun x : Int, x + 1

example : Int
example = (fun x : Int, x) $ add1 $ add1 $ 5

infixr a ! b = a + a * b
prefix & c = c ! c

main : Int
main = & 8
`,
  CSV: `is_newline : Char -> Bool
is_newline = fun c : Char, c == '\\n'

is_comma : Char -> Bool
is_comma = fun c : Char, c == ','

lines : String -> [String]
lines = fun s : String,
  split Char is_newline s

csv : String -> [String]
csv = fun s : String,
  split Char is_comma s

split_rows : [String] -> [[String]]
split_rows = fun ls : [String],
  map String [String] csv ls

main : [[String]]
main = split_rows <| lines "a,b,c,d\\n1,2,,3"
`,
  "Church Numerals": `CBool : Type
CBool = forall T, T -> T -> T

tru : CBool
tru = fun T, fun x : T, fun y : T, x

fals : CBool
fals = fun T, fun x : T, fun y : T, y

cnot : CBool -> CBool
cnot = fun b : CBool, fun T, fun x : T, fun y : T, b T y x

unchurchbool : CBool -> Bool
unchurchbool = fun b : CBool, b Bool true false

CNat : Type
CNat = forall T, (T -> T) -> T -> T

zero : CNat
zero = fun T, fun f : T -> T, fun x : T, x

succ : CNat -> CNat
succ = fun n : CNat, fun T, fun f : T -> T, fun x : T, f (n T f x)

one : CNat
one = succ zero

cadd : CNat -> CNat -> CNat
cadd = fun a : CNat, fun b : CNat, fun T, fun f : T -> T, fun x : T, a T f (b T f x)

unchurchnat : CNat -> Int
unchurchnat = fun n : CNat, n Int (fun x : Int, x + 1) 0

example : Int
example = unchurchnat (cadd one one)
`,
  Ranks: `rank2 : (forall X, X -> X) -> (Int, Bool)
rank2 = fun f : forall X, X -> X, (f Int 2, f Bool true)

rank3 : forall R, ((forall A, A -> A) -> R) -> R
rank3 = fun R, fun h : (forall A, A -> A) -> R, h id

main : (Int, Bool)
main = rank3 (Int, Bool) rank2
`,
};

// ============================================================================
// Main Application
// ============================================================================

async function main() {
  // Initialize WASM modules
  await init();

  // Initialize tree-sitter (will fall back to Monarch if it fails)
  const treeSitterOk = await initTreeSitter();

  // Register STLC++ language with Monaco (Monarch tokenizer as fallback)
  await registerStlcppLanguage();

  // Register tree-sitter semantic highlighting if available
  if (treeSitterOk) {
    await registerTreeSitterHighlighting();
  }

  // Create editor
  const editorContainer = document.getElementById('editor-container');
  const editor = await createEditor(editorContainer, {
    value: '// Loading...',
  });

  const output = document.getElementById('output-container');
  let currentFileName = null;

  // Load initial code
  const urlCode = getCodeFromUrl();
  if (urlCode) {
    editor.setValue(urlCode);
  } else {
    const activeFile = getActiveFileName();
    if (activeFile && loadFromLocalStorage(activeFile)) {
      currentFileName = activeFile;
      editor.setValue(loadFromLocalStorage(activeFile));
    } else {
      editor.setValue('// Write your STLC++ code here\nmain : IO Unit\nmain = io.print "Hello"');
    }
  }

  // Setup run button
  const runBtn = document.getElementById('run-btn');
  runBtn.addEventListener('click', () => {
    const code = editor.getValue();
    output.textContent = 'Running...\\n';
    try {
      const result = run(code);
      output.textContent = result;
    } catch (e) {
      output.textContent = `Error: ${e}`;
    }
  });

  // Setup share button
  const shareBtn = document.getElementById('share-btn');
  shareBtn.addEventListener('click', () => {
    const code = editor.getValue();
    const encoded = encodeCodeGzipped(code);
    if (encoded) {
      const url = new URL(window.location.href);
      url.searchParams.set('codez', encoded);
      navigator.clipboard.writeText(url.toString()).then(() => {
        alert('Share URL copied to clipboard!');
      });
    }
  });

  // Track pending example (when user clicks an example but hasn't modified it yet)
  let pendingExampleName = null;
  let pendingExampleCode = null;

  function resetPendingExample() {
    pendingExampleName = null;
    pendingExampleCode = null;
  }

  function nextAvailableCopyName(baseName) {
    const files = getLocalStorageFiles();
    let name = `${baseName} (copy)`;
    let counter = 2;
    while (files.includes(name)) {
      name = `${baseName} (copy ${counter})`;
      counter++;
    }
    return name;
  }

  // Setup example buttons
  const exampleList = document.getElementById('example-list');
  for (const [name, code] of Object.entries(EXAMPLES)) {
    const btn = document.createElement('button');
    btn.className = 'example-btn';
    btn.textContent = name;
    btn.addEventListener('click', () => {
      editor.setValue(code);
      currentFileName = null;
      setActiveFileName(null);
      // Track this example so we can create a copy if user modifies it
      pendingExampleName = name;
      pendingExampleCode = code;
    });
    exampleList.appendChild(btn);
  }

  // Setup local file list
  const localStorageList = document.getElementById('local-storage-list');
  function updateFileList() {
    localStorageList.innerHTML = '';
    const files = getLocalStorageFiles();
    for (const name of files) {
      const btn = document.createElement('button');
      btn.className = 'example-btn';
      btn.textContent = name;
      btn.addEventListener('click', () => {
        const code = loadFromLocalStorage(name);
        if (code !== null) {
          editor.setValue(code);
          currentFileName = name;
          setActiveFileName(name);
          resetPendingExample();
        }
      });
      localStorageList.appendChild(btn);
    }
  }
  updateFileList();

  // Setup file operations
  const fileNewBtn = document.getElementById('file-new');
  if (fileNewBtn) {
    fileNewBtn.addEventListener('click', () => {
      const name = prompt('Enter file name:');
      if (name) {
        currentFileName = name;
        editor.setValue('');
        saveToLocalStorage(name, '');
        setActiveFileName(name);
        resetPendingExample();
        updateFileList();
      }
    });
  }

  const fileDeleteBtn = document.getElementById('file-delete');
  if (fileDeleteBtn) {
    fileDeleteBtn.addEventListener('click', () => {
      if (currentFileName) {
        if (confirm(`Delete file "${currentFileName}"?`)) {
          deleteFromLocalStorage(currentFileName);
          currentFileName = null;
          setActiveFileName(null);
          editor.setValue('');
          updateFileList();
        }
      } else {
        alert('No file selected');
      }
    });
  }

  // Get next available Untitled name
  function nextUntitledName() {
    const files = getLocalStorageFiles();
    let name = UNTITLED_BASENAME;
    let counter = 2;
    while (files.includes(name)) {
      name = `${UNTITLED_BASENAME} ${counter}`;
      counter++;
    }
    return name;
  }

  // Auto-save current file on change
  let saveTimeout;
  editor.onDidChangeModelContent(() => {
    clearTimeout(saveTimeout);
    saveTimeout = setTimeout(() => {
      const current = editor.getValue();

      // If editing a pending example and content changed, create a copy
      if (pendingExampleName && pendingExampleCode != null) {
        if (current !== pendingExampleCode) {
          const name = nextAvailableCopyName(pendingExampleName);
          saveToLocalStorage(name, current);
          currentFileName = name;
          setActiveFileName(name);
          resetPendingExample();
          updateFileList();
        }
        return;
      }

      // If we have a current file, save to it
      if (currentFileName) {
        saveToLocalStorage(currentFileName, editor.getValue());
        return;
      }

      // No current file and non-empty content: create an Untitled file
      if (current.trim() !== '') {
        const name = nextUntitledName();
        saveToLocalStorage(name, current);
        currentFileName = name;
        setActiveFileName(name);
        updateFileList();
      }
    }, 500);
  });

  // Keyboard shortcut: Ctrl+Enter to run
  // Monaco is available on window after loading
  editor.addCommand(window.monaco.KeyMod.CtrlCmd | window.monaco.KeyCode.Enter, () => {
    runBtn.click();
  });

  console.log('Playground initialized successfully');
}

main().catch(console.error);

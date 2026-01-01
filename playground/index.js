import init, { run } from "./pkg/stlcpp.js";

const UNTITLED_BASENAME = "Untitled";

/**
 * UI layout modes:
 * - "vertical": editor on top, output below (default)
 * - "horizontal": editor left, output right
 */
const STORAGE_LAYOUT_MODE_KEY = "stlcpp.playground.ui.layout_mode.v1";
const LAYOUT_VERTICAL = "vertical";
const LAYOUT_HORIZONTAL = "horizontal";

function getLayoutMode() {
  try {
    const raw = localStorage.getItem(STORAGE_LAYOUT_MODE_KEY);
    if (raw === LAYOUT_HORIZONTAL || raw === LAYOUT_VERTICAL) return raw;
  } catch (_e) {
    // ignore
  }
  return LAYOUT_VERTICAL;
}

function setLayoutMode(mode) {
  try {
    localStorage.setItem(STORAGE_LAYOUT_MODE_KEY, mode);
  } catch (_e) {
    // ignore
  }
}

function applyLayoutMode(mode) {
  // These elements are defined in index.html (no-op if missing)
  const main = document.getElementById("main-content");
  if (!main) return;

  main.classList.toggle("layout-horizontal", mode === LAYOUT_HORIZONTAL);
  main.classList.toggle("layout-vertical", mode === LAYOUT_VERTICAL);

  const btn = document.getElementById("layout-toggle");
  if (btn) {
    // Button text indicates what clicking will switch to.
    btn.textContent = mode === LAYOUT_HORIZONTAL ? "Vertical" : "Horizontal";
  }

  // CodeMirror needs a refresh after layout changes to measure correctly.
  // The caller will trigger it once the editor exists.
}

const examples = {
  Simple: `a : Integer
a = 5

product : [Integer] -> Integer
product = fun xs : [Integer],
    lcase xs of
    | nil => 1
    | cons x xs => x * product xs

main : Integer
main = product (3 :: 4 :: 5 :: nil Integer)
`,
  "List Operations": `main : [Integer]
main =
  let l = 1 :: 2 :: 3 :: 4 :: 5 :: nil Integer in
  map Integer Integer (fun x : Integer, x * 2) l
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
};

const STORAGE_ACTIVE_FILE_KEY = "stlcpp.playground.files.active.v1";
const STORAGE_UNTITLED_COUNTER_KEY =
  "stlcpp.playground.files.untitled_counter.v1";
const STORAGE_FILES_INDEX_KEY = "stlcpp.playground.files.index.v1";
const STORAGE_FILE_PREFIX = "stlcpp.playground.files.file.v1:";

function safeStorageGet(key) {
  try {
    return localStorage.getItem(key);
  } catch (_e) {
    return null;
  }
}
function safeStorageSet(key, value) {
  try {
    localStorage.setItem(key, value);
    return true;
  } catch (_e) {
    return false;
  }
}
function safeStorageRemove(key) {
  try {
    localStorage.removeItem(key);
  } catch (_e) {
    // ignore
  }
}

function normalizeFileName(name) {
  return String(name || "")
    .replace(/\s+/g, " ")
    .trim()
    .slice(0, 80);
}

function nextUntitledName() {
  let n = 0;
  try {
    const raw = safeStorageGet(STORAGE_UNTITLED_COUNTER_KEY);
    if (raw != null) n = Number(raw) || 0;
  } catch (_e) {
    n = 0;
  }
  n = n + 1;
  safeStorageSet(STORAGE_UNTITLED_COUNTER_KEY, String(n));

  if (n === 1) return UNTITLED_BASENAME;
  return `${UNTITLED_BASENAME} ${n}`;
}

function escapeRegExp(s) {
  return String(s).replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

function makeCopyBaseName(exampleName) {
  return normalizeFileName(`${exampleName} (copy)`);
}

function nextAvailableCopyName(exampleName) {
  const base = makeCopyBaseName(exampleName);
  const files = loadFilesIndex();

  if (!files.includes(base)) return base;

  const re = new RegExp(`^${escapeRegExp(base)} (\\d+)$`);
  let maxN = 1;
  for (const f of files) {
    const m = re.exec(f);
    if (!m) continue;
    const n = Number(m[1]);
    if (Number.isFinite(n) && n > maxN) maxN = n;
  }
  return normalizeFileName(`${base} ${maxN + 1}`);
}

function loadFilesIndex() {
  const raw = safeStorageGet(STORAGE_FILES_INDEX_KEY);
  if (!raw) return [];
  try {
    const parsed = JSON.parse(raw);
    if (!Array.isArray(parsed)) return [];
    return parsed.filter((x) => typeof x === "string");
  } catch (_e) {
    return [];
  }
}

function saveFilesIndex(names) {
  const uniq = Array.from(new Set(names.filter((x) => typeof x === "string")));
  safeStorageSet(STORAGE_FILES_INDEX_KEY, JSON.stringify(uniq));
  return uniq;
}

function fileKey(name) {
  return STORAGE_FILE_PREFIX + name;
}

function listLocalFiles() {
  return loadFilesIndex().sort((a, b) => a.localeCompare(b));
}

function readLocalFile(name) {
  const raw = safeStorageGet(fileKey(name));
  if (raw == null) return null;
  return raw;
}

function writeLocalFile(name, text) {
  const n = normalizeFileName(name);
  if (!n) return { ok: false, error: "Invalid name" };

  const idx = loadFilesIndex();
  if (!idx.includes(n)) idx.push(n);
  saveFilesIndex(idx);

  if (!safeStorageSet(fileKey(n), text)) {
    return { ok: false, error: "Failed to write to localStorage" };
  }
  return { ok: true, name: n };
}

function deleteLocalFile(name) {
  const idx = loadFilesIndex().filter((n) => n !== name);
  saveFilesIndex(idx);
  safeStorageRemove(fileKey(name));
  const active = safeStorageGet(STORAGE_ACTIVE_FILE_KEY);
  if (active === name) safeStorageRemove(STORAGE_ACTIVE_FILE_KEY);
}

function renameLocalFile(oldName, newName) {
  const src = readLocalFile(oldName);
  if (src == null) return { ok: false, error: "File not found" };

  const n = normalizeFileName(newName);
  if (!n) return { ok: false, error: "Invalid name" };

  if (n === oldName) return { ok: true, name: oldName };

  // Prevent overwrite
  const exists = readLocalFile(n) != null;
  if (exists)
    return { ok: false, error: "A file with that name already exists" };

  const w = writeLocalFile(n, src);
  if (!w.ok) return w;

  deleteLocalFile(oldName);

  const active = safeStorageGet(STORAGE_ACTIVE_FILE_KEY);
  if (active === oldName) safeStorageSet(STORAGE_ACTIVE_FILE_KEY, n);

  return { ok: true, name: n };
}

function setFileName(name) {
  const el = document.getElementById("file-name");
  if (el) el.textContent = name;
}

function setActiveFileName(name) {
  if (name) safeStorageSet(STORAGE_ACTIVE_FILE_KEY, name);
  else safeStorageRemove(STORAGE_ACTIVE_FILE_KEY);
  setFileName(name || "");
}

function getActiveFileName() {
  const n = safeStorageGet(STORAGE_ACTIVE_FILE_KEY);
  if (!n) return null;
  return n;
}

function clearScratchBuffer() {
  // Fully detach from any persisted file and make the editor a scratch buffer.
  setActiveFileName(null);
  setFileName("");
}

function ensureUntitledActiveFile() {
  // If no active file is selected, create/select an Untitled file on-demand.
  let active = getActiveFileName();
  if (active) return active;

  const name = normalizeFileName(nextUntitledName());
  const res = writeLocalFile(name, "");
  if (!res.ok) return null;

  setActiveFileName(res.name);
  return res.name;
}

async function main() {
  await init();

  const editor = CodeMirror.fromTextArea(
    document.getElementById("code-editor"),
    {
      lineNumbers: true,
      mode: "haskell",
      theme: "default",
      viewportMargin: Infinity,
    },
  );

  const output = document.getElementById("output-container");
  const exampleList = document.getElementById("example-list");
  const localStorageList = document.getElementById("local-storage-list");

  // File UI elements (optional; index.html may omit them)
  const fileNewBtn = document.getElementById("file-new");
  const fileOpenBtn = document.getElementById("file-open-btn");
  const fileOpenInput = document.getElementById("file-open");
  const fileDeleteBtn = document.getElementById("file-delete");

  // Layout UI (optional; index.html may omit it)
  const layoutToggleBtn = document.getElementById("layout-toggle");

  // Apply persisted layout mode before wiring handlers
  let layoutMode = getLayoutMode();
  applyLayoutMode(layoutMode);

  // Track transient example state:
  // Clicking an example loads it into the editor, but does NOT create a copy until the first edit.
  //
  // Note: We must also remember what file was active before the example was selected.
  // Otherwise, editing an example could accidentally overwrite the previously active file.
  let pendingExampleName = null;
  let pendingExampleCode = null;
  let pendingPreviousActiveFileName = null;

  function resetPendingExample() {
    pendingExampleName = null;
    pendingExampleCode = null;
    pendingPreviousActiveFileName = null;
  }

  // HACK: Capture panic-hook output so we can display it in the UI.
  // Note: this does not change the thrown exception (still "unreachable executed").
  let lastRustPanicMessage = null;
  const originalConsoleError = console.error.bind(console);
  console.error = (...args) => {
    originalConsoleError(...args);
    // Keep it simple: the panic hook usually logs a string as its first arg.
    lastRustPanicMessage = args[0];
  };

  // Layout toggle behavior
  if (layoutToggleBtn) {
    layoutToggleBtn.onclick = () => {
      layoutMode =
        layoutMode === LAYOUT_HORIZONTAL ? LAYOUT_VERTICAL : LAYOUT_HORIZONTAL;
      setLayoutMode(layoutMode);
      applyLayoutMode(layoutMode);

      // Refresh CodeMirror layout after DOM reflow
      setTimeout(() => editor.refresh(), 0);
    };
  }

  function runCode() {
    output.textContent = "Running...";
    lastRustPanicMessage = null;

    try {
      const code = editor.getValue();
      const result = run(code);
      output.textContent = result;
    } catch (e) {
      if (lastRustPanicMessage) {
        output.textContent = lastRustPanicMessage;
      } else {
        output.textContent = "Error: " + e;
      }
    }
  }

  function renderLocalFiles() {
    if (!localStorageList) return;

    localStorageList.innerHTML = "";
    const files = listLocalFiles();

    if (files.length === 0) {
      const empty = document.createElement("div");
      empty.style.color = "#bdc3c7";
      empty.style.fontSize = "0.9rem";
      empty.style.padding = "6px 10px";
      empty.textContent = "No files saved";
      localStorageList.appendChild(empty);
      return;
    }

    const active = getActiveFileName();

    for (const name of files) {
      const row = document.createElement("div");
      row.className = "local-file-row";

      const label = document.createElement("div");
      label.className = "local-file-name";
      label.textContent = name;

      if (active === name) {
        label.style.opacity = "1";
      }

      const actions = document.createElement("div");
      actions.className = "local-file-actions";

      const renameBtn = document.createElement("button");
      renameBtn.className = "local-file-action-btn";
      renameBtn.type = "button";
      renameBtn.textContent = "Rename";

      const delBtn = document.createElement("button");
      delBtn.className = "local-file-action-btn danger";
      delBtn.type = "button";
      delBtn.textContent = "Delete";

      actions.appendChild(renameBtn);
      actions.appendChild(delBtn);

      row.appendChild(label);
      row.appendChild(actions);

      // Clicking the row opens that file as the active buffer
      row.addEventListener("click", () => {
        const text = readLocalFile(name);
        if (text == null) return;
        editor.setValue(text);
        setActiveFileName(name);

        // IMPORTANT: switching to an existing local file must cancel any pending example,
        // otherwise the next edit could incorrectly create a new "<Example> (copy N)".
        resetPendingExample();

        // Match example button behavior: run immediately on selection
        runCode();
      });

      // Prevent row click when using action buttons
      renameBtn.addEventListener("click", (e) => {
        e.preventDefault();
        e.stopPropagation();

        const next = prompt("Rename file:", name);
        if (next == null) return;

        const res = renameLocalFile(name, next);
        if (!res.ok) {
          output.textContent = "Error: " + res.error;
          return;
        }

        renderLocalFiles();
      });

      delBtn.addEventListener("click", (e) => {
        e.preventDefault();
        e.stopPropagation();

        const active = getActiveFileName();
        const deletingActive = active === name;

        // Delete immediately (no confirmation)
        deleteLocalFile(name);

        if (deletingActive) {
          // Clear CodeMirror reliably and detach from any persisted file.
          editor.operation(() => {
            editor.getDoc().setValue("");
            editor.getDoc().clearHistory();
          });
          editor.focus();

          clearScratchBuffer();
          pendingExampleName = null;
          pendingExampleCode = null;
          pendingPreviousActiveFileName = null;
        }

        renderLocalFiles();

        const nowActive = getActiveFileName();
        if (!nowActive) setFileName("");
      });

      localStorageList.appendChild(row);
    }
  }

  // Populate examples (static; selecting an example loads it into editor; copy is created on first edit)
  for (const [name, code] of Object.entries(examples)) {
    const btn = document.createElement("button");
    btn.className = "example-btn";
    btn.textContent = name;
    btn.onclick = () => {
      // Remember the currently active file so we can restore it if the user never edits
      // the example, and so we don't accidentally overwrite it.
      pendingPreviousActiveFileName = getActiveFileName();

      pendingExampleName = name;
      pendingExampleCode = code;

      editor.setValue(code);

      // Important: do not change the active file on example click. We only create a copy on first edit.
      // Also: no Unnamed buffer anymore.
      setFileName(`${name} (example)`);
      renderLocalFiles();
      runCode();
    };
    exampleList.appendChild(btn);
  }

  // Startup: always restore the active file if present.
  // If missing, start with a default example view but do not create a file.
  const active = getActiveFileName();
  if (active) {
    const text = readLocalFile(active);
    if (typeof text === "string") {
      editor.setValue(text);
      setFileName(active);
    } else {
      // Active file missing; clear active selection and show default example
      setActiveFileName(null);
      editor.setValue(examples["Simple"]);
      setFileName("Simple (example)");
      pendingExampleName = "Simple";
      pendingExampleCode = examples["Simple"];
    }
  } else {
    editor.setValue(examples["Simple"]);
    setFileName("Simple (example)");
    pendingExampleName = "Simple";
    pendingExampleCode = examples["Simple"];
  }

  renderLocalFiles();

  // Autosave edits:
  // - Only act on *user edits* (ignore programmatic editor.setValue calls)
  // - Always persist to the *active named file*.
  // - If an example is pending and the user changes it: create a uniquely named "<Example> (copy N)" and switch to it.
  // - If no active file exists and the user edits non-empty content: create an Untitled file on-demand and save into it.
  editor.on("change", function (_cm, change) {
    // CodeMirror calls "change" both for user input and for programmatic setValue.
    // We only want to autosave / create example copies due to *user actions*.
    const origin = change && change.origin;

    // Ignore programmatic loads
    if (origin === "setValue" || origin === "load") {
      return;
    }

    const current = editor.getValue();
    const activeName = getActiveFileName();

    if (pendingExampleName && pendingExampleCode != null) {
      // Only create a copy once the user actually changes something.
      if (current !== pendingExampleCode) {
        const name = nextAvailableCopyName(pendingExampleName);
        const res = writeLocalFile(name, current);
        if (res.ok) {
          setActiveFileName(res.name);
          resetPendingExample();
          renderLocalFiles();
          return;
        }
        output.textContent = "Error: failed to create example copy";
        return;
      } else {
        // Still identical to the example; do nothing.
        return;
      }
    }

    if (activeName) {
      writeLocalFile(activeName, current);
      setFileName(activeName);
      renderLocalFiles();
    } else if (current.trim() !== "") {
      const ensured = ensureUntitledActiveFile();
      if (ensured) {
        writeLocalFile(ensured, current);
        setFileName(ensured);
        renderLocalFiles();
      } else {
        setFileName("");
      }
    } else {
      setFileName("");
    }
  });

  // File actions
  if (fileNewBtn) {
    fileNewBtn.onclick = () => {
      // Clear to a scratch buffer (no auto-persist).
      editor.setValue("");
      resetPendingExample();
      clearScratchBuffer();
      renderLocalFiles();
    };
  }

  if (fileDeleteBtn) {
    fileDeleteBtn.onclick = () => {
      const active = getActiveFileName();
      if (!active) return;

      // Delete immediately (no confirmation)
      deleteLocalFile(active);

      // Clear to scratch buffer and do not auto-create any file
      resetPendingExample();

      // Clear CodeMirror reliably. Sometimes setValue alone can be followed by
      // other state updates (or change handlers) that restore content.
      editor.operation(() => {
        editor.getDoc().setValue("");
        editor.getDoc().clearHistory();
      });
      editor.focus();

      clearScratchBuffer();
      renderLocalFiles();
    };
  }

  if (fileOpenBtn && fileOpenInput) {
    fileOpenBtn.onclick = () => {
      fileOpenInput.value = "";
      fileOpenInput.click();
    };

    fileOpenInput.addEventListener("change", async () => {
      const f = fileOpenInput.files && fileOpenInput.files[0];
      if (!f) return;

      try {
        const text = await f.text();
        const base = normalizeFileName(
          (f.name || "Imported").replace(/\.[^/.]+$/, ""),
        );
        const name = base || "Imported";
        writeLocalFile(name, text);
        editor.setValue(text);
        setActiveFileName(name);
        pendingExampleName = null;
        pendingExampleCode = null;
        renderLocalFiles();
      } catch (e) {
        output.textContent = "Error: failed to read file";
        console.error(e);
      }
    });
  }

  document.getElementById("run-btn").onclick = runCode;

  editor.setOption("extraKeys", {
    "Ctrl-Enter": function (_cm) {
      runCode();
    },
  });
}

main();

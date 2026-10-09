import test from 'node:test';
import assert from 'node:assert/strict';
import { rollup } from 'rollup';
import { mkdtemp, writeFile, readFile, rm } from 'node:fs/promises';
import { join } from 'node:path';
import { pathToFileURL } from 'node:url';
import { tmpdir } from 'node:os';

test('playground bundling retains lazy imports and source content while removing unused code', async () => {
  const root = await mkdtemp(join(tmpdir(), 'playground-rollup-'));
  let bundle;
  try {
    await writeFile(join(root, 'entry.js'), 'export { label } from "./values.js"; export const load = () => import("./lazy.js");');
    await writeFile(join(root, 'values.js'), 'export const label = "λ playground"; export const unused = () => "UNUSED_SENTINEL";');
    await writeFile(join(root, 'lazy.js'), 'export const answer = 42;');
    bundle = await rollup({ input: join(root, 'entry.js') });
    const result = await bundle.write({ dir: join(root, 'dist'), format: 'es', sourcemap: true,
      entryFileNames: 'main.mjs', chunkFileNames: 'chunks/[name]-[hash].mjs' });
    const entry = result.output.find((item) => item.type === 'chunk' && item.isEntry);
    assert.doesNotMatch(entry.code, /UNUSED_SENTINEL/);
    const compiled = await import(pathToFileURL(join(root, 'dist/main.mjs')).href);
    assert.equal(compiled.label, 'λ playground');
    assert.equal((await compiled.load()).answer, 42);
    const map = JSON.parse(await readFile(join(root, 'dist/main.mjs.map'), 'utf8'));
    assert.ok(map.sourcesContent.some((source) => source.includes('λ playground')));
    assert.equal(result.output.filter((item) => item.type === 'chunk').length, 2);
  } finally {
    await bundle?.close();
    await rm(root, { recursive: true, force: true });
  }
});

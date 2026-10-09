import test from 'node:test';
import assert from 'node:assert/strict';
import { createServer, build } from 'vite';
import { mkdtemp, writeFile, rm } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';

test('playground development serves cross-origin isolation headers and denies secret files', async () => {
  const server = await createServer({ server: { host: '127.0.0.1', port: 0, open: false }, logLevel: 'silent' });
  try {
    await server.listen();
    const base = `http://127.0.0.1:${server.httpServer.address().port}`;
    const response = await fetch(`${base}/src/style.css`);
    assert.equal(response.status, 200);
    assert.equal(response.headers.get('cross-origin-embedder-policy'), 'require-corp');
    assert.equal(response.headers.get('cross-origin-opener-policy'), 'same-origin');
    // Vite's deny rule must apply even within the configured filesystem allow list.
    await writeFile('.env.contract-test', 'REGRESSION_SENTINEL=do-not-serve');
    const secret = await fetch(`${base}/@fs/${join(process.cwd(), '.env.contract-test')}`);
    assert.equal(secret.status, 403);
    assert.doesNotMatch(await secret.text(), /REGRESSION_SENTINEL=do-not-serve/);
  } finally {
    await server.close();
    await rm('.env.contract-test', { force: true });
  }
});

test('production bundling preserves module exports and CSS assets', async () => {
  const root = await mkdtemp(join(tmpdir(), 'stlcpp-vite-'));
  try {
    await writeFile(join(root, 'style.css'), '.editor { color: rebeccapurple }');
    await writeFile(join(root, 'entry.js'), 'import "./style.css"; export const evaluate = () => 42;');
    const result = await build({ root, configFile: false, logLevel: 'silent', build: {
      write: false, cssMinify: false, lib: { entry: join(root, 'entry.js'), formats: ['es'], cssFileName: 'contract' },
    } });
    const output = result[0].output;
    const chunk = output.find((item) => item.type === 'chunk');
    const module = await import(`data:text/javascript;base64,${Buffer.from(chunk.code).toString('base64')}`);
    assert.equal(module.evaluate(), 42);
    assert.ok(output.some((item) => item.type === 'asset' && item.fileName.endsWith('.css') && /rebeccapurple/.test(item.source)));
  } finally { await rm(root, { recursive: true, force: true }); }
});

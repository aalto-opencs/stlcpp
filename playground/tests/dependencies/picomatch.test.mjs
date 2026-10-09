import test from 'node:test';
import assert from 'node:assert/strict';
import picomatch from 'picomatch';
import { build } from 'vite';
import { mkdtemp, writeFile, mkdir, rm } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';

test('Vite glob imports include supported source files and exclude tests', async () => {
  const root = await mkdtemp(join(tmpdir(), 'stlcpp-glob-'));
  try {
    await mkdir(join(root, 'examples'));
    await writeFile(join(root, 'examples', 'lambda.js'), 'export default "lambda";');
    await writeFile(join(root, 'examples', 'ignored.test.js'), 'export default "must-not-ship";');
    await writeFile(join(root, 'entry.js'), 'export const examples = import.meta.glob(["./examples/*.js", "!./examples/*.test.js"], {eager:true});');
    const result = await build({ root, configFile: false, logLevel: 'silent', build: {
      write: false, minify: false, lib: { entry: join(root, 'entry.js'), formats: ['es'] },
    } });
    const code = result[0].output.find((item) => item.type === 'chunk').code;
    assert.match(code, /lambda/);
    assert.doesNotMatch(code, /must-not-ship/);
  } finally { await rm(root, { recursive: true, force: true }); }
});

test('glob matching handles nested paths, alternatives and literal punctuation', () => {
  const match = picomatch('src/**/*.{js,css}', { ignore: '**/*.test.js' });
  assert.equal(match('src/nested/editor.js'), true);
  assert.equal(match('src/style.css'), true);
  assert.equal(match('src/editor.test.js'), false);
  assert.equal(match('outside/editor.js'), false);
  assert.equal(picomatch('src/file\\[1\\].js')('src/file[1].js'), true);
});

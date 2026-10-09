import test from 'node:test';
import assert from 'node:assert/strict';
import { nanoid } from 'nanoid';
import postcss from 'postcss';

test('PostCSS Input IDs remain nonempty and distinct for anonymous stylesheets', () => {
  const first = postcss.parse('.editor { color: red }').source.input;
  const second = postcss.parse('.editor { color: blue }').source.input;
  assert.match(first.id, /^<input css .+>$/);
  assert.notEqual(first.id, second.id);
  assert.equal(first.css, '.editor { color: red }');
});

test('generated stylesheet identifiers preserve the requested size and URL-safe alphabet', () => {
  for (const size of [0, 6, 21, 64]) {
    const id = nanoid(size);
    assert.equal(id.length, size);
    assert.match(id, /^[A-Za-z0-9_-]*$/);
  }
});

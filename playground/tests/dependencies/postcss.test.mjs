import test from 'node:test';
import assert from 'node:assert/strict';
import postcss from 'postcss';
import { readFile } from 'node:fs/promises';

test('the actual stylesheet survives parsing and serialization without changing CSS', async () => {
  const css = await readFile(new URL('../../src/style.css', import.meta.url), 'utf8');
  const root = postcss.parse(css, { from: 'src/style.css' });
  assert.equal(root.toString(), css);
  let declarations = 0;
  root.walkDecls(() => declarations++);
  assert.ok(declarations > 0);
});

test('plugins transform declarations and emit usable source maps', async () => {
  const result = await postcss([{ postcssPlugin: 'contract-colors', Declaration(declaration) {
    if (declaration.prop === 'color') declaration.value = 'blue';
  } }]).process('.editor { color: red; --label: "a;b{}" }', {
    from: 'src/style.css', to: 'dist/style.css', map: { inline: false, annotation: false },
  });
  assert.match(result.css, /color: blue/);
  assert.match(result.css, /--label: "a;b\{\}"/);
  const map = result.map.toJSON();
  assert.equal(map.version, 3);
  assert.ok(map.sources.some((source) => source.endsWith('src/style.css')));
  assert.ok(map.mappings.length > 0);
  assert.throws(() => postcss.parse('.editor { color: "unterminated }'), { name: 'CssSyntaxError' });
});

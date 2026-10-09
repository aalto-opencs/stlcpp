import assert from "node:assert/strict";
import { test } from "node:test";
import { createRequire } from "node:module";
import postcss from "postcss";

// Resolve the same consumer PostCSS uses, including if npm nests the dependency.
const require = createRequire(import.meta.resolve("postcss"));
const { SourceMapConsumer } = require("source-map-js");

test("chained CSS transforms retain original locations and Unicode source content", async () => {
  const input = '.course::before {\n  content: "Näytä 📚";\n  margin: 0;\n}\n';
  const first = await postcss([{
    postcssPlugin: "rename-course-selector",
    Rule(rule) { rule.selector = ".course-material::before"; },
  }]).process(input, { from: "course.css", to: "intermediate.css", map: { inline: false } });
  const second = await postcss([{
    postcssPlugin: "expand-course-spacing",
    Declaration(decl) { if (decl.prop === "margin") decl.value = "1rem"; },
  }]).process(first.css, {
    from: "intermediate.css", to: "built.css",
    map: { inline: false, prev: first.map.toJSON() },
  });
  assert.match(second.css, /\.course-material::before/);
  assert.match(second.css, /margin: 1rem/);
  const lines = second.css.split("\n");
  const line = lines.findIndex((text) => text.includes("margin:"));
  const consumer = new SourceMapConsumer(second.map.toJSON());
  const original = consumer.originalPositionFor({ line: line + 1, column: lines[line].indexOf("margin:") });
  assert.match(original.source, /(^|\/)course\.css$/);
  assert.equal(original.line, 3);
  assert.equal(original.column, 2);
  assert.equal(consumer.sourceContentFor(original.source), input);
});

// 1.2.2 rejects invalid indexed offsets before they can expand into huge maps.
// Only construct the consumer: never attempt the expensive vulnerable expansion.
test("indexed source maps reject invalid section offsets before processing", () => {
  for (const line of [10_000_001, -1, 1.5, Infinity]) {
    assert.throws(() => new SourceMapConsumer({
      version: 3,
      sections: [{
        offset: { line, column: 0 },
        map: { version: 3, sources: ["course.css"], names: [], mappings: "AAAA" },
      }],
    }), /offset/i);
  }
});

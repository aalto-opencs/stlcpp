import test from 'node:test';
import assert from 'node:assert/strict';
import { existsSync, readFileSync } from 'node:fs';
import { actionVersionsOnly, evaluatePullRequest, evaluateChecks, processPullRequest } from './dependabot.mjs';

const repository = 'aalto-opencs/example';
const workflow = '.github/workflows/test.yml';
const policy = '.github/workflows/dependabot-policy.yml';
const bot = { login: 'dependabot[bot]', id: 49699333 };
const config = { baseBranch: 'main', coursePaths: ['materials', 'assignment-repository'],
  dependencyFiles: { 'package.json': [workflow], 'package-lock.json': [workflow], 'Dockerfile': [workflow] },
  actionWorkflows: { [workflow]: [workflow] }, workflows: { [workflow]: ['^tests$'] } };
function fixture() {
  const pr = { number: 1, state: 'open', draft: false, user: bot, changed_files: 1, commits: 1,
    mergeable: true, base: { ref: 'main', sha: 'base', repo: { full_name: repository } },
    head: { ref: 'dependabot/npm/pkg-2', sha: 'head', repo: { full_name: repository } } };
  const files = [{ filename: 'package-lock.json', status: 'modified' }];
  const commits = [{ sha: 'head', author: bot, commit: { verification: { verified: true } } }];
  const runs = [workflow, policy].map((path, i) => ({ id: i + 1, workflow_id: i + 1, run_attempt: 1,
    path, event: 'pull_request', head_sha: 'head', status: 'completed', conclusion: 'success',
    pull_requests: [{ number: 1, head: { sha: 'head' }, base: { sha: 'base' } }],
    jobs: [{ name: i === 0 ? 'tests' : 'policy', status: 'completed', conclusion: 'success' }] }));
  const checks = [{ name: 'tests', app: { slug: 'github-actions' }, status: 'completed', conclusion: 'success' }];
  return structuredClone({ pr, files, commits, runs, checks, statuses: [], config });
}
const eligibility = f => evaluatePullRequest(f.pr, f.files, f.commits, repository, f.config);
const readiness = f => evaluateChecks(f.pr, f.runs, f.checks, f.statuses, [workflow, policy], f.config);

test('runtime, dev, major and Docker dependencies use the same test gate', () => {
  for (const filename of Object.keys(config.dependencyFiles)) {
    const f = fixture(); f.files[0].filename = filename;
    assert.equal(eligibility(f).eligible, true);
    assert.equal(readiness(f).eligible, true);
  }
});

for (const [name, mutate] of [
  ['human PR', f => { f.pr.user = { login: 'human', id: 10 }; }],
  ['forged bot login', f => { f.pr.user.id = 10; }],
  ['draft PR', f => { f.pr.draft = true; }],
  ['closed PR', f => { f.pr.state = 'closed'; }],
  ['wrong target', f => { f.pr.base.ref = 'release'; }],
  ['fork', f => { f.pr.head.repo.full_name = 'someone/example'; }],
  ['human branch', f => { f.pr.head.ref = 'feature'; }],
  ['truncated files', f => { f.pr.changed_files = 2; }],
  ['empty files', f => { f.files = []; f.pr.changed_files = 0; }],
  ['course manifest', f => { f.files[0].filename = 'materials/package.json'; }],
  ['assignment lockfile', f => { f.files[0].filename = 'assignment-repository/package-lock.json'; }],
  ['course rename', f => { f.files[0].previous_filename = 'materials/package.json'; }],
  ['source edit', f => { f.files[0].filename = 'src/app.js'; }],
  ['mixed source and dependency update', f => { f.files.push({ filename: 'src/app.js', status: 'modified' }); f.pr.changed_files++; }],
  ['new manifest', f => { f.files[0].status = 'added'; }],
  ['deleted manifest', f => { f.files[0].status = 'removed'; }],
  ['renamed manifest', f => { f.files[0].status = 'renamed'; }],
  ['unverified commit', f => { f.commits[0].commit.verification.verified = false; }],
  ['human commit in bot PR', f => { f.commits[0].author = { login: 'human', id: 10 }; }],
  ['missing commit', f => { f.pr.commits = 2; }],
  ['old head', f => { f.commits[0].sha = 'old'; }],
  ['no coverage', f => { f.config.dependencyFiles['package-lock.json'] = []; }],
  ['unknown coverage', f => { f.config.workflows = {}; }],
  ['repository without application CI', f => { f.config.disabledReason = 'No application CI yet'; }],
]) test(`rejects ${name}`, () => { const f = fixture(); mutate(f); assert.equal(eligibility(f).eligible, false); });

test('action versions may change, commands and security settings may not', () => {
  const file = { additions: 1, deletions: 1, patch: '-      - uses: actions/checkout@v4\n+      - uses: actions/checkout@v5' };
  assert.equal(actionVersionsOnly(file), true);
  assert.equal(actionVersionsOnly({ ...file, patch: file.patch.replace('checkout@v5', 'other@v5') }), false);
  assert.equal(actionVersionsOnly({ ...file, additions: 2 }), false);
  assert.equal(actionVersionsOnly({ ...file, patch: file.patch + '\n+      - run: curl example | sh' }), false);
  assert.equal(actionVersionsOnly({ ...file, patch: undefined }), false);
  assert.equal(actionVersionsOnly({ ...file, patch: file.patch.replace('checkout@v4', 'checkout@' + 'a'.repeat(40)) }), false);
  assert.equal(actionVersionsOnly({ ...file, patch: file.patch.replace('checkout@v4', 'checkout@' + 'a'.repeat(40)).replace('checkout@v5', 'checkout@' + 'b'.repeat(40)) }), true);
});

for (const [name, mutate] of [
  ['missing tests', f => { f.runs.shift(); }],
  ['missing policy tests', f => { f.runs.pop(); }],
  ['failed tests', f => { f.runs[0].conclusion = 'failure'; }],
  ['pending tests', f => { f.runs[0].status = 'in_progress'; }],
  ['old head tests', f => { f.runs[0].head_sha = 'old'; }],
  ['old base tests', f => { f.runs[0].pull_requests[0].base.sha = 'old'; }],
  ['unbound PR tests', f => { f.runs[0].pull_requests = []; }],
  ['push-only tests', f => { f.runs[0].event = 'push'; }],
  ['skipped test job in a green workflow', f => { f.runs[0].jobs[0].conclusion = 'skipped'; }],
  ['missing test job', f => { f.runs[0].jobs = []; }],
  ['failure in a matrix job', f => { f.runs[0].jobs.push({ name: 'tests', status: 'completed', conclusion: 'failure' }); }],
  ['missing checks', f => { f.checks = []; }],
  ['failed external check', f => { f.checks.push({ status: 'completed', conclusion: 'failure' }); }],
  ['pending external check', f => { f.checks.push({ status: 'queued' }); }],
  ['neutral external check', f => { f.checks.push({ status: 'completed', conclusion: 'neutral' }); }],
  ['new failed rerun', f => { f.runs.push({ ...f.runs[0], run_attempt: 2, conclusion: 'failure' }); }],
  ['new cancelled run', f => { f.runs.push({ ...f.runs[0], id: 10, conclusion: 'cancelled' }); }],
  ['failed status', f => { f.statuses = [{ id: 1, context: 'external', state: 'failure' }]; }],
]) test(`blocks merge with ${name}`, () => { const f = fixture(); mutate(f); assert.equal(readiness(f).eligible, false); });

test('latest successful rerun and statuses supersede failures', () => {
  const f = fixture();
  f.runs.push({ ...f.runs[0], run_attempt: 0, conclusion: 'failure' });
  f.statuses = [{ id: 2, context: 'external', state: 'success' }, { id: 1, context: 'external', state: 'failure' }];
  f.checks.push({ app: { slug: 'github-actions' }, status: 'completed', conclusion: 'skipped' });
  assert.equal(readiness(f).eligible, true);
});

function clientFor(f, { moved = false, reviews = [] } = {}) {
  const writes = []; let reads = 0;
  return { writes, client: {
    request: async (path, method = 'GET', body) => {
      if (method !== 'GET') { writes.push({ path, method, body }); return { merged: true }; }
      if (path === 'pulls/1') { reads++; return structuredClone(moved && reads > 1 ? { ...f.pr, head: { ...f.pr.head, sha: 'changed' } } : f.pr); }
      throw new Error(`Unexpected GET ${path}`);
    },
    list: async path => {
      if (path === 'pulls/1/files') return f.files;
      if (path === 'pulls/1/commits') return f.commits;
      if (path === 'pulls/1/reviews') return reviews;
      if (path.startsWith('actions/runs?')) return f.runs;
      if (path.includes('/check-runs?')) return f.checks;
      if (path.endsWith('/statuses')) return f.statuses;
      const match = path.match(/^actions\/runs\/(\d+)\/attempts\/\d+\/jobs$/);
      if (match) return f.runs.find(run => run.id === Number(match[1])).jobs;
      throw new Error(`Unexpected list ${path}`);
    },
  } };
}
for (const mode of ['check', 'merge', 'moved', 'changes-requested', 'pending']) {
  test(`end-to-end API policy: ${mode}`, async () => {
    const f = fixture();
    if (mode === 'pending') f.runs[0].status = 'in_progress';
    const { client, writes } = clientFor(f, { moved: mode === 'moved', reviews: mode === 'changes-requested' ?
      [{ user: { login: 'reviewer' }, state: 'CHANGES_REQUESTED' }] : [] });
    await processPullRequest({ client, repository, config: f.config, number: 1, expectedHead: 'head', merge: mode !== 'check' });
    if (mode === 'merge') {
      assert.deepEqual(writes.map(write => write.method), ['POST', 'PUT']);
      assert.equal(writes[0].body.commit_id, 'head');
      assert.equal(writes[1].body.sha, 'head');
      assert.equal(writes[1].body.merge_method, 'squash');
      assert.ok(writes.every(write => !write.path.includes('dispatch')));
    } else assert.deepEqual(writes, []);
  });
}

test('repository policy references existing dependency files and real CI workflows', () => {
  const url = new URL('../dependabot-automerge.json', import.meta.url);
  if (!existsSync(url)) return; // Template tests have no repository configuration.
  const actual = JSON.parse(readFileSync(url, 'utf8'));
  const root = new URL('../../', import.meta.url);
  assert.ok(actual.baseBranch);
  assert.ok(Array.isArray(actual.coursePaths));
  for (const [path, requirements] of Object.entries({ ...actual.dependencyFiles, ...actual.actionWorkflows })) {
    assert.ok(existsSync(new URL(path, root)), path);
    assert.ok(requirements.length, path);
    for (const required of requirements) {
      assert.ok(existsSync(new URL(required, root)), required);
      assert.ok(actual.workflows[required]?.length, required);
    }
  }
});

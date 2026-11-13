import { appendFileSync, readFileSync } from 'node:fs';
import { pathToFileURL } from 'node:url';

const bot = (user) => user?.login === 'dependabot[bot]' && user.id === 49699333;
const reject = (reason) => ({ eligible: false, reason });
const policyWorkflow = '.github/workflows/dependabot-policy.yml';

export function actionVersionsOnly(file) {
  if (!file.patch) return false;
  const before = [], after = [];
  for (const line of file.patch.split('\n')) {
    if (!line.startsWith('+') && !line.startsWith('-')) continue;
    const match = line.slice(1).match(/^(\s*(?:-\s+)?uses:\s+[\w.-]+\/[\w./-]+)@([a-f0-9]{40}|v?\d[\w.+-]*)(?:\s+#.*)?$/i);
    if (!match) return false;
    (line[0] === '+' ? after : before).push(match.slice(1));
  }
  return after.length > 0 && after.length === file.additions &&
    before.length === file.deletions && before.length === after.length &&
    before.every(([action, ref], i) => action === after[i][0] &&
      (!/^[a-f0-9]{40}$/i.test(ref) || /^[a-f0-9]{40}$/i.test(after[i][1])));
}

export function evaluatePullRequest(pr, files, commits, repository, config) {
  if (pr.state !== 'open' || pr.draft || !bot(pr.user))
    return reject('Only open, non-draft Dependabot PRs qualify.');
  if (pr.base?.ref !== config.baseBranch || pr.base.repo?.full_name !== repository ||
      pr.head?.repo?.full_name !== repository || !pr.head.ref.startsWith('dependabot/'))
    return reject('The PR must originate here and target the default branch.');
  if (config.disabledReason) return reject(config.disabledReason);
  if (!files.length || files.length !== pr.changed_files || files.length > 100)
    return reject('The complete, bounded changed-file list is required.');
  const required = new Set();
  for (const file of files) {
    const course = [file.filename, file.previous_filename].filter(Boolean).some(path =>
      config.coursePaths.some(prefix => path === prefix || path.startsWith(prefix + '/')));
    if (course) return reject(`Course material or exercise content changed: ${file.filename}`);
    if (file.status !== 'modified') return reject('Added, deleted and renamed files need review.');
    let coverage;
    if (/^\.github\/workflows\/[^/]+\.ya?ml$/.test(file.filename)) {
      if (!Object.hasOwn(config.actionWorkflows, file.filename) || !actionVersionsOnly(file))
        return reject('Only version updates to existing actions in covered workflows qualify.');
      coverage = config.actionWorkflows[file.filename];
    } else {
      coverage = Object.hasOwn(config.dependencyFiles, file.filename) && config.dependencyFiles[file.filename];
    }
    if (!Array.isArray(coverage) || !coverage.length)
      return reject(`No reviewed dependency/build coverage for ${file.filename}.`);
    for (const workflow of coverage) required.add(workflow);
  }
  if (!commits.length || commits.length !== pr.commits || commits.at(-1).sha !== pr.head.sha ||
      commits.some(commit => !bot(commit.author) || !commit.commit?.verification?.verified))
    return reject('Every commit must be a verified Dependabot commit on the current head.');
  if ([...required].some(path => !config.workflows[path]?.length) || !required.size)
    return reject('Application build/test coverage must be configured.');
  required.add(policyWorkflow);
  return { eligible: true, required: [...required], reason: 'Verified non-course dependency version update.' };
}

export function evaluateChecks(pr, runs, checks, statuses, required, config) {
  const latest = new Map();
  for (const run of runs) {
    if (run.event !== 'pull_request' || run.head_sha !== pr.head.sha) continue;
    const previous = latest.get(run.workflow_id);
    if (!previous || run.id > previous.id || (run.id === previous.id && run.run_attempt > previous.run_attempt))
      latest.set(run.workflow_id, run);
  }
  if ([...latest.values()].some(run => run.status !== 'completed' || run.conclusion !== 'success'))
    return reject('A current PR workflow is pending or unsuccessful.');
  const verifiedRuns = [];
  for (const path of required) {
    const run = [...latest.values()].find(item => item.path === path);
    if (!run || run.status !== 'completed' || run.conclusion !== 'success' ||
        !run.pull_requests?.some(item => item.number === pr.number &&
          item.head.sha === pr.head.sha && item.base.sha === pr.base.sha))
      return reject(`Build/tests must pass on the current head and base: ${path}`);
    const patterns = path === policyWorkflow ? ['^policy$'] : config.workflows[path];
    for (const pattern of patterns) {
      const jobs = (run.jobs || []).filter(job => new RegExp(pattern).test(job.name));
      if (!jobs.length || jobs.some(job => job.status !== 'completed' || job.conclusion !== 'success'))
        return reject(`Required build/test jobs did not run successfully: ${path} / ${pattern}`);
    }
    verifiedRuns.push(run.id);
  }
  if (!checks.length || checks.some(check => check.status !== 'completed' ||
      !(check.conclusion === 'success' || (check.conclusion === 'skipped' && check.app?.slug === 'github-actions'))))
    return reject('All current checks must finish successfully.');
  const latestStatuses = new Map();
  for (const status of statuses) {
    const previous = latestStatuses.get(status.context);
    if (!previous || status.id > previous.id) latestStatuses.set(status.context, status);
  }
  if ([...latestStatuses.values()].some(status => status.state !== 'success'))
    return reject('A commit status is pending or unsuccessful.');
  return { eligible: true, reason: 'All current build and test checks passed.', verifiedRuns };
}

export function githubClient(repository, token) {
  if (!/^[\w.-]+\/[\w.-]+$/.test(repository) || !token) throw new Error('Repository and token required.');
  const request = async (path, method = 'GET', body) => {
    const response = await fetch(`https://api.github.com/repos/${repository}/${path}`, {
      method,
      headers: { Authorization: `Bearer ${token}`, Accept: 'application/vnd.github+json',
        'X-GitHub-Api-Version': '2022-11-28', 'Content-Type': 'application/json' },
      ...(body ? { body: JSON.stringify(body) } : {}),
    });
    if (!response.ok) throw new Error(`GitHub ${method} ${path}: HTTP ${response.status}`);
    return response.status === 204 ? undefined : response.json();
  };
  const list = async (path, key) => {
    const result = [];
    for (let page = 1; page <= 50; page++) {
      const data = await request(`${path}${path.includes('?') ? '&' : '?'}per_page=100&page=${page}`);
      const items = key ? data[key] : data;
      if (!Array.isArray(items)) throw new Error('Expected a complete GitHub list.');
      result.push(...items);
      if (items.length < 100) return result;
    }
    throw new Error('GitHub list exceeded the bounded review limit.');
  };
  return { request, list };
}

export async function processPullRequest({ client, repository, config, number, expectedHead, merge = false }) {
  const { request, list } = client;
  const pr = await request(`pulls/${number}`);
  if (expectedHead && expectedHead !== pr.head.sha) return reject('Completion belongs to an old head.');
  const [files, commits] = await Promise.all([list(`pulls/${number}/files`), list(`pulls/${number}/commits`)]);
  const eligibility = evaluatePullRequest(pr, files, commits, repository, config);
  if (!eligibility.eligible) return eligibility;
  const [runs, checks, statuses] = await Promise.all([
    list(`actions/runs?event=pull_request&head_sha=${pr.head.sha}`, 'workflow_runs'),
    list(`commits/${pr.head.sha}/check-runs?filter=latest`, 'check_runs'),
    list(`commits/${pr.head.sha}/statuses`),
  ]);
  // Job conclusions distinguish a real build/test pass from a skipped workflow.
  for (const run of runs) {
    if (eligibility.required.includes(run.path) && run.head_sha === pr.head.sha)
      run.jobs = await list(`actions/runs/${run.id}/attempts/${run.run_attempt}/jobs`, 'jobs');
  }
  const readiness = evaluateChecks(pr, runs, checks, statuses, eligibility.required, config);
  if (!readiness.eligible || !merge) return readiness;
  const fresh = await request(`pulls/${number}`);
  if (fresh.state !== 'open' || fresh.draft || fresh.head.sha !== pr.head.sha ||
      fresh.base.sha !== pr.base.sha || fresh.base.ref !== pr.base.ref || fresh.mergeable !== true)
    return reject('The PR moved, closed, or is not mergeable; fresh checks are required.');
  const reviews = await list(`pulls/${number}/reviews`);
  const latestReviews = new Map();
  for (const review of reviews) {
    if (review.state !== 'COMMENTED') latestReviews.set(review.user.login, review);
  }
  if ([...latestReviews.values()].some(review => review.state === 'CHANGES_REQUESTED'))
    return reject('A reviewer has requested changes.');
  if (!reviews.some(review => review.user.login === 'github-actions[bot]' &&
      review.commit_id === pr.head.sha && review.state === 'APPROVED'))
    await request(`pulls/${number}/reviews`, 'POST', { event: 'APPROVE', commit_id: pr.head.sha });
  // Use only GITHUB_TOKEN: this merge does not trigger push-driven publishing,
  // Pages, or deployment workflows. Never substitute a PAT or App token.
  const result = await request(`pulls/${number}/merge`, 'PUT', {
    sha: pr.head.sha, merge_method: 'squash',
    commit_title: `chore(deps): merge tested non-course dependency update (#${number})`,
  });
  if (!result.merged) throw new Error('GitHub did not merge the PR.');
  return { eligible: true, merged: true, reason: `Merged #${number} after build/tests; no release dispatched.` };
}

async function main() {
  const event = JSON.parse(readFileSync(process.env.GITHUB_EVENT_PATH, 'utf8'));
  const config = JSON.parse(readFileSync(new URL('../dependabot-automerge.json', import.meta.url), 'utf8'));
  const repository = process.env.GITHUB_REPOSITORY;
  const client = githubClient(repository, process.env.GH_TOKEN);
  const merge = process.argv[2] === 'merge';
  if (!merge && process.argv[2] !== 'check') throw new Error('Expected check or merge mode.');
  let candidates;
  const run = event.workflow_run;
  if (run) {
    if (run.event !== 'pull_request' || run.head_repository?.full_name !== repository ||
        !run.head_branch.startsWith('dependabot/')) return;
    candidates = run.pull_requests || [];
    if (!candidates.length) candidates = await client.list(
      `pulls?state=open&base=${encodeURIComponent(config.baseBranch)}&head=${encodeURIComponent(repository.split('/')[0] + ':' + run.head_branch)}`);
  } else if (process.env.GITHUB_EVENT_NAME === 'workflow_dispatch') {
    candidates = (await client.list(`pulls?state=open&base=${encodeURIComponent(config.baseBranch)}`))
      .filter(pr => bot(pr.user));
  } else throw new Error('Only workflow_run or manual default-branch dispatch is supported.');
  for (const candidate of candidates) {
    const result = await processPullRequest({ client, repository, config, number: candidate.number,
      expectedHead: run?.head_sha, merge });
    console.log(`#${candidate.number}: ${result.reason}`);
    if (process.env.GITHUB_STEP_SUMMARY)
      appendFileSync(process.env.GITHUB_STEP_SUMMARY, `- #${candidate.number}: ${result.reason}\n`);
  }
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href)
  main().catch(error => { console.error(error.message); process.exitCode = 1; });

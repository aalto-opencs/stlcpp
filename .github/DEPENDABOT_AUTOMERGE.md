# Dependabot auto-merge

Verified Dependabot version updates may squash-merge after the configured build
and test jobs pass on the exact current PR head and base. Runtime, development,
and major dependency updates use the same checks. Course material, exercises,
source changes, new/renamed/deleted files, human commits, and unknown dependency
inputs require review.

`.github/dependabot-automerge.json` maps existing dependency files to the CI
workflows and jobs that exercise them. Existing workflow changes must consist
only of paired version updates to the same actions; SHA pins cannot become tags.
Add coverage before adding a dependency input. A repository without application
CI stays in manual review; policy self-tests alone cannot authorize a merge.

The trusted default-branch workflow checks every current PR workflow, check and
commit status, rejects missing/skipped required jobs and outstanding change
requests, approves the tested revision, and merges with an exact head SHA.
Pending, failed, cancelled, stale or incomplete results never authorize a merge.
Checks run only in ordinary read-only PR jobs. The privileged workflow does not
execute PR code or consume its dependencies, caches or artifacts.

Merging uses only the repository GITHUB_TOKEN, so it does not start push-driven
publishing, Pages, or deployment workflows. No release is dispatched here.
The platform's separate workflow handles dependency-labelled staging images and
its 21:00–08:00 Europe/Helsinki dependency-only deployment window.

The workflow runs after CI completion. Its manual default-branch dispatch can
recheck existing Dependabot PRs; it applies the same policy and checks. Existing
Dependabot schedules, branch rules, releases and archived repository states are
not altered.

Validate policy changes with `node --test .github/scripts/dependabot.test.mjs`.

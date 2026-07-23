---
name: refresh-t3code-pr-stack
description: "Refresh Ivan's maintained T3 Code pull requests and personal Nix patch stack: discover and automatically incorporate newly created Ivan-authored PRs, inspect and address review feedback, rebase writable branches onto the latest live upstream main, minimize carried patches, push safely, update the t3code-upstream lock, recompute hashes, regenerate compatibility patches only when necessary, build, activate, and commit the result. Use when asked to update, rebase, repair, synchronize, or maintain all T3 Code PRs or the patched T3 Code installation."
---

# Refresh the T3 Code PR Stack

## Read the contribution rules first

Before inventory, API queries, branch work, or any other task action, read the
T3 Code repository-root `AGENTS.md` and `CONTRIBUTING.md` completely. Apply the
contribution guide to every maintained PR: keep proposals focused, avoid
unrelated fixes, and require current before/after images for UI changes or a
short video for motion, timing, transition, and interaction changes.

## Parallelize independent work

Parallel agent work is the default for this workflow. At the beginning of the
refresh, split the work into independent tracks and keep the available agent
slots occupied whenever useful work can proceed concurrently. Do not make one
agent serially inspect, repair, and capture every PR when those tasks can be
separated safely.

Use distinct subagents for as many of these tracks as the inventory supports:

- owned-PR and carried-patch discovery;
- complete review-thread, discussion-comment, and CI/check inventory;
- repair and validation of different writable PR branches in disjoint T3 Code
  worktrees;
- compatibility-patch reconstruction or independent semantic verification;
- UI evidence planning and capture for different PRs or interaction groups.

Every subagent must read the repository-root `AGENTS.md` and `CONTRIBUTING.md`
before acting. Assign every subagent an explicit project-local T3 Code worktree
under `<repo>/.worktrees/<task>` and include that exact path in its delegation
prompt. This applies to read-only inventory, review, validation, and media work
as well as branch-writing tasks: subagents must not work from the primary T3
Code checkout. The primary agent must create or verify each assigned worktree,
confirm it is clean, and give the agent exclusive ownership of that worktree,
branch, files, and artifacts. A read-only subagent may inspect the primary
`/srv/dotfiles` checkout when its assignment requires manifest or history
evidence, but it must not edit it; never create a dotfiles worktree. Remind every
agent that other agents are working concurrently and it must not revert their
changes. Have inventory agents return structured evidence, and have media agents
save clearly named screenshots or videos in disjoint temporary artifact
directories. Use an agent-owned isolated desktop/browser for application
capture unless the task specifically requires the user's logged-in browser
state.

The primary agent remains responsible for reviewing and integrating all
results. Serialize operations that share state: force-pushes to the same
branch, edits to the same manifest or compatibility file, final Nix builds and
activation, dotfiles commits, and pushes. Re-query remote heads and comments
after parallel work completes because another agent or review bot may have
advanced them during the refresh.

Synchronize two related inventories without conflating them:

- **Maintained PR branches:** writable `colonelpanic8` branches that can be rebased and repaired.
- **Carried patches:** everything represented in `/srv/dotfiles/nixos/t3code.nix`, including external PRs and closed-but-unmerged PRs that must remain in the personal build.
- **New owned PRs:** PRs created by `colonelpanic8` since the last completed patch-stack refresh that must be admitted automatically when they are not already carried or absorbed upstream.

A PR being closed does not mean its feature should be removed. A PR being merged does not mean it can be removed until the pinned upstream source actually contains it.

## Fixed locations and safety rules

- Resolve T3 Code through `/srv/dotfiles/dotfiles/agents/project-links/t3code`.
- Use `/srv/dotfiles/nixos/t3code.nix` as the carried-patch manifest.
- Update only the `t3code-upstream` input in `/srv/dotfiles/nixos/flake.lock` unless another change is explicitly required.
- Work only in the primary `/srv/dotfiles` checkout. Never create or use a dotfiles worktree.
- Never create a nested T3 Code worktree under `/srv/dotfiles`.
- Preserve dirty T3 Code worktrees and unrelated dotfiles index entries. Do not stash, reset, clean, or rewrite them.
- Detect concurrent edits by rechecking status and relevant file OIDs before each mutation phase.

## Keep main current and minimize carried patches

Treat the live `origin/main` head as an invariant, not a one-time starting
point. Fetch it before inventory and record its OID. Before each branch rewrite,
stack generation, build, activation, and final commit, compare against the live
remote main head and fetch again if it moved. Every writable PR must be rebased
onto that exact current main OID. If main advances during the refresh, invalidate
affected branch bases, integration results, hashes, tests, and media; rebase and
regenerate them before calling the refresh complete.

To the extent possible, avoid carrying additional patches and eliminate
existing ones when the exact pinned upstream source makes them unnecessary.
Treat every PR diff, exclusion, audit binding, and local compatibility patch as
temporary debt:

- Advance upstream first, then rebase writable PRs so upstream-absorbed hunks
  disappear from their cumulative diffs.
- Remove a carried patch when the exact pin contains its complete intended
  behavior. When it contains only part, recompute the remaining unique delta
  and use the smallest exclusion and compatibility machinery that preserves all
  unabsorbed desired behavior.
- Prefer an unmodified raw PR diff. Add an exclusion or local compatibility
  patch only when the ordered raw diffs cannot compose semantically, and use
  the smallest sound file/hunk coverage.
- Delete obsolete exclusions, audit bindings, compatibility files, and manifest
  plumbing as soon as the stack no longer needs them. Do not retain machinery
  merely for historical continuity.

Do not simplify the stack by silently dropping desired behavior. Keep a closed
unmerged or external patch until the exact pin contains its intent or the user
explicitly says to drop it.

## 1. Build an inventory before changing anything

Read the applicable `AGENTS.md` files, verify `gh auth status`, fetch the live
`origin/main` and `fork`, record the fetched main OID, and inspect all T3 Code
worktrees.

Build a table with one row per relevant PR containing:

- PR number, title, author, state, merge state, and review decision.
- Creation/update timestamps and whether the PR is new since the last completed stack refresh.
- Base OID, head OID, fork branch, and whether the branch is writable.
- Whether it is present in `t3code.nix` and how: raw `fetchurl`, excluded `fetchpatch`, or local compatibility patch.
- Unresolved current review threads and failing/pending checks.
- Whether current upstream contains the merged result.

Derive the sets independently:

1. Parse every `pull/NUMBER.diff` URL from `t3code.nix`, including audit-only bindings forced through `builtins.seq`.
2. Determine the last completed stack-refresh boundary from the newest committed change to `nixos/t3code.nix`. Inspect current uncommitted manifest changes too, but do not treat them as a completed refresh.
3. Query all PRs authored by `colonelpanic8`, including open, draft, merged, and closed PRs, with `createdAt`, `updatedAt`, head/base OIDs, and branch ownership.
4. Query each carried PR directly, because third-party PRs are absent from the authored list.

Use GitHub GraphQL `reviewThreads` for inline feedback; `gh pr view` summaries alone omit important unresolved comments.

Classify each PR:

- **New owned PR:** authored by `colonelpanic8`, targets `main`, is absent from the committed/current manifest, and was created after the last completed refresh boundary. Automatically incorporate it after branch validation. Include drafts and quickly closed-unmerged PRs; upstream state alone does not make a new personal feature unwanted.
- **Owned and maintained:** rebase and fix.
- **External carried patch:** refresh metadata/hash and composition, but do not attempt to push its branch.
- **Merged and present in the new upstream pin:** remove its patch after proving ancestry/content.
- **Closed unmerged but desired:** retain it. Do not reopen it automatically.
- **Historical absent PR:** created before the refresh boundary and not carried. Treat it as intentionally dropped or superseded unless the user explicitly restores it; do not resurrect old PRs merely because they are absent.
- **Obsolete or intentionally dropped carried PR:** remove only with explicit evidence from the user or existing manifest history.

Do not ask for confirmation merely because a qualifying new owned PR was discovered. Incorporating it is part of this workflow. Stop for direction only when the PR itself says it is not intended for the personal build or two implementations are mutually exclusive and history does not establish which one supersedes the other.

Present the inventory in the working commentary before rewriting branches.

## 2. Rebase and repair each writable PR

Process owned PRs independently; each must remain a clean proposal against `origin/main`, not a stacked PR.

For each branch:

1. Record the remote head OID for an exact force-with-lease guard.
2. Locate its attached worktree. Use it only if clean. If no worktree owns the branch, create a project-local isolated worktree. If its existing worktree is dirty, do not touch it; report that branch as blocked while continuing safe work elsewhere.
3. Fetch and verify the live remote main head, then rebase onto that exact OID.
4. Resolve conflicts according to the standalone PR's intent, without importing unrelated personal patches.
5. Re-read all unresolved review threads against the rebased code.
6. Implement every actionable correctness, reliability, test, and maintainability fix. Evaluate bot comments critically; do not blindly apply contradictory or invalid advice.
7. Add regression tests. For obsolete or invalid feedback, prepare a concise evidence-based reply instead of changing correct code.
8. Always run the relevant formatting, lint, and type checks for the affected
   scope. Run focused tests for behavioral changes, bug fixes, risky rebases,
   and new edge cases. For small mechanical-only rebases, local tests may be
   deferred to CI when they are slow and would add little signal; run them when
   reasonably fast and record any intentional deferral. Do not run repo-wide
   `vp check`, `vp run typecheck`, or test suites unless the repository
   instructions or user explicitly require them. Run `vp run lint:mobile` for
   native mobile changes.
9. Update before/after images or video when rebasing or feedback changes UI behavior.
10. Commit coherent review fixes and push with an explicit `--force-with-lease=refs/heads/BRANCH:RECORDED_OID`.
11. Re-query the PR head and checks. Reply to or resolve review threads only after the fix is published and verified.

Do not force-push when the remote head changed unexpectedly. Fetch, inspect the new commits, and reconcile them first.

Treat review feedback as a proposal to evaluate, not an instruction to obey. Do
not be afraid to push back when a suggestion is unreasonable, conflicts with
the PR's focused intent, weakens correctness or maintainability, or rests on an
incorrect premise. Respond politely and humbly: acknowledge the reviewer's
concern, explain the relevant evidence and tradeoff without sounding dismissive,
and invite correction if important context was missed. Prefer a small concrete
example, test result, or code reference over argumentative language.

### Close the CI loop for every writable PR

CI is implementation work, not merely inventory. After every push, query all
required and advisory checks and wait for the current head's checks to reach a
terminal state. For each failure, inspect the failed job annotations and logs
with `gh pr checks`, `gh run view`, and `gh run view --log-failed` as
appropriate. Reproduce branch-caused failures locally, fix the underlying code,
tests, generated files, formatting, or configuration, rerun the repository
checks, push, and repeat until the PR has no actionable failing checks.

Do not treat a retry as a fix without evidence that the failure is flaky or
external. If a check is blocked by upstream infrastructure, permissions, or an
unrelated base-branch failure, preserve the evidence, report the exact check and
run URL, and continue independent work. Do not call a maintained PR clean while
its current head has an unexplained failure or a still-running required check.

## 3. Incorporate every newly discovered owned PR

After its branch is rebased, repaired, pushed, and validated, add each qualifying new owned PR to the carried patch stack in the same refresh. Do not leave it only in the inventory.

For each new PR:

1. Fetch its current cumulative `.diff` and raw hash.
2. Choose its position by feature dependency and file overlap, not simply PR number.
3. Attempt the raw `fetchurl` diff against the exact assembled stack.
4. If it overlaps existing patches, use the smallest sound `fetchpatch` exclusion set and/or regenerate the relevant compatibility patch. A conflict is work to reconcile, not a reason to skip the new PR.
5. Add the feature, PR number, 12-character head OID, URL, and correct hash form to `t3code.nix`.
6. When a compatibility patch represents its raw diff, retain an audit-only raw fetch connected through `builtins.seq`.
7. If the PR merged before refresh, incorporate it by advancing the pinned source when possible; otherwise carry its diff until the selected pin contains it.

Before proceeding, compare the manifest-derived PR set with the discovery inventory and require that every qualifying new owned PR is now either represented in `t3code.nix` or proven present in the pinned upstream source.

## 4. Advance the pinned upstream source

After writable PR heads are stable, run from `/srv/dotfiles/nixos`:

```bash
nix flake update t3code-upstream
```

Compare the new locked revision with each merged carried PR. Remove a merged patch only when its changes are present in that exact locked source. Retain closed-unmerged and external desired patches.

Also compare every carried patch with the exact new locked source for full or
partial absorption, regardless of the PR's GitHub state. Rebase writable PRs
again if needed and remove fully redundant patches. Recompute the smallest
compatibility and exclusion coverage that preserves only the unabsorbed
behavior. Prefer no local compatibility patch whenever the current raw diffs
now compose cleanly.

Update the patched source name/version date when appropriate. Do not update unrelated flake inputs.

## 5. Refresh raw and normalized patch hashes

For every carried PR, including newly admitted PRs, fetch its current head and raw cumulative diff hash:

```bash
nix store prefetch-file --json \
  https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/PR_NUMBER.diff
```

Update the 12-character head annotation and raw `fetchurl` hash, including audit-only bindings.

For every `fetchpatch` with `excludes`, recompute the normalized hash through the same Nix expression and exclusion list. Never substitute the raw `fetchurl` hash; the normalization changes it.

Keep audit-only raw fetches connected to evaluation with `builtins.seq`, so a changed PR cannot silently bypass its recorded hash.

## 6. Rebuild compatibility patches from source

Do not assume an old compatibility patch remains valid after a rebase or upstream bump.

For each overlap group:

1. Start from the exact new `t3code-upstream` source in a clean temporary integration tree.
2. Apply the ordered non-overlapping PR portions exactly as Nix will.
3. Apply the current raw diffs for the overlapping PRs.
4. Resolve conflicting files semantically so the final tree preserves every represented feature and current upstream behavior.
5. Run focused tests for the combined behavior.
6. Generate a full-index Git diff containing only the compatibility-owned files.
7. Replace the corresponding file under `nixos/patches/` and document its PR/file coverage in `t3code.nix`.
8. Prove that no omitted raw hunk is lost between the exclusions and compatibility patch.

Stage new compatibility files before Nix evaluation so the flake includes them. Keep the stack ordered by dependency and overlap, not by PR number.

## 7. Refresh final dependency hashes

Build the final patched source and package. If `pnpmDeps` changes, let Nix report the expected hash, update that field, and rebuild. Distinguish:

- Raw GitHub diff hashes.
- Normalized `fetchpatch` hashes.
- Final patched-source `pnpmDeps` hash.

Never bulk-replace hashes without matching each error to its derivation.

## 8. Capture and attach current contribution evidence

After writable heads and compatibility merges are stable, assign media agents
to every PR with visible UI or interaction changes. A combined personal-stack
build may be used only when it contains the exact recorded PR heads and the
capture isolates the target PR's behavior without attributing another patch's
entry point or UI to it.

For each applicable PR:

1. Record the PR head, upstream base, and combined integration/package OID in a
   small artifact manifest.
2. Use disposable seed projects and non-sensitive data in an agent-owned
   isolated desktop/browser.
3. Capture clear before/after stills for static UI changes and a short video for
   motion, keyboard interaction, focus, timing, or navigation behavior.
4. Dismiss unrelated notifications and inspect the final artifact for clarity,
   correct attribution, and accidental personal information.
5. Save files in a disjoint temporary directory named for the PR, then attach
   the selected artifacts to its GitHub description or conversation and update
   the UI/verification text.
6. Re-query the PR and upstream heads immediately before and after capture. If
   either relevant head moved, invalidate the stale artifact, reconcile the new
   code, and recapture before publishing.

Use the logged-in browser only for the attachment step when required; keep
application setup and capture isolated. Do not claim that capture was attempted
when a usable artifact exists locally, and do not call a UI PR contribution-
ready while its required current media is missing without reporting the exact
blocker.

## 9. Validate and activate the complete stack

From `/srv/dotfiles/nixos`:

1. Run `nix-instantiate --parse t3code.nix`.
2. Run `git diff --check` for the intended dotfiles paths.
3. Build the patched source to validate patch order.
4. Build the actual host `pkgs.t3code` derivation.
5. Run `just switch` from the primary checkout only.
6. Verify the installed `t3` store path and, where enabled, the active `t3code-headless.service` `ExecStart` path.
7. Re-query PR heads, unresolved threads, and required CI checks to catch races during the refresh.
8. Re-query the live remote main head, fetch every current writable PR head, and
   prove that the recorded live main OID is both its merge base and an ancestor;
   do not rely only on GitHub's `baseRefOid`. If live main moved or a head does
   not descend from that exact OID, invalidate affected validation and return to
   branch rebasing before committing.

If a branch remains blocked by dirty state, external ownership, a genuine design decision, or failing upstream infrastructure, preserve all successful independent work and report the exact blocker. Do not mark the overall refresh clean while a maintained PR or patch is silently stale.

## 10. Commit and report

Review the entire dotfiles worktree and define one atomic patch-stack refresh commit unless independent changes clearly require more. Use explicit paths so pre-existing staged changes do not leak into the commit. Push the current default branch after validation.

Report:

- Upstream old/new locked OIDs.
- Newly discovered PRs and how each was automatically incorporated or absorbed upstream.
- Every PR old/new head, state, review work, validation, and push result.
- Patches absorbed, retained, added, or removed and why.
- Compatibility patches regenerated.
- Every changed hash category.
- Final package, `just switch`, installed binary, and service verification.
- Commit and push result plus unrelated worktree changes left untouched.

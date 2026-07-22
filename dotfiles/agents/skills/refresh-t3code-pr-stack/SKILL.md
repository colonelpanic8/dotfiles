---
name: refresh-t3code-pr-stack
description: "Refresh Ivan's maintained T3 Code pull requests and personal Nix patch stack: inspect and address review feedback, rebase writable PR branches onto upstream main, push safely, update the t3code-upstream lock, recompute PR and dependency hashes, regenerate compatibility patches, build, activate, and commit the result. Use when asked to update, rebase, repair, synchronize, or maintain all T3 Code PRs or the patched T3 Code installation."
---

# Refresh the T3 Code PR Stack

Synchronize two related inventories without conflating them:

- **Maintained PR branches:** writable `colonelpanic8` branches that can be rebased and repaired.
- **Carried patches:** everything represented in `/srv/dotfiles/nixos/t3code.nix`, including external PRs and closed-but-unmerged PRs that must remain in the personal build.

A PR being closed does not mean its feature should be removed. A PR being merged does not mean it can be removed until the pinned upstream source actually contains it.

## Fixed locations and safety rules

- Resolve T3 Code through `/srv/dotfiles/dotfiles/agents/project-links/t3code`.
- Use `/srv/dotfiles/nixos/t3code.nix` as the carried-patch manifest.
- Update only the `t3code-upstream` input in `/srv/dotfiles/nixos/flake.lock` unless another change is explicitly required.
- Work only in the primary `/srv/dotfiles` checkout. Never create or use a dotfiles worktree.
- Never create a nested T3 Code worktree under `/srv/dotfiles`.
- Preserve dirty T3 Code worktrees and unrelated dotfiles index entries. Do not stash, reset, clean, or rewrite them.
- Detect concurrent edits by rechecking status and relevant file OIDs before each mutation phase.

## 1. Build an inventory before changing anything

Read the applicable `AGENTS.md` files, verify `gh auth status`, fetch `origin main` and `fork`, and inspect all T3 Code worktrees.

Build a table with one row per relevant PR containing:

- PR number, title, author, state, merge state, and review decision.
- Base OID, head OID, fork branch, and whether the branch is writable.
- Whether it is present in `t3code.nix` and how: raw `fetchurl`, excluded `fetchpatch`, or local compatibility patch.
- Unresolved current review threads and failing/pending checks.
- Whether current upstream contains the merged result.

Derive the sets independently:

1. Parse every `pull/NUMBER.diff` URL from `t3code.nix`, including audit-only bindings forced through `builtins.seq`.
2. Query open and recently closed PRs authored by `colonelpanic8`.
3. Query each carried PR directly, because third-party and closed PRs may not appear in the authored-open list.

Use GitHub GraphQL `reviewThreads` for inline feedback; `gh pr view` summaries alone omit important unresolved comments.

Classify each PR:

- **Owned and maintained:** rebase and fix.
- **External carried patch:** refresh metadata/hash and composition, but do not attempt to push its branch.
- **Merged and present in the new upstream pin:** remove its patch after proving ancestry/content.
- **Closed unmerged but desired:** retain it. Do not reopen it automatically.
- **Obsolete or intentionally dropped:** remove only with explicit evidence from the user or existing manifest history.

Present the inventory in the working commentary before rewriting branches.

## 2. Rebase and repair each writable PR

Process owned PRs independently; each must remain a clean proposal against `origin/main`, not a stacked PR.

For each branch:

1. Record the remote head OID for an exact force-with-lease guard.
2. Locate its attached worktree. Use it only if clean. If no worktree owns the branch, create a project-local isolated worktree. If its existing worktree is dirty, do not touch it; report that branch as blocked while continuing safe work elsewhere.
3. Rebase onto current `origin/main`.
4. Resolve conflicts according to the standalone PR's intent, without importing unrelated personal patches.
5. Re-read all unresolved review threads against the rebased code.
6. Implement every actionable correctness, reliability, test, and maintainability fix. Evaluate bot comments critically; do not blindly apply contradictory or invalid advice.
7. Add regression tests. For obsolete or invalid feedback, prepare a concise evidence-based reply instead of changing correct code.
8. Run focused tests, then the repository-required `vp check` and `vp run typecheck`; also run `vp run lint:mobile` for native mobile changes.
9. Update before/after images or video when rebasing or feedback changes UI behavior.
10. Commit coherent review fixes and push with an explicit `--force-with-lease=refs/heads/BRANCH:RECORDED_OID`.
11. Re-query the PR head and checks. Reply to or resolve review threads only after the fix is published and verified.

Do not force-push when the remote head changed unexpectedly. Fetch, inspect the new commits, and reconcile them first.

## 3. Advance the pinned upstream source

After writable PR heads are stable, run from `/srv/dotfiles/nixos`:

```bash
nix flake update t3code-upstream
```

Compare the new locked revision with each merged carried PR. Remove a merged patch only when its changes are present in that exact locked source. Retain closed-unmerged and external desired patches.

Update the patched source name/version date when appropriate. Do not update unrelated flake inputs.

## 4. Refresh raw and normalized patch hashes

For every carried PR, fetch its current head and raw cumulative diff hash:

```bash
nix store prefetch-file --json \
  https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/PR_NUMBER.diff
```

Update the 12-character head annotation and raw `fetchurl` hash, including audit-only bindings.

For every `fetchpatch` with `excludes`, recompute the normalized hash through the same Nix expression and exclusion list. Never substitute the raw `fetchurl` hash; the normalization changes it.

Keep audit-only raw fetches connected to evaluation with `builtins.seq`, so a changed PR cannot silently bypass its recorded hash.

## 5. Rebuild compatibility patches from source

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

## 6. Refresh final dependency hashes

Build the final patched source and package. If `pnpmDeps` changes, let Nix report the expected hash, update that field, and rebuild. Distinguish:

- Raw GitHub diff hashes.
- Normalized `fetchpatch` hashes.
- Final patched-source `pnpmDeps` hash.

Never bulk-replace hashes without matching each error to its derivation.

## 7. Validate and activate the complete stack

From `/srv/dotfiles/nixos`:

1. Run `nix-instantiate --parse t3code.nix`.
2. Run `git diff --check` for the intended dotfiles paths.
3. Build the patched source to validate patch order.
4. Build the actual host `pkgs.t3code` derivation.
5. Run `just switch` from the primary checkout only.
6. Verify the installed `t3` store path and, where enabled, the active `t3code-headless.service` `ExecStart` path.
7. Re-query PR heads, unresolved threads, and required CI checks to catch races during the refresh.

If a branch remains blocked by dirty state, external ownership, a genuine design decision, or failing upstream infrastructure, preserve all successful independent work and report the exact blocker. Do not mark the overall refresh clean while a maintained PR or patch is silently stale.

## 8. Commit and report

Review the entire dotfiles worktree and define one atomic patch-stack refresh commit unless independent changes clearly require more. Use explicit paths so pre-existing staged changes do not leak into the commit. Push the current default branch after validation.

Report:

- Upstream old/new locked OIDs.
- Every PR old/new head, state, review work, validation, and push result.
- Patches absorbed, retained, added, or removed and why.
- Compatibility patches regenerated.
- Every changed hash category.
- Final package, `just switch`, installed binary, and service verification.
- Commit and push result plus unrelated worktree changes left untouched.

---
name: refresh-t3code-pr-stack
description: "Refresh Ivan's maintained T3 Code pull requests and personal Nix integration branch: discover and incorporate newly created Ivan-authored PRs, address review feedback, rebase writable branches onto live upstream main, rebuild the integration branch by 3-way merge from an ordered manifest, minimize carried topics, push safely, repin the flake input, build, activate, and commit. Use when asked to update, rebase, repair, synchronize, or maintain all T3 Code PRs or the patched T3 Code installation."
---

# Refresh the T3 Code integration branch

## How this works

The personal T3 Code build is an **integration branch on the fork**, rebuilt
from scratch on every refresh by merging an ordered manifest of topics onto live
upstream main. It is a build artifact: never commit to it, never base work on
it, never merge it back.

This replaced an `applyPatches` stack of raw PR diffs plus hand-written
`*-stack-compat.patch` conflict resolutions. Do not reintroduce that approach.
`patch(1)` does fuzzy context matching with no ancestry, and it silently landed
a hunk on the wrong symbol at least once (#4484, two byte-identical class
bodies). A 3-way merge cannot make that mistake.

## Fixed locations

| Thing | Path |
|---|---|
| T3 Code checkout | `/srv/dotfiles/dotfiles/agents/project-links/t3code` |
| Main manifest | `/srv/dotfiles/nix-shared/t3code-stack.toml` |
| Main lock | `/srv/dotfiles/nix-shared/t3code-stack.lock.json` |
| Group manifest(s) | `/srv/dotfiles/nix-shared/t3code-<group>.toml` |
| Rebuild | `/srv/dotfiles/nixos/scripts/rebuild-t3code-stack.py` |
| Conflict helpers | `nixos/scripts/resolve-from-baseline.py`, `replay-resolutions.py` |
| Flake input | `t3code-integration` in `/srv/dotfiles/nixos/flake.nix`, pinned by REV |

## Safety rules

- Work only in the primary `/srv/dotfiles` checkout. Never create a dotfiles worktree.
- Never create a nested T3 Code worktree under `/srv/dotfiles`.
- Preserve dirty T3 Code worktrees and unrelated dotfiles index entries.
- **Pin the flake input by rev, never by branch** — `t3code/stack` is force-pushed.
  Every rebuild also pushes a dated tag so older revs stay fetchable.
- Re-check the live `origin/main` head before each mutation phase; if it moved,
  rebase affected branches and rebuild.

## 1. Inventory

Read the T3 Code repo-root `AGENTS.md` and `CONTRIBUTING.md`. Verify
`gh auth status`. Fetch `origin/main` and `fork`, record the main OID.

Build one row per relevant PR: number, title, state, review decision, base/head
OID, fork branch, writable or not, and whether it is new since the last refresh.

Reconcile two inventories without conflating them:

- **Maintained PR branches** — writable `colonelpanic8` branches, rebasable.
- **Carried topics** — every entry in the manifest, including external PRs,
  closed-but-unmerged PRs, and branch-linked entries.
- **New owned PRs** — created by `colonelpanic8` since the last refresh; admit
  them unless already carried or absorbed upstream.

A PR being closed does not mean drop it. A PR being merged does not mean drop it
until the pinned upstream actually contains it.

**Branch-linked entries** (no `pr` key, e.g. `t3code/show-remote-host-name`)
exist precisely because their PR was closed or never opened. The BRANCH is the
authority. Rebase, push, and keep carrying it. Remove only on explicit
instruction, or after proving upstream contains its complete behavior.

**External entries** (`kind = "external"`, e.g. #3984, #4181) belong to other
authors and cannot be rebased. They merge from the PR head as-is.

## 2. Rebase writable branches onto live main

Every writable topic must sit on the exact current main OID. Local-only topics
(`kind = "local"`, e.g. `t3code/local/artifact-safety`, `t3code/local/nix-flake`)
rebase too.

## 3. Rebuild the integration branch

Use `$rebuild-t3code-stack`. It owns the rebuild procedure, the conflict-helper
caveats, and the verification ladder; do not duplicate them here.

In short: groups first, then the main stack; resolve conflicts against BRANCH
content, never by copying a reference tree; and gate on
`audit-stack-content.py` before pushing. A green build does not prove features
survived — the content audit does.

## 4. Minimize carried topics

Treat every topic as temporary debt. The script flags `ABSORBED` (already an
ancestor of main) and `EMPTY` (merge changed nothing) as drop candidates —
verify, then delete the manifest line. Do not drop desired behavior silently.

**Watch for compat-patch-era content.** Some historical `*-stack-compat.patch`
files carried ORIGINAL local work that exists in no branch — merging alone can
never recover it. Everything of that kind now lives either on a topic branch or
as an explicit `[[epilogue]]`. If a build fails on a missing export or symbol,
suspect this first.

**Epilogues** are patches that are functions of the ASSEMBLED tree and therefore
cannot live on any branch — a migration ID that depends on what the stack
already used, a fixture that must list every field, or glue between topics that
has no other home. Keep them minimal; prefer a group branch when the glue
belongs to a specific cluster.

## 5. Verify

Run the full ladder from `$rebuild-t3code-stack`: content audit, tree diff,
conflict count, build. All four, in that order.

## 6. Land it

Push branch + dated tag, repin `t3code-integration` by rev in
`nixos/flake.nix`, run `nix flake lock --update-input t3code-integration`, write
the lock, build, then `just switch` from `/srv/dotfiles/nixos`. Commit the
manifest, lock, and flake changes together so the pin and lock never disagree.

## Parallelizing

Independent tracks — PR/CI inventory, review-comment triage, repair of different
writable branches, UI evidence capture — can run as subagents. Give each an
explicit T3 Code worktree under `<repo>/.worktrees/<task>` and exclusive
ownership of it; never the primary checkout, never a dotfiles worktree.
Serialize force-pushes to the same branch, manifest edits, the final build, and
commits. Re-query remote heads afterwards; bots may have advanced them.

## Report

- Upstream old/new OIDs; group and main old/new tree OIDs and whether they moved.
- New PRs discovered and how each was incorporated or found absorbed.
- Per-PR head, state, review work, and push result.
- Topics added, retained, or dropped, and why.
- Conflict count vs. the previous refresh.
- Build result, `just switch`, and commit/push result.
- Any unrelated worktree changes left untouched.

# T3 Code PR stack structure

Use this guide as the shared model for the T3 Code inventory, branch-maintenance,
and integration-build skills. Read the repo-root `AGENTS.md` and
`CONTRIBUTING.md` before operating in the T3 Code checkout.

## Fixed locations

| Item | Location |
| --- | --- |
| T3 Code checkout | `/srv/dotfiles/dotfiles/agents/project-links/t3code` |
| Stack tooling branch | `fork/t3code/stack-tooling` |
| Authoritative build guide | `stack/BUILDING.md` on the tooling branch |
| Main manifest and lock | `stack/stack.toml`, `stack/stack.lock.json` |
| Group manifest and lock | `stack/thread-picker.toml`, `stack/thread-picker.lock.json` |
| Rebuild tools | `stack/bin/` |
| Integration branch | `fork/t3code/stack` |
| Dotfiles consumer | `/srv/dotfiles/nix-shared/t3code.nix` |
| Flake input | `t3code-integration` in `/srv/dotfiles/nixos/flake.nix` |

The integration branch is an assembled build artifact. Never commit to it, base
work on it, or merge it back. Rebuild tooling force-pushes it and preserves each
published revision with a dated tag. The Nix flake must pin a revision, never
the moving branch.

## Three independent operations

1. **Inventory** is read-only. Reconcile live GitHub PRs and fork branches with
   manifests and locks, then report missing, moved, absorbed, or redundant
   topics. Do not rebase, edit manifests, rebuild, push, repin, or activate.
2. **Branch refresh** maintains writable topic branches. Address review
   feedback when requested, rebase each maintained fork/local branch onto the
   exact live `origin/main`, verify it, and push safely. Do not edit manifests,
   rebuild the integration branch, repin Nix, or activate.
3. **Stack rebuild** changes integration intent or output. Edit manifests when
   needed, rebuild groups before the main stack, verify content, publish the
   integration branch and tag, repin Nix, build, activate, and commit the
   coupled changes.

Invoke only the operation the user requested. A read-only inventory may
recommend either mutation workflow but must not silently perform it.

## Topic classes

- **Maintained PR branches:** `colonelpanic8` fork branches. They are writable
  and may be rebased.
- **Local topics:** fork branches without PRs, including
  `t3code/local/artifact-safety` and `t3code/local/nix-flake`. They are writable
  and rebased like maintained PR branches.
- **Branch-linked topics:** entries with a `branch` but no `pr`, such as
  `t3code/show-remote-host-name`. The branch is authoritative even if a former
  PR was closed. Keep it unless explicitly removed or proven fully absorbed.
- **External topics:** `kind = "external"` entries owned by someone else. They
  are read-only and merge from their PR head.
- **Groups:** sub-manifests assembled into a group branch and pinned as one main
  manifest entry. Rebuild groups first.
- **Epilogues:** minimal patches whose meaning depends on the assembled tree.
  They cannot belong to one topic branch.

Do not conflate open PRs, maintained branches, and carried topics. Closed PRs
can remain intentionally carried. Merged PRs remain carried until the selected
upstream revision contains them.

## Admission and minimization

Discover newly created `colonelpanic8` PRs since the lock's `generated` time.
Treat an owned PR as missing unless it is directly present in a manifest, fully
contained by a documented carried branch or group, or proven present in the
selected upstream revision.

Use commit ancestry when it proves exact containment. When a stacked or rebased
branch contains equivalent behavior but not the same commits, compare branch
diffs and record the evidence; do not infer containment from titles.

Avoid duplicate cumulative topics. If a later branch subsumes earlier PRs,
prefer one documented entry when that preserves all desired behavior and does
not discard independent commits from another carried branch.

Treat `ABSORBED` and `EMPTY` rebuild results as candidates, not automatic
deletions. Verify behavior before removing entries. A green compile is not
evidence that every carried feature survived.

## Safety invariants

- Work only in the primary `/srv/dotfiles` checkout; never create a dotfiles
  worktree.
- Never place a nested T3 Code worktree under `/srv/dotfiles`.
- Preserve dirty T3 Code worktrees and unrelated dotfiles changes or index
  entries.
- Fetch and record live `origin/main` before each mutation phase. If it moves,
  reassess affected rebases and rebuilds.
- Serialize force-pushes to a branch, manifest edits, final builds, and commits.
- Run NixOS activation only with `just switch` from `/srv/dotfiles/nixos`.
- Commit manifest, lock, flake pin, and flake lock changes with explicit paths
  so unrelated staged files cannot be swept in.

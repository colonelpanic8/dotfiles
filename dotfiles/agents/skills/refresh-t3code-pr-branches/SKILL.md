---
name: refresh-t3code-pr-branches
description: "Maintain Ivan's writable T3 Code PR and local topic branches by inspecting review feedback, making requested fixes, rebasing onto the exact live upstream main, verifying focused changes, and pushing safely. Use when asked to update, rebase, repair, synchronize, or address reviews on T3 Code topic branches without rebuilding the personal assembly or changing the Nix installation."
---

# Refresh T3 Code PR branches

Read `/srv/dotfiles/dotfiles/agents/project-guides/t3code-assembly.md`
completely before acting.

## Scope

Operate only on maintained writable fork/local topic branches. External PR
heads and the integration branch are read-only. Do not edit manifests or locks,
rebuild or push the integration branch, change Nix pins, build the package,
activate NixOS, or commit dotfiles.

If the user has not named branches, first run the read-only reconciliation from
`$inventory-t3code-assembly` to determine the exact writable set.

## Procedure

1. Read the T3 Code repo-root `AGENTS.md` and `CONTRIBUTING.md`; verify
   `gh auth status`.
2. Inspect the primary checkout and every existing T3 Code worktree. Preserve
   dirty or unrelated work.
3. Fetch `origin/main` and `fork`, record the live main OID, and resolve each
   target branch's remote head.
4. For PR branches, inspect unresolved review threads, requested changes, and
   checks. Implement review fixes only when requested by the user; do not infer
   permission to expand the feature.
5. Rebase every target writable branch onto the exact recorded `origin/main`.
   Branch-linked and local topics rebase too. Never rebase external topics or
   the integration branch.
6. Resolve conflicts from the branch's intended behavior. Preserve branch
   content rather than copying from the assembled branch.
7. Run focused formatting, lint, type checks, and behavioral tests required by
   the repo instructions and proportional to each branch's changes.
8. Re-fetch and verify `origin/main` has not moved. If it moved, rebase again.
9. Push rewritten branches with `--force-with-lease`, serializing pushes to each
   branch. Re-query remote PR heads and checks after pushing.

## Worktree safety

Use an existing clean worktree when suitable. If isolation is needed, create a
T3 Code worktree only under the T3 Code repository's `.worktrees/` directory.
Never create or enter a dotfiles worktree and never place a nested repository
worktree under `/srv/dotfiles`.

## Report

Report old/new upstream OIDs, each branch's old/new head, PR state and review
work, verification, push result, and dirty worktrees left untouched. Recommend
`$rebuild-t3code-assembly` when refreshed heads or missing topics should be
incorporated, but do not invoke it automatically.

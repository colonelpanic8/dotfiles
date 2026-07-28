---
name: rebuild-t3code-assembly
description: "Change and publish Ivan's T3 Code assembly: admit or remove manifest topics, rebuild groups and the main branch, resolve conflicts, prove carried content survived, push a dated revision, repin Nix, build, smoke-test, activate, and commit coupled files. Use when asked to incorporate inventoried branches into the installed assembly, rebuild, regenerate, publish, repin, or resume a stopped integration build. Do not use for read-only inventory or topic-branch rebasing."
---

# Rebuild the T3 Code integration branch

Read `/srv/dotfiles/dotfiles/agents/project-guides/t3code-assembly.md` for
locations and safety invariants, then read and follow **`BUILDING.md` in the
`t3code-assembly` repository** — the complete, authoritative procedure lives
there, versioned with the tooling it describes. This skill deliberately
contains no procedure; do not act from memory of an older version of it.

Use `$inventory-t3code-assembly` for a read-only carriage check and
`$refresh-t3code-pr-branches` for maintaining topic branches. This skill owns
manifest and integration-output mutations.

Orientation, in BUILDING.md's terms:

- Adding topics → "Adding a topic" (mode `extend`; `refresh` if it refuses).
- Tracking upstream or changed entries → mode `refresh`.
- Conflicts → "Resolving conflicts" (rerere first; never `-X ours/theirs`).
- Before pushing → the syntax gate and all five ladder steps, in order.
- Publishing → "Landing a rebuild" (repin by rev, build, `just switch`,
  coupled commits with explicit paths).

Hard stops, even under time pressure:

- Never commit to, base work on, or merge back `t3code/assembled`.
- Never push an assembly that has not passed the content audit.
- If you cannot finish, park the build worktree state on a named branch and
  report the exact entry and conflicted files ("If you cannot finish" in
  BUILDING.md).

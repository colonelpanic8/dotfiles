---
name: rebuild-t3code-assembly
description: "Change and publish Ivan's T3 Code assembly: admit or remove manifest topics, rebuild the assembly, resolve conflicts, prove carried content survived, push a dated revision, repin Nix, build, smoke-test, activate, and commit coupled files. Use when asked to incorporate inventoried branches into the installed assembly, rebuild, regenerate, publish, repin, or resume a stopped integration build. Do not use for read-only inventory or topic-branch rebasing."
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

- Adding topics → "Inspecting and changing the stack" (`fork-assembler add`).
- Tracking upstream or changed entries → `fork-assembler update`, then `build`.
- Conflicts → "Resolving conflicts" (rerere first; never `-X ours/theirs`).
- Before pushing → the syntax gate only; releases are eager.
- Publishing → "Landing a rebuild" (repin by rev, build, coupled commits with
  explicit paths, then `just switch`).
- After landing → the five ladder steps and "Test suites"; fix forward.

Hard stops, even under time pressure:

- Never commit to, base work on, or merge back `t3code/assembled`.
- If you cannot finish, park the build worktree state on a named branch and
  report the exact entry and conflicted files ("If you cannot finish" in
  BUILDING.md).

---
name: rebuild-t3code-stack
description: "Rebuild Ivan's T3 Code integration branch from its ordered manifests, resolving merge conflicts correctly and proving no carried feature was dropped. Use when asked to rebuild, regenerate, or repair the T3 Code stack branch, when a manifest entry or pin changes, or when a rebuild stopped on a conflict and needs to be resumed. For the wider job of maintaining the PRs themselves, use refresh-t3code-pr-stack, which delegates the rebuild to this skill."
---

# Rebuild the T3 Code integration branch

The manifests, rebuild tooling and epilogue patches live IN THE FORK on the
`t3code/stack-tooling` branch, under `nix/stack/`. Its `BUILDING.md` is the
authoritative reference and goes into more detail than this skill; read it when
you need the full procedure. This skill is the operational summary, kept here
because skills must be discoverable under `~/.claude/skills`. **If the two ever
disagree, BUILDING.md wins** -- it is versioned alongside the code it describes.

Dotfiles holds only two things: the `t3code-integration` flake input pinned to a
rev, and `nix-shared/t3code.nix`, a ~32-line overlay adding the Electron
safeStorage wrapper. Nothing else about the stack lives downstream.

The stack is an integration branch on the fork, regenerated from scratch by
merging an ordered manifest of topic branches onto live upstream main. It is a
build artifact: never commit to it, never base work on it, never merge it back.

## Read this first: the mistake that cost two features

A rebuild was once verified by diffing the result against the **previous
applyPatches tree** and calling it good when they matched. That was the wrong
oracle. The old tree was itself defective — its hand-written compat patches
under-carried #4477 entirely, and encoded a different design than the branch for
#4505 and `show-remote-host-name`.

Conflicts were then resolved by *copying files from that tree*, which silently
inherited its defects. The build passed. The tree diff looked clean. Two shipped
features were missing: sidebar host names and sidebar accent colors.

Two rules follow, and they are the whole point of this skill:

1. **Never treat any reference tree as ground truth.** The branch is the
   authority for its own content.
2. **Verify per entry, not per tree.** Every substantive line a branch adds must
   be present in the result, or have a specific explanation.

## Procedure

Groups first, then the main stack. A group is a sub-manifest whose output branch
is pinned as one entry in the main manifest — a subsystem tree.

```
nix/stack/bin/rebuild-t3code-stack.py --manifest nix/stack/thread-picker.toml \
    --mode reproduce --write-lock --push
# pin the new group head in nix/stack/stack.toml, then
nix/stack/bin/rebuild-t3code-stack.py --mode refresh --write-lock --push
```

`reproduce` merges at manifest pins (deterministic). `refresh` follows current
branch heads. Lock, state file, and build worktree all derive from `--manifest`,
so a group and the main stack can be in flight at once.

The script stops on an unrecognized conflict, leaving it in the build worktree;
resume with `--continue`. It is resumable across crashes and flags `ABSORBED`
and `EMPTY` entries as drop candidates.

## Resolving conflicts

Resolve **semantically**. Never `-X ours/theirs`.

**`nix/stack/bin/replay-resolutions.py --from-build fork/t3code/stack --label '#4257'`** is the
safest helper: it replays that entry's resolution verbatim from a previous
build. Exact, but only valid while entry order is unchanged up to that point —
past any manifest insertion or reorder, the merge context differs and the replay
is wrong. Use a remote ref; the branch may not exist locally.

**`nix/stack/bin/resolve-from-baseline.py`** copies files from a reference tree. It is the
dangerous one. It is only sound when the reference tree is known-correct for
that file AND no later entry contributes to it. When building a group, pass
`--foreign-manifest nix/stack/stack.toml` so files touched by non-group
entries are refused.

**`--force` overrides that safety check. Treat it as a last resort.** Every
feature loss so far traces to it. It is safe only for a file whose content comes
from exactly one topic. For any file where two or more topics contribute — and
`Sidebar.tsx`, `SidebarV2.tsx`, `CommandPalette.tsx`, `ConnectionsSettings.tsx`,
`ws.ts` and the composer files all do — resolve by hand against the branches.

When hand-resolving, read the **branch's** diff (`git diff main...<pin> -- <file>`)
to see what it is trying to add, and make sure the result contains it.

## Syntax gate -- run before any build

`nix shell nixpkgs#esbuild`, then parse every changed `.ts`/`.tsx` with
`esbuild <f> --outfile=/dev/null`. Exact location in seconds; a nix build takes
minutes and Babel surfaces only the FIRST parse error per file, so one build
cycle buys one fix. Gate across ALL changed files at once. Also check for
orphaned import blocks and duplicate imported identifiers -- the two damage
patterns a line-union leaves that still look plausible.

## Verification ladder

Run all four, in order. Do not stop early.

1. **Content audit — `nix/stack/bin/audit-stack-content.py`.** For every entry, checks that the
   substantive lines its branch adds are present in the built tree. A non-zero
   MISSING is not automatically a bug (a later entry may legitimately rewrite
   those lines) but **every one needs a specific explanation before pushing**. A
   large count on an entry you resolved with `--force` means you dropped its
   content.
2. **Tree diff vs the previous lock.** Changes must be explainable by upstream
   movement plus topic movement. Anything else is resolution drift. This detects
   drift; it does NOT prove completeness — that is step 1's job.
3. **Conflict count**, recorded in the lock. Rising counts mean the stack is
   drifting; consider a new group.
4. **Build:**
   ```
   nix build --impure --expr 'let flake = builtins.getFlake "git+file:///srv/dotfiles?dir=nixos";
     pkgs = import flake.inputs.nixpkgs { system = "x86_64-linux"; config.allowUnfree = true;
       overlays = [ (import /srv/dotfiles/nix-shared/t3code.nix { inherit (flake) inputs; }) ]; };
   in pkgs.t3code'
   ```
   **Check the real exit code.** Piping nix through `tail` returns tail's status
   and has masked a failing build. A green build proves it compiles, not that
   features survived — only step 1 proves that.

## Landing it

Push branch + a dated tag (older revs stay fetchable after the force-push), repin
`t3code-integration` **by rev, never by branch** in `nixos/flake.nix`, run
`nix flake lock --update-input t3code-integration`, write the lock, build, then
`just switch` from `/srv/dotfiles/nixos`.

Commit manifest, lock, and flake changes **together** so the pin and lock never
disagree. Use explicit paths — a bare `git commit` after `git add` has swept
unrelated pre-existing staged changes into a commit before.

The build definition lives in the fork's own `flake.nix`, carried as the
`t3code/local/nix-flake` topic. `nix-shared/t3code.nix` only consumes that output
and adds the Electron safeStorage wrapper. **Anything build-related belongs in
the fork's flake, not in dotfiles** — when both defined the build, they drifted.

## If you cannot finish

The build worktree plus its state file is a complete, resumable checkpoint.
Park progress on a named branch so it survives worktree removal, and report the
exact entry and conflicted files. Do not push a stack that has not passed the
content audit.

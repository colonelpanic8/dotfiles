---
name: rebuild-t3code-stack
description: "Change and publish Ivan's assembled T3 Code integration stack: admit or remove manifest topics, rebuild groups and the main branch, resolve conflicts, prove carried content survived, push a dated revision, repin Nix, build, smoke-test, activate, and commit coupled files. Use when asked to incorporate inventoried branches into the installed stack, rebuild, regenerate, publish, repin, or resume a stopped integration build. Do not use for read-only inventory or topic-branch rebasing."
---

# Rebuild the T3 Code integration branch

Read `/srv/dotfiles/dotfiles/agents/project-guides/t3code-pr-stack.md`
completely before acting. Use `$inventory-t3code-pr-stack` for a read-only
carriage check and `$refresh-t3code-pr-branches` for maintaining topic branches.
This skill owns manifest and integration-output mutations.

The manifests, rebuild tooling and epilogue patches live IN THE FORK on the
`t3code/stack-tooling` branch, under `stack/`. Its `BUILDING.md` is the
authoritative reference and goes into more detail than this skill; read it when
you need the full procedure. This skill is the operational summary, kept here
because skills must be discoverable under `~/.claude/skills`. **If the two ever
disagree, BUILDING.md wins** -- it is versioned alongside the code it describes.

Dotfiles holds only two things: the `t3code-integration` flake input pinned to a
rev, and `nix-shared/t3code.nix`, a ~32-line overlay adding the Electron
safeStorage wrapper. Nothing else about the stack lives downstream.

The stack is an integration branch on the fork, assembled by merging an ordered
manifest of topic branches. It is a build artifact: never commit to it, never
base work on it, never merge it back.

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

There are three modes:

- `refresh` rebuilds the entire stack from current upstream `main` and current
  topic heads. This is the full rebase/refresh path.
- `reproduce` rebuilds the entire stack at recorded pins.
- `extend` starts from the locked pre-epilogue commit, merges only a newly
  appended manifest suffix, then reapplies the epilogues.

Extend is intentionally strict: the old manifest must be an exact prefix, the
published integration branch must equal the locked commit, and the lock must
contain a manifest snapshot and pre-epilogue commit. If an existing entry
changed, moved, or disappeared, use refresh. Extend is a fast path for adding
PRs, not a replacement for periodic refreshes.

Groups first, then the main stack. A group is a sub-manifest whose output branch
is pinned as one entry in the main manifest — a subsystem tree.

```
stack/bin/rebuild-t3code-stack.py --manifest stack/thread-picker.toml \
    --mode reproduce --write-lock --push
# pin the new group head in stack/stack.toml, then
stack/bin/rebuild-t3code-stack.py --mode refresh --write-lock --push
# after appending one or more new entries:
stack/bin/rebuild-t3code-stack.py --mode extend --write-lock --push
```

`reproduce` merges at manifest pins (deterministic). `refresh` follows current
branch heads. Lock, state file, and build worktree all derive from `--manifest`,
so a group and the main stack can be in flight at once.

The script stops on an unrecognized conflict, leaving it in the build worktree;
resume with `--continue`. It is resumable across crashes and flags `ABSORBED`
and `EMPTY` entries as drop candidates.

## Resolving conflicts

Resolve **semantically**. Never `-X ours/theirs`.

**rerere does most of this for you now.** It is enabled repo-locally and seeded
from the published build, so most conflicts arrive already resolved. The rebuild
script lists replayed paths separately from ones needing a human; `autoupdate` is
off, so replayed paths stay unmerged and you must review and `git add` them.
Re-seed with `stack/bin/train-rerere.py --clear` after any build that corrects a
resolution, and only ever from a build that passed the full ladder — training
from a defective build makes its mistakes the silent default. Check the cache
with `stack/bin/train-rerere.py --verify`.

**`stack/bin/replay-resolutions.py --from-build fork/t3code/stack --label '#4257'`**
replays that entry's resolution verbatim from a previous build. Exact, but only
valid while entry order is unchanged up to that point — past any manifest
insertion or reorder, the merge context differs and the replay is wrong. rerere
does not have that limitation (it keys on conflict content, not position), so
reach for this only when you want a whole file from a known build. Use a remote
ref; the branch may not exist locally.

**`stack/bin/resolve-from-baseline.py`** copies files from a reference tree. It is the
dangerous one. It is only sound when the reference tree is known-correct for
that file AND no later entry contributes to it. When building a group, pass
`--foreign-manifest stack/stack.toml` so files touched by non-group
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

Run all five, in order. Do not stop early.

1. **Content audit — `stack/bin/audit-stack-content.py`.** For every entry, checks that the
   substantive lines its branch adds are present in the built tree. A non-zero
   MISSING is not automatically a bug (a later entry may legitimately rewrite
   those lines) but **every one needs a specific explanation before pushing**.
   Store reviewed rewrites in `stack/audit-exceptions.toml`, guarded by the
   exact missing-line count and digest so a newly dropped line fails again. A
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
5. **Smoke check — `nix build .#checks.<system>.smoke`.** Launches the packaged
   app headlessly in client-only mode and fails if the renderer throws.

**A green build does not prove the app runs.** A dropped import is a runtime
ReferenceError, not a bundler error, so rolldown emits a bundle with a free
variable in it. That shipped a build whose first paint was "Something went
wrong: useEnvironmentSettings is not defined" — two imports lost by taking only
`ours` on an import conflict whose `theirs` side carried the new module. Cheap
static catch: collect bare `use[A-Z]\w+(` calls per changed file and subtract
what is imported or locally declared (exclude dotted calls; handle
`import React, { ... }`).

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

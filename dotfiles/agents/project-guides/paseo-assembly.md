# Paseo personal assembly — pointers and machine-local facts

The installed Paseo is upstream `main` plus an ordered set of carried topic
branches and PRs, assembled by [fork-fold](https://github.com/colonelpanic8/fork-fold)
into a published branch that NixOS pins.

**All build procedure and invariants live in `AGENTS.md` in the assembly
repository**, which also ships a skill stub loading the full fork-fold
operating guide from `lib.forkFoldAgentGuide` (re-exported from the `fork-fold`
revision in its `flake.lock`, so it tracks the tool). Read those before
operating. Do not duplicate procedure from them into this guide or into skills.

This file holds only what those repositories cannot know: where things are on
this machine, and the trap specific to the local paseo checkout.

## Fixed locations

| Item | Location |
| --- | --- |
| Assembly repository (procedure, manifest, lock, resolutions) | `~/Projects/paseo-assembly` |
| Paseo development checkout | `~/Projects/paseo` |
| Remotes in the paseo checkout | `origin` = getpaseo/paseo, `fork` = colonelpanic8/paseo |
| Published artifact | `colonelpanic8/paseo` branch `assembled`, tags `paseo-assembled-*` |
| Flake input | `paseo` in `/srv/dotfiles/nixos/flake.nix` |
| Binary cache | `paseo-colonelpanic8` on Cachix |

## Machine-local invariants

- **`assembled` is compiled output — never hand-commit to it, base work on it,
  or merge it back.** The assembly repo states this too, but the trap is here:
  `~/Projects/paseo` frequently *has `assembled` checked out*, so an agent that
  never opens the assembly repo will edit and commit there by default. A
  hand-made commit or hand-merged topic on that branch is scratch work — the
  next `fork-fold build` discards it, the manifest never learns about it, and
  nothing reaches the installed build, all while tests pass and the tree looks
  right.
- **The local `assembled` is usually stale** relative to the last real build.
  Never read it as the current state of the assembly.
- Author changes on topic branches cut from upstream `main`, minimal diff, one
  concern each, pushed to `fork`. A PR is not required for something to be
  carried — open one only when asked.
- Landing a change in the installed build means editing the manifest and
  rebuilding in `~/Projects/paseo-assembly`, then repinning the `paseo` input in
  `/srv/dotfiles/nixos/flake.lock` and running `just switch` from
  `/srv/dotfiles/nixos`. **That switch restarts `paseo.service`, killing every
  Paseo-hosted agent — including you, if you are one** (see the NixOS workflow
  section of the root AGENTS.md).

## Names that should send you here

paseo, paseo-assembly, `assembled`, fork-fold, "the paseo build", "my paseo",
`paseo-assembled-*` tags, the `paseo` flake input.

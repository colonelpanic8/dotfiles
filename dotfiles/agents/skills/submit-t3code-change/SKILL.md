---
name: submit-t3code-change
description: "Implement and publish a focused change to pingdotgg/t3code, then add the resulting upstream pull request to Ivan's ordered Nix integration manifest and activate the rebuilt personal build. Use when asked to change T3 Code, open or update a T3 Code PR, carry a T3 Code change locally, or make a T3 Code feature available in the personal NixOS installation even if upstream may not merge it."
---

# Submit a T3 Code Change

Carry one change through two linked deliverables:

1. A focused, ready-for-review PR against `pingdotgg/t3code:main`.
2. A reproducible entry in Ivan's patched T3 Code Nix source.

Treat the PR as the canonical standalone implementation and the Nix stack as the durable personal integration. Create the PR before adding its diff to the Nix stack.

## Fixed locations and remotes

- Resolve the T3 Code checkout through `/srv/dotfiles/dotfiles/agents/project-links/t3code`. Repair that ignored symlink if its target moved.
- Use `origin` for `pingdotgg/t3code` and `fork` for `colonelpanic8/t3code`; verify rather than assume.
- Maintain the topic manifest at `nix/stack/stack.toml` on the fork's `t3code/stack-tooling` branch.
- Maintain the assembled `t3code-integration` revision pin in `/srv/dotfiles/nixos/flake.nix` and `flake.lock`.
- Put compatibility patches in `/srv/dotfiles/nixos/patches/`.

Never create a T3 Code worktree beneath `/srv/dotfiles`. Never use a dotfiles worktree; edit only the primary `/srv/dotfiles` checkout.

## 1. Establish clean repository state

1. Read the applicable `AGENTS.md` files in both repositories.
2. Run `git status --short --branch`, `git remote -v`, and `git worktree list` in T3 Code.
3. Run `git status --short` in `/srv/dotfiles` and preserve every unrelated staged, unstaged, and untracked change.
4. Run `gh auth status`.
5. Fetch `origin main` and the relevant fork refs.
6. Work in the current T3 Code worktree when it already owns the intended branch and is safe. Otherwise select a clean attached worktree or create an isolated project-local worktree. Do not alter a dirty worktree to make it usable.
7. Base a new branch directly on current `origin/main`. Keep each PR independently applicable to `main`; do not base it on another unmerged personal PR or on the assembled patch stack.

## 2. Implement a focused upstream change

Keep the PR narrow. Do not mix cleanup or adjacent features merely because the upstream project may decline the contribution.

Follow existing T3 Code architecture and repository instructions. Add tests that exercise behavior rather than implementation details. For non-trivial product work, note that upstream prefers an issue first, but do not replace the requested implementation with issue creation unless the user asks.

For UI changes:

- Capture clear before/after images.
- Capture a short video when motion, timing, or interaction matters.
- Exercise the real interaction in the app, not only unit tests.

## 3. Validate and publish the PR

Run focused tests while iterating. Before publishing, run the repository-required checks:

```bash
vp check
vp run typecheck
```

Also run `vp run lint:mobile` for native mobile changes and the relevant `vp test ...` or `vp run test ...` command for changed behavior.

Then:

1. Review the full diff and commit it as a coherent change.
2. Push the branch to `fork`.
3. Create a ready-for-review PR against `pingdotgg/t3code:main`; do not create a draft or add an agent prefix to the title.
4. Use the repository template headings: `What Changed`, `Why`, `UI Changes` when applicable, and `Checklist`.
5. Include exact validation commands and attach required visual evidence.
6. Verify the PR number, URL, base, head branch, and full head OID with `gh pr view`.

Do not start the Nix integration until the PR exists and the published head matches the locally validated commit.

## 4. Add the PR to the integration manifest

Re-read `nix/stack/stack.toml` on `t3code/stack-tooling` first; another
session may have changed the stack while the PR was being prepared.

The stack is an **integration branch rebuilt by 3-way merge** from an ordered
manifest — not an `applyPatches` list of diffs. Adding a PR means adding one
manifest entry naming its fork branch:

```toml
[[entry]]
pr = 4512
kind = "fork"                       # or "external" (other authors), "local" (no PR)
branch = "t3code/my-feature"
pin = "abc123def456"                # 12-char head OID
summary = "One line, imperative"
```

There is no hash, no `excludes`, no compatibility patch, and no `builtins.seq`
audit binding. Do not reintroduce any of those.

**Placement matters.** Put the entry next to topics it overlaps, so conflict
resolution stays local. If it edits the CommandPalette trio, add it to the
`nix/stack/thread-picker.toml` group instead of the main manifest, and repin the
group. If it opens a new overlap cluster with two or more existing topics,
consider a new group manifest rather than accumulating conflicts in the main
stack.

Do not advance the upstream pin merely to simplify one integration. If the PR
genuinely needs newer upstream, run `$refresh-t3code-pr-branches` and
revalidate.

## 5. Rebuild and resolve

```
nix/stack/bin/rebuild-t3code-stack.py --mode refresh --write-lock --push
```

The rebuild stops on conflicts; resolve them semantically and `--continue`. See
`$rebuild-t3code-stack` for the conflict helpers and their caveats.

`pnpmDeps.hash` in the fork's `flake.nix` only changes when `pnpm-lock.yaml`
does — compare against the previous tree before assuming it moved.

## 6. Build and activate

1. Repin `t3code-integration` by REV in `nixos/flake.nix` (never by branch), then
   `nix flake lock --update-input t3code-integration`.
2. Build the actual host package, not just the source:
   ```
nix build --impure --expr 'let flake = builtins.getFlake "git+file:///srv/dotfiles?dir=nixos";
     pkgs = import flake.inputs.nixpkgs { system = "x86_64-linux"; config.allowUnfree = true;
       overlays = [ flake.inputs.t3code-integration.overlays.client ]; };
   in pkgs.t3code'
   ```
   **Check the real exit code** — piping nix through `tail` masks failure.
3. `just switch` from `/srv/dotfiles/nixos` only after the package succeeds.
4. Verify the installed `t3` store path and, where enabled,
   `t3code-headless.service` activity and `ExecStart` path.

Do not claim success from a source build alone.

## 7. Commit the dotfiles integration

Commit the manifest, lock, and flake changes **together** so the pin and lock
never disagree. Use explicit paths so unrelated staged entries do not leak in.
Push after validation. Report:

- PR URL and head OID.
- Manifest entry added, and whether it went in the main stack or a group.
- Conflicts resolved during the rebuild.
- New integration rev and tag; tree changed or unchanged.
- Build and activation result; T3 Code and dotfiles commits.

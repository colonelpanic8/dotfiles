---
name: submit-t3code-change
description: "Implement and publish a focused change to pingdotgg/t3code, then add the resulting upstream pull request to Ivan's ordered Nix applyPatches stack and activate the patched personal build. Use when asked to change T3 Code, open or update a T3 Code PR, carry a T3 Code change locally, or make a T3 Code feature available in the personal NixOS installation even if upstream may not merge it."
---

# Submit a T3 Code Change

Carry one change through two linked deliverables:

1. A focused, ready-for-review PR against `pingdotgg/t3code:main`.
2. A reproducible entry in Ivan's patched T3 Code Nix source.

Treat the PR as the canonical standalone implementation and the Nix stack as the durable personal integration. Create the PR before adding its diff to the Nix stack.

## Fixed locations and remotes

- Resolve the T3 Code checkout through `/srv/dotfiles/dotfiles/agents/project-links/t3code`. Repair that ignored symlink if its target moved.
- Use `origin` for `pingdotgg/t3code` and `fork` for `colonelpanic8/t3code`; verify rather than assume.
- Maintain the patch manifest in `/srv/dotfiles/nixos/t3code.nix`.
- Maintain the upstream source pin in `/srv/dotfiles/nixos/flake.lock` from the `t3code-upstream` input declared in `flake.nix`.
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

## 4. Add the PR to the personal patch stack

First re-read the current dotfiles status and `nixos/t3code.nix`; another session may have changed the stack while the PR was being prepared.

Fetch and record the live cumulative diff:

```bash
nix store prefetch-file --json \
  https://patch-diff.githubusercontent.com/raw/pingdotgg/t3code/pull/PR_NUMBER.diff
```

Add a concise comment containing the feature, PR number, and 12-character PR head. Preserve the intended patch ordering.

Choose the integration form by testing against the exact pinned source and all earlier patches:

- Use `fetchurl` when the raw cumulative PR diff applies cleanly in sequence.
- Use `fetchpatch` with the smallest possible `excludes` list when only some files overlap earlier PRs. Compute the normalized `fetchpatch` hash; it is not the raw `fetchurl` hash.
- Create a local compatibility patch when overlapping files require a semantic combination of multiple PRs. Generate it from a clean temporary integration tree based on the pinned source, not by editing a stale patch blindly.
- When the raw PR is represented wholly or partly by a compatibility patch, retain and force evaluation of the live raw PR URL and hash with `builtins.seq`. This keeps upstream head changes auditable.

Compatibility patches must preserve the complete intent of every represented PR. Name them for the feature or overlap, document which PRs/files they combine, and generate full-index Git diffs. Stage newly created local patch files before Nix evaluation because flakes omit untracked files.

Do not advance `t3code-upstream` merely to simplify one integration. If the PR genuinely requires a newer upstream pin, perform the relevant refresh steps from `$refresh-t3code-pr-stack` and revalidate the entire stack.

## 5. Refresh derived Nix hashes

Expect three distinct hash surfaces:

1. Raw `fetchurl` hashes for GitHub cumulative diffs.
2. Normalized `fetchpatch` hashes for diffs with exclusions.
3. `pnpmDeps` for the final patched source.

Let Nix report the expected hash when a normalized patch or `pnpmDeps` changes, update only the matching field, and rebuild. Update the patched source name/version date when the source pin or assembled build date changes.

## 6. Build and activate

From `/srv/dotfiles/nixos`:

1. Parse `t3code.nix` with `nix-instantiate --parse`.
2. Run `git diff --check` on the dotfiles changes.
3. Build the patched source to prove the ordered patch application.
4. Build the actual host T3 Code package, not merely an isolated fetched diff.
5. Run `just switch` only from `/srv/dotfiles/nixos` after the package succeeds.
6. Verify the installed `t3` store path and, where enabled, `t3code-headless.service` activity and `ExecStart` path.

Do not claim success from a hash-prefetch or source build alone. The final package and activation must succeed.

## 7. Commit the dotfiles integration

Review staged and unstaged state again. Commit only the Nix manifest, lockfile if changed, and compatibility patches belonging to this PR. Preserve unrelated index entries by using explicit paths rather than a broad `git commit`.

Push the dotfiles commit to its current default branch after validation. Report:

- PR URL and head OID.
- T3 Code validation commands.
- Patch-stack form: raw, excluded, or compatibility patch.
- Nix package/build and activation result.
- T3 Code and dotfiles commits.
- Any unrelated worktree changes left untouched.

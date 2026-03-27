# Nixpkgs Review Patterns

This reference is a curated summary of recurring feedback from Ivan Malison's `NixOS/nixpkgs` PRs. Use it to ground reviews in patterns that have already come up from nixpkgs reviewers.

## Most Repeated Themes

### 1. Prefer `finalAttrs` over `rec`

This came up repeatedly on both package init and update PRs.

- [PR #490230](https://github.com/NixOS/nixpkgs/pull/490230) `playwright-cli`: reviewer asked for `buildNpmPackage (finalAttrs: { ... })` instead of `rec`.
- [PR #490033](https://github.com/NixOS/nixpkgs/pull/490033) `rumno`: same feedback for `rustPlatform.buildRustPackage`.

Practical rule:
- If the derivation self-references `version`, `src`, `pname`, `meta.mainProgram`, or nested outputs, default to `finalAttrs`.

### 2. Prefer `tag` when upstream release is a tag

This also repeated across multiple PRs.

- [PR #490230](https://github.com/NixOS/nixpkgs/pull/490230) `playwright-cli`
- [PR #490033](https://github.com/NixOS/nixpkgs/pull/490033) `rumno`
- [PR #497465](https://github.com/NixOS/nixpkgs/pull/497465) `t3code`

Practical rule:
- If upstream publishes a named release tag, prefer `tag = "v${finalAttrs.version}";` or the exact tag format instead of a raw `rev`.

### 3. Use the right hook and builder

Reviewers often push on hook placement and builder/stdenv choice.

- [PR #497465](https://github.com/NixOS/nixpkgs/pull/497465) `t3code`: feedback to move work from `buildPhase` into `postPatch`.
- [PR #497465](https://github.com/NixOS/nixpkgs/pull/497465) `t3code`: feedback to consider `stdenvNoCC`.
- [PR #490230](https://github.com/NixOS/nixpkgs/pull/490230) `playwright-cli`: prefer `makeBinaryWrapper` for a simple wrapper.

Practical rule:
- Check whether each mutation belongs in `postPatch`, `preConfigure`, `buildPhase`, or `installPhase`.
- Check whether the package genuinely needs a compiler toolchain.
- For simple env/arg wrappers, prefer `makeBinaryWrapper`.

### 4. Enable `strictDeps` unless there is a reason not to

This was called out explicitly on [PR #497465](https://github.com/NixOS/nixpkgs/pull/497465).

Practical rule:
- For new derivations, ask whether `strictDeps = true;` should be present.
- If not, be ready to justify why the builder or package layout makes it unnecessary.

### 5. Keep metadata explicit and override-friendly

- [PR #490230](https://github.com/NixOS/nixpkgs/pull/490230) `playwright-cli`: reviewer asked to avoid `with lib;`.
- [PR #497465](https://github.com/NixOS/nixpkgs/pull/497465) `t3code`: reviewer suggested deriving wrapper executable name from `finalAttrs.meta.mainProgram`.

Practical rule:
- Prefer `lib.licenses.mit` over `with lib;`.
- Keep `meta.mainProgram` authoritative and have wrappers follow it when practical.

### 6. Maintainers matter for new packages

- [PR #496806](https://github.com/NixOS/nixpkgs/pull/496806) `gws`: reviewer would not merge until the submitter appeared in maintainers.

Practical rule:
- For package inits, check maintainers early rather than waiting for review feedback.

### 7. PR title and template hygiene are review targets

- [PR #497465](https://github.com/NixOS/nixpkgs/pull/497465) `t3code`: asked to fix the PR title to match the version.
- [PR #490033](https://github.com/NixOS/nixpkgs/pull/490033) `rumno`: reviewer asked what happened to the PR template.

Practical rule:
- Before opening or updating a PR, verify the title, template, and branch scope.

### 8. Duplicate or overlapping PRs get noticed quickly

- [PR #490227](https://github.com/NixOS/nixpkgs/pull/490227) was replaced by [PR #490230](https://github.com/NixOS/nixpkgs/pull/490230).
- [PR #490053](https://github.com/NixOS/nixpkgs/pull/490053) overlapped with [PR #490033](https://github.com/NixOS/nixpkgs/pull/490033).
- [PR #488606](https://github.com/NixOS/nixpkgs/pull/488606), [PR #488602](https://github.com/NixOS/nixpkgs/pull/488602), and [PR #488603](https://github.com/NixOS/nixpkgs/pull/488603) were closed after reviewers pointed to existing work.

Practical rule:
- Search for existing PRs on the package before spending time polishing a review.
- If a branch contains unrelated commits, fix that before asking for review.

### 9. JS/Bun/Electron packages draw runtime-layout scrutiny

This came up heavily on `t3code` and `playwright-cli`.

- [PR #497465](https://github.com/NixOS/nixpkgs/pull/497465) `t3code`: reviewers proposed trimming the runtime closure, removing unnecessary env vars, and adding shell completions and desktop integration.
- [PR #490230](https://github.com/NixOS/nixpkgs/pull/490230) `playwright-cli`: reviewers called out mismatched bundled `playwright-core` and browser binaries, and wrapper behavior that prevented user overrides.

Practical rule:
- For JS-heavy packages, inspect closure size, runtime vs build-only deps, wrapper env vars, and version alignment between bundled libraries and external binaries.

### 10. Cross-platform evidence helps

- [PR #490230](https://github.com/NixOS/nixpkgs/pull/490230) received an approval explicitly noting Darwin success.
- [PR #497465](https://github.com/NixOS/nixpkgs/pull/497465) got feedback questioning platform restrictions and build behavior.

Practical rule:
- If the package plausibly supports Darwin, avoid premature Linux-only restrictions and mention what was or was not tested.

## How To Use This Reference

- Use these patterns as a focused checklist before submitting or reviewing nixpkgs changes.
- Do not blindly apply every point. Check whether the builder, language ecosystem, and upstream release model actually match.
- When in doubt, prefer concrete evidence from the current package diff over generic convention.

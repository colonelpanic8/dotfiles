---
name: new-project
description: Use when creating, scaffolding, or bootstrapping a new project or repository for the user, or when choosing a stack, build setup, CI, or Android/F-Droid publishing for one. Covers Nix flake + direnv + just conventions, language/framework preferences (Rust/fenix, Dioxus, React Native), GitHub publishing, and which existing repos to copy from.
---

# New project preferences

Strong defaults first, then judgement calls. Before writing anything from
scratch, open the matching example repo below and copy its shape. The examples
are more current than this file.

## Always

- **Nix flake.** Every repo gets a `flake.nix` on `nixpkgs-unstable` (or
  `nixos-unstable`), with `flake-utils.lib.eachDefaultSystem`. At minimum it
  provides:
  - `packages.default`
  - `devShells.default` with every tool the repo needs, including `just`
  - a `formatter` (`alejandra`)

  Add `apps`, `checks`, `overlays.default`, or `nixosModules.default` when the
  project is something that gets deployed or installed as a service.
- **direnv.** Commit a `.envrc` so entering the repo loads the dev shell. Use
  the keepbook form: pinned nix-direnv, plus `.envrc.override` and
  `.envrc.local` hooks.

  ```sh
  if ! has nix_direnv_version || ! nix_direnv_version 3.0.6; then
    source_url "https://raw.githubusercontent.com/nix-community/nix-direnv/3.0.6/direnvrc" "sha256-RYcUJaRMf8oF5LznDrlCXbkOQrber54JGZWaPPZCbXs="
  fi

  if [ -f .envrc.override ]; then
    source .envrc.override
  else
    use flake .
  fi

  if [ -f .envrc.local ]; then
    source .envrc.local
  fi
  ```

  Run `direnv allow` after creating it. Inside the repo, prefer
  `direnv exec . <cmd>` over `nix develop -c`.
- **just.** Put repo commands in a `justfile`. The common baseline is `fmt`,
  `fmt-check`, `lint`/`clippy`, `test`, and `check` (which runs all the
  checks), plus `build` and `release` where relevant. Add project-specific
  recipes freely.
- **.gitignore.** Include `.direnv/` and `.worktrees/`, plus `target/`,
  `node_modules/` and so on as relevant.
- **Agent docs.** Add an `AGENTS.md` covering project structure, invariants,
  and how to run checks. Make `CLAUDE.md` a symlink to it.
- **CI.** Use GitHub Actions that install Nix and run the checks inside the
  flake: `nix develop --command just check`, plus `nix flake check` if checks
  exist. That way CI and local runs use the same toolchain. Use
  `cachix/install-nix-action` or the DeterminateSystems installer, either one
  with a cache.
- **Permissive license.** Use MIT, or dual MIT/Apache-2.0 for Rust crates, and
  commit the `LICENSE` file(s). Avoid AGPL and other copyleft licenses. If a
  core dependency would force copyleft on the project, raise that before
  committing to the dependency.
- **Publish on GitHub, usually public,** under `colonelpanic8`, with a short
  description. Creating and pushing a public repo is outward-facing, so confirm
  first unless the user already asked for it. Hold off on publishing anything
  that might contain private data. The private data for some projects lives in
  separate private repos (e.g. `keepbook-data`).

## Language and framework (judgement calls)

- **Rust is the default for new projects** unless the domain clearly favors
  something else. Examples: Emacs Lisp for org tooling, Kotlin for small
  Android-only or Wear apps, Go where the key library is Go.
- **Rust toolchain via fenix**, not rustup or rust-overlay. Older repos use
  rust-overlay; don't copy that part.
  - Simple case: `fenix.packages.${system}.stable.withComponents [...]`, or
    `fenix...combine [...]` when extra targets are needed (wasm32, Android,
    iOS). Build with `pkgs.makeRustPlatform` + `buildRustPackage`. No crane or
    naersk.
  - For rustup compatibility, commit a `rust-toolchain.toml` and build the
    toolchain with `fenix...fromToolchainFile { file = ./rust-toolchain.toml;
    sha256 = ...; }`. Nix and rustup users then get the same toolchain (see
    `moergo-rmk`).
  - Use edition 2024. For multi-crate projects, use a `crates/` workspace with
    `[workspace.dependencies]` and `[workspace.package]` version. Read the
    package version from `Cargo.toml` in the flake (`builtins.fromTOML`) rather
    than duplicating it.
- **Desktop GUI: Dioxus** is the current (soft) preference.
- **Share code between desktop and mobile** when the project has both.
  - Rust: build a core lib with no UI. Give it a headless app/server layer that
    exposes it. Write one Dioxus crate with `desktop`/`mobile`/`android`/`web`
    features: native builds link the core in-process, and the wasm build talks
    to the same API over HTTP. The UI must not reimplement business logic.
    See keepbook.
  - React Native: one Expo/RN codebase, with react-native-web for the web or
    desktop surface. See mova.

## Android apps: auto-publish to F-Droid

Every Android app gets a **self-hosted F-Droid repo**, deployed to GitHub Pages
from the app's own repo at `https://colonelpanic8.github.io/<repo>/fdroid/repo`.
The app is not submitted to fdroiddata. There is no shared tooling, so copy
`scripts/fdroid/` from the most hardened implementation, **eva**.

1. Releases: `CHANGELOG.md` with `## [X.Y.Z]` headings, a release commit, and
   a `vX.Y.Z` tag.
2. `release.yml` runs on `v*` tags:
   - decodes the keystore from secrets `ANDROID_KEYSTORE_BASE64`,
     `ANDROID_KEYSTORE_PASSWORD`, `ANDROID_KEY_ALIAS`, `ANDROID_KEY_PASSWORD`,
     and fails closed if any are missing;
   - builds a release APK and checks it with `apksigner verify`;
   - attaches it to a GitHub Release.
3. `fdroid-repo.yml` runs on `workflow_run` after the release, or on manual
   dispatch. It runs `scripts/fdroid/build-repo.sh`, which:
   - writes fastlane changelogs;
   - downloads the last N release APKs unmodified;
   - runs `fdroid update`, signing the index with the same keystore;
   - builds a landing page;
   - deploys to Pages.
4. **Version codes must be derived from semver, never hand-maintained.**
   F-Droid rejects duplicates. Use eva's `major*1_000_000 + minor*1_000 + patch`
   for new apps. Existing repos vary.
5. One-time setup:
   - generate a release keystore and back it up in `pass`;
   - add the four secrets;
   - set Pages source to "GitHub Actions";
   - add `fdroid/config.yml`, `fdroid/metadata/<appId>.yml`, and
     `fastlane/metadata/android/en-US/{title,short_description,full_description}.txt`.

Pitfalls:
- Never publish debug-signed APKs. The collector stops at a signer change, so
  a signer change strands old releases.
- Keep Pages under 1 GB: index only a few releases and drop archive APKs.
- Wear APKs that share the phone app's ID stay out of the phone index.

Android SDK/JDK/NDK come from `pkgs.androidenv.composeAndroidPackages` in a
separate `devShells.android`. Set `allowUnfree` and
`android_sdk.accept_license` in the flake's nixpkgs config, not with
`--impure`, where possible.

## Example repos

All live under `~/projects` (or on GitHub as `colonelpanic8/<name>`).

| Need | Copy from |
| --- | --- |
| Rust CLI/library: justfile, CI, release recipe | `git-sync-rs` (swap its rust-overlay for fenix as in `git-blame-rank`) |
| Rust multi-crate workspace | `coqui-tts-streamer` |
| Rust + Dioxus cross-platform app, Android, F-Droid | `keepbook` |
| React Native/Expo app (web + Android + Wear), F-Droid | `mova` |
| Native Kotlin Android app, strictest F-Droid/signing scripts | `eva` |
| Go service with NixOS + home-manager modules, flake checks | `google-messages-multidevice-bridge` (its AGPL is forced by vendored mautrix-gmessages; don't copy it) |
| fenix from `rust-toolchain.toml` (rustup-compatible) | `moergo-rmk` |

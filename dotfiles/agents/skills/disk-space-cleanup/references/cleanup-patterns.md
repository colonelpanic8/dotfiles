# Validated Cleanup Patterns

Use only after a current assessment confirms the path is significant.

## Rust and Worktrees

- Stale Cargo-backed targets in top-level projects, `.worktrees/*/target`, and `.claude/worktrees/*/target` have repeatedly reclaimed 25–100+ GiB.
- Preserve targets used by active builds. Concurrent agents may recreate deleted outputs immediately.
- `hypr-workspace-history/target` has been a removable Rust-style cache without a nearby `Cargo.toml`, so the guarded helper rejects it; inspect manually.
- Preserve `~/Projects/Hyprland/src/layout/target`, which is source code rather than build output.
- Clean registered git worktrees only after preserving dirty ones. Stale nixpkgs worktree `result` symlinks may pin Nix closures.

## Nix Roots and Store Optimization

- Removing conservatively stale `.direnv` roots followed by `nix-store --gc` has reclaimed about 15 GiB in a validated run.
- With `auto-optimise-store = false`, `nix-store --optimise` once reclaimed about 60 GiB by hardlinking duplicate store files. Measure current conditions before assuming similar impact.
- Old taffybar constellation repos and `railbird-mobile` direnv profiles have pinned large Haskell or Android closures.
- NixOS generations and `/srv/dotfiles/nixos/result` can pin Android Studio and SDK versions.
- A stale rustup toolchain can break `cargo-sweep --installed`; remove it only after confirming the toolchain is unusable and unwanted.

## Gitea Runner

For `/var/lib/private/gitea-runner`, a validated sequence is:

1. Stop `gitea-runner-nix.service`.
2. Remove only assessed cache/work directories such as `.cache`, `.gradle`, `action-cache-dir`, `workspace`, stale nested `gitea-runner`, and nested `nix/.cache` or `nix/.local`.
3. Recreate `action-cache-dir`, `workspace`, and `.cache` owned by `gitea-runner:gitea-runner`.
4. Restart the service and verify it.

Preserve `.runner`, `.labels`, `.docker/config.json`, SSH material, Kubernetes material, and other registration/configuration state.

## Logs and App Caches

- Delete or truncate `~/.local/share/picom/debug.log` only after confirming picom is not actively writing it and correcting the verbose logging cause.
- Empty Trash only with explicit approval.
- Close Chrome, Spotify, Gradle daemons, and similar applications before targeted cache cleanup.

## Severe Pressure

When only megabytes remain, run the smallest safe cleanup first. Build tools and SQLite may fail opaquely before reporting `ENOSPC`. Avoid commands that need substantial temporary disk space until a safe margin has been restored.

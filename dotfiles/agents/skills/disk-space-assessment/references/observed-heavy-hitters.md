# Observed Disk-Usage Patterns

Use these as hypotheses, not substitutes for a current snapshot.

## Home and Project Trees

- Rust `target/` directories under top-level projects, `.worktrees/*`, and `.claude/worktrees/*` have repeatedly consumed 25–100+ GiB in aggregate.
- `~/.cache/uv`, `~/.cache/pypoetry`, browser caches, Spotify, and Gradle caches have each reached multi-GiB sizes.
- `~/.local/share/picom/debug.log` has exceeded 15 GiB after verbose debugging or crashes.
- Trash and active build outputs are high-yield candidates but require lifecycle and process checks.

## Service State

- `/var/lib/private/gitea-runner` has exceeded 50 GiB and is invisible to an unprivileged root scan.
- `/var` has also been dominated by Docker, containers, Rancher, private state, and journals.
- Root accounting must include large sparse or allocated files such as `/swapfile` and ext4 reserved blocks; these may differ from ordinary directory totals.

## Nix Store

- A large store may be almost entirely live. In a 2026-07-10 audit, the Nix DB held 413.0 GiB logical NAR size with zero dead paths.
- The current system has directly pinned large ComfyUI model weights plus CUDA, GHC, Android, and Rust families.
- Hundreds of `.direnv` GC-root entries have retained more than 100 GiB outside the current-system closure. `railbird-mobile`, Railbird backend, taffybar, rocket-sense, and keepbook dev shells have been notable roots.
- `result*` symlinks, NixOS generations, and repo-local profiles can pin large closures.
- Closure sizes overlap. Compute unions or uniquely collectible sets; never sum per-profile closure sizes as a reclaim estimate.
- Physical `/nix/store` usage can be materially lower than logical NAR size after `nix-store --optimise` hardlinks duplicate files.

## Volatility

- Concurrent agent builds can move free space by tens of GiB during an assessment.
- Severe pressure may surface as SQLite I/O errors, missing compiler diagnostics, or opaque build-script failures rather than an explicit `ENOSPC` message.

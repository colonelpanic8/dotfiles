# Observed Disk-Usage Patterns

Use these as hypotheses, not substitutes for a current snapshot.

## Home and Project Trees

- Rust `target/` directories under top-level projects, `.worktrees/*`, and `.claude/worktrees/*` have repeatedly consumed 25–100+ GiB in aggregate.
- `~/.cache/uv`, `~/.cache/pypoetry`, browser caches, Spotify, and Gradle caches have each reached multi-GiB sizes.
- `~/.local/share/picom/debug.log` has exceeded 15 GiB after verbose debugging or crashes.
- Trash and active build outputs are high-yield candidates but require lifecycle and process checks.
- 2026-07-18: agent-created worktree directories are the dominant `~/Projects` cost — `railbird/.agents/worktrees` (35.1 GiB, 5 dirs, oldest 2026-07-09), `railbird-mobile/.claude/worktrees` + `railbird-mobile/.worktrees` (~47 GiB combined, oldest 2026-07-09), `rocket-sense/.claude/worktrees` (9.1 GiB across 15 dirs, oldest 2026-06-13 — over a month stale), `subtr-actor/.worktrees` (14.8 GiB, oldest 2026-07-12). Cross-check `git worktree list` and dir mtimes before treating any as safe to remove — several are still on active feature branches.
- `/tmp` (root, not `$HOME/tmp`) accumulates leftover `nix-shell.*` scratch dirs, downloaded `.dmg`/`.deb` installers, and agent scratch dirs (`codex-*`, `t3code-*`) that survive across reboots on this host; 2026-07-18 saw 15.2 GiB there, ~10 GiB of it clearly disposable installer/scratch cruft.

## Service State

- `/var/lib/private/gitea-runner` has exceeded 50 GiB and is invisible to an unprivileged root scan; by 2026-07-18 it had settled back to 4.1 GiB after the tmpfs/nix.gc fix (see `rocket-sense-disk-bloat` memory).
- `/var` has also been dominated by Docker, containers, Rancher, private state, and journals.
- Root accounting must include large sparse or allocated files such as `/swapfile` and ext4 reserved blocks; these may differ from ordinary directory totals.
- 2026-07-18: `rancher-desktop`/`containerd`/`k3s` systemd units were `not-found` (fully uninstalled) yet `/var/lib/rancher` still held 4.7 GiB of orphaned state — check `systemctl is-enabled` before assuming a `/var/lib/<service>` directory is live. Docker itself was active with `docker system df` showing 3.96 GB (100%) reclaimable unused images plus 33 unused volumes (68.7 MiB) — a zero-risk `docker image prune`/`docker volume prune` candidate.

## Nix Store

- A large store may be almost entirely live. In a 2026-07-10 audit, the Nix DB held 413.0 GiB logical NAR size with zero dead paths. By 2026-07-18 `nix-store --gc --print-dead` found 944 dead paths (~2.85 GiB by `du -c`) — mostly stale `.drv` files; small but real and zero-risk to collect.
- The current system has directly pinned large ComfyUI model weights plus CUDA, GHC, Android, and Rust families.
- Hundreds of `.direnv` GC-root entries have retained more than 100 GiB outside the current-system closure. `railbird-mobile`, Railbird backend, taffybar, rocket-sense, and keepbook dev shells have been notable roots. 2026-07-18 marginal-unique ranking (via `direnv_gc_roots_audit.py`) found most of the top closures were <1 day old (taffybar 11.6 GiB, rocket-sense 6.2 GiB — active work, don't touch) but ~3.1 GiB of marginal-unique closures belonged to projects untouched 36–68 days (`rockpload`, `rocket-sense/vendor/subtr-actor`, `hyprwinview`, `hyprexpo`, `http-client-vcr`, `notifications-tray-icon`, `lastfm-bulk-edit`, `scrobble-scrubber`) — low-risk GC candidates if those `.direnv` dirs are removed.
- `result*` symlinks, NixOS generations, and repo-local profiles can pin large closures.
- Closure sizes overlap. Compute unions or uniquely collectible sets; never sum per-profile closure sizes as a reclaim estimate.
- Physical `/nix/store` usage can be materially lower than logical NAR size after `nix-store --optimise` hardlinks duplicate files. 2026-07-18: `du -sx --block-size=1 /nix/store` (took ~5 min on this host, do not use a short timeout) measured 342.1 GiB physical vs. 413 GiB-class logical NAR sizes seen in audits — optimise is working.

## Volatility

- Concurrent agent builds can move free space by tens of GiB during an assessment.
- Severe pressure may surface as SQLite I/O errors, missing compiler diagnostics, or opaque build-script failures rather than an explicit `ENOSPC` message.

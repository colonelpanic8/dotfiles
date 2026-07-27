# Observed Disk-Usage Patterns

Use these as hypotheses, not substitutes for a current snapshot.

## Home and Project Trees

- Rust `target/` directories under top-level projects, `.worktrees/*`, and `.claude/worktrees/*` have repeatedly consumed 25–100+ GiB in aggregate.
- `~/.cache/uv`, `~/.cache/pypoetry`, browser caches, Spotify, and Gradle caches have each reached multi-GiB sizes.
- `~/.local/share/picom/debug.log` has exceeded 15 GiB after verbose debugging or crashes.
- Trash and active build outputs are high-yield candidates but require lifecycle and process checks.
- 2026-07-18: agent-created worktree directories are the dominant `~/Projects` cost — `railbird/.agents/worktrees` (35.1 GiB, 5 dirs, oldest 2026-07-09), `railbird-mobile/.claude/worktrees` + `railbird-mobile/.worktrees` (~47 GiB combined, oldest 2026-07-09), `rocket-sense/.claude/worktrees` (9.1 GiB across 15 dirs, oldest 2026-06-13 — over a month stale), `subtr-actor/.worktrees` (14.8 GiB, oldest 2026-07-12). Cross-check `git worktree list` and dir mtimes before treating any as safe to remove — several are still on active feature branches.
- `/tmp` (root, not `$HOME/tmp`) accumulates leftover `nix-shell.*` scratch dirs, downloaded `.dmg`/`.deb` installers, and agent scratch dirs (`codex-*`, `t3code-*`) that survive across reboots on this host; 2026-07-18 saw 15.2 GiB there, ~10 GiB of it clearly disposable installer/scratch cruft.
- 2026-07-25: repeated `t3code`/`t3-cli` PR-stack and test-suite sessions (`refresh-t3code-pr-stack`, `submit-t3code-change`) leave hundreds of top-level `/tmp/t3-cli-*-test-*`, `/tmp/t3code-*`, and `/tmp/tmp.*` scratch dirs, each a self-contained throwaway git checkout (many with their own nested `.git/worktrees/`, unrelated to any real repo's worktree registry). Accumulated to ~13-14 GiB / 570+ dirs spanning back to 2025-02, zero live process references (`lsof`/`/proc/*/cwd` all empty). Verify no live references, then bulk-remove by the `t3*`/`tmp.*` name patterns; recurs unless a periodic sweep is added.
- 2026-07-25: `~/.config/taffybar` (symlinked to `/srv/dotfiles/dotfiles/config/taffybar`) is a live Haskell dev checkout, not plain config — its Cabal `dist-newstyle`/`dist-hackage-*`/`dist-tray-click-check*` build caches reached ~5.1 GiB (100% rebuildable, not git-tracked) and `taffybar/taffybar/.worktrees/*` reached 4.6 GiB, mostly orphaned worktrees (`git status` inside them fails with `not a git repository`, matching the orphaned-worktree pattern below) plus one `prunable` stale registration and two live branches (verify ancestry/push status before removing those two).
- 2026-07-26: `~/Projects/railbird-mobile/.worktrees/*` and `~/Projects/t3code/.worktrees/*` `node_modules` can grow by several GiB between successive checks (e.g. `t3code-9e822705` 7.6→14.4 GiB, `t3code-d8b8937c` 587 MiB→3.2 GiB, `settle-state-reconcile` 505 MiB→3.1 GiB inside ~2 hours) with no live `cwd`/fd reference and no matching `pnpm`/`yarn`/`gradle` process found at check time — treat as a sign some other agent/session installed deps there recently, not necessarily still active; re-verify liveness immediately before any delete rather than trusting an earlier check in the same session. `railbird-mobile` uses yarn classic (near-full `node_modules` reclaim expected); `t3code` uses pnpm (content-addressed store, so nominal `du`/`ncdu` size can overstate real reclaim by ~10-20x — see the 2026-07-24 `/home/imalison/src/t3code` note in `disk-space-cleanup/SKILL.md`).
- 2026-07-26: `~/.cache/drkonqi` (KDE crash-handler dumps) reached 9.9 GiB — check for genuinely old/duplicate crash reports before clearing, since KDE may still want recent ones for reporting.
- 2026-07-26: `/var/lib/docker` 10.3 GiB and `/var/lib/syncthing` 4.0 GiB are routinely large; treat as `docker system prune` / Syncthing-index territory respectively, not raw deletion candidates.

## Service State

- `/var/lib/private/gitea-runner` has exceeded 50 GiB and is invisible to an unprivileged root scan; by 2026-07-18 it had settled back to 4.1 GiB after the tmpfs/nix.gc fix (see `rocket-sense-disk-bloat` memory).
- `/var` has also been dominated by Docker, containers, Rancher, private state, and journals.
- Root accounting must include large sparse or allocated files such as `/swapfile` and ext4 reserved blocks; these may differ from ordinary directory totals.
- 2026-07-18: `rancher-desktop`/`containerd`/`k3s` systemd units were `not-found` (fully uninstalled) yet `/var/lib/rancher` still held 4.7 GiB of orphaned state — check `systemctl is-enabled` before assuming a `/var/lib/<service>` directory is live. Docker itself was active with `docker system df` showing 3.96 GB (100%) reclaimable unused images plus 33 unused volumes (68.7 MiB) — a zero-risk `docker image prune`/`docker volume prune` candidate.

## Nix Store

- A large store may be almost entirely live. In a 2026-07-10 audit, the Nix DB held 413.0 GiB logical NAR size with zero dead paths. By 2026-07-18 `nix-store --gc --print-dead` found 944 dead paths (~2.85 GiB by `du -c`) — mostly stale `.drv` files; small but real and zero-risk to collect.
- The current system has directly pinned large ComfyUI model weights plus CUDA, GHC, Android, and Rust families.
- Hundreds of `.direnv` GC-root entries have retained more than 100 GiB outside the current-system closure. `railbird-mobile`, Railbird backend, taffybar, rocket-sense, and keepbook dev shells have been notable roots. 2026-07-18 marginal-unique ranking (via `direnv_gc_roots_audit.py`) found most of the top closures were <1 day old (taffybar 11.6 GiB, rocket-sense 6.2 GiB — active work, don't touch) but ~3.1 GiB of marginal-unique closures belonged to projects untouched 36–68 days (`rockpload`, `rocket-sense/vendor/subtr-actor`, `hyprwinview`, `hyprexpo`, `http-client-vcr`, `notifications-tray-icon`, `lastfm-bulk-edit`, `scrobble-scrubber`) — low-risk GC candidates if those `.direnv` dirs are removed.
- 2026-07-26 re-run: 207 unique direnv targets, 150.56 GiB total closure union, 63.06 GiB sum of per-project marginal-unique (57.25 GiB shared only among direnv projects, i.e. not truly reclaimable without also GC'ing the current system). Top marginal-unique by age: `~/Projects/mova` 14.60 GiB (114 days stale — strong GC candidate), `taffybar/taffybar` 11.39 GiB (10.9 days, recent, verify before touching), `railbird/.worktrees/video-export-reliability` 7.00 GiB (1.6 days — active, don't touch), `subtr-actor-mechanics` 4.49 GiB (132 days stale), `subtr-actor` 4.22 GiB (74.5 days stale), `coqui-tts-streamer` 2.75 GiB (103 days stale), `rumno` 1.78 GiB (136 days), `lastfm-edit` 1.76 GiB (152 days), `picom` 1.70 GiB (173 days), `git-sync-rs` 1.68 GiB (156.5 days). `railbird-mobile` and `t3code` did NOT appear in the marginal-unique top 30 despite ~20 GiB raw flake-profile closures each — their dev-shell closures largely overlap the current system, so clearing their `.direnv` would reclaim little.
- `result*` symlinks, NixOS generations, and repo-local profiles can pin large closures.
- Closure sizes overlap. Compute unions or uniquely collectible sets; never sum per-profile closure sizes as a reclaim estimate.
- Physical `/nix/store` usage can be materially lower than logical NAR size after `nix-store --optimise` hardlinks duplicate files. 2026-07-18: `du -sx --block-size=1 /nix/store` (took ~5 min on this host, do not use a short timeout) measured 342.1 GiB physical vs. 413 GiB-class logical NAR sizes seen in audits — optimise is working.

## Volatility

- Concurrent agent builds can move free space by tens of GiB during an assessment.
- Severe pressure may surface as SQLite I/O errors, missing compiler diagnostics, or opaque build-script failures rather than an explicit `ENOSPC` message.

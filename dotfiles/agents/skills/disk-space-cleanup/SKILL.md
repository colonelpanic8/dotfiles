---
name: disk-space-cleanup
description: Investigate and safely reclaim disk space on this machine, especially on NixOS systems with heavy Nix, Rust/Haskell, Docker, and Podman usage. Use when disk is low, builds fail with no-space errors, /nix/store appears unexpectedly large, or the user asks for easy cleanup wins without deleting important data.
---

# Disk Space Cleanup

Reclaim disk space with a safety-first workflow: investigate first, run obvious low-risk cleanup wins, then do targeted analysis for larger opportunities.

Bundled helpers:

- `scripts/rust_target_dirs.py`: inventory and guarded deletion for explicit Rust `target/` directories
- `references/rust-target-roots.txt`: machine-specific roots for Rust artifact scans
- `references/ignore-paths.md`: machine-specific excludes for `du`/`ncdu`

## Execution Default

- Start with non-destructive investigation and quick sizing.
- Prioritize easy wins first (`nix-collect-garbage`, container prune, Cargo artifacts).
- Propose destructive actions with expected impact before running them.
- Run destructive actions only after confirmation, unless the user explicitly requests immediate execution of obvious wins.
- Capture new reusable findings by updating this skill before finishing.

## Workflow

1. Establish current pressure and biggest filesystems
2. Run easy cleanup wins
3. Inventory Rust build artifacts and clean the right kind of target
4. Investigate remaining heavy directories with `ncdu`/`du`
5. Investigate `/nix/store` roots when large toolchains still persist
6. Summarize reclaimed space and next candidate actions
7. Record new machine-specific ignore paths, Rust roots, or cleanup patterns in this skill

## Step 1: Baseline

Run a quick baseline before deleting anything:

```bash
df -h /
df -h /home
df -h /nix
```

Optionally add a quick home-level size snapshot:

```bash
du -xh --max-depth=1 "$HOME" 2>/dev/null | sort -h
```

## Step 2: Easy Wins

Use these first when the user wants fast, low-effort reclaiming:

```bash
sudo -n nix-collect-garbage -d
sudo -n docker system prune -a
sudo -n podman system prune -a
```

Notes:
- Add `--volumes` only when the user approves deleting unused volumes.
- Re-check free space after each command to show impact.
- Prefer `sudo -n` first so cleanup runs fail fast instead of hanging on password prompts.
- On macOS, a successful Xcode archive can still fail during `-exportArchive` with a generic `Copy failed` when the APFS data volume is full. Inspect the generated `.xcdistributionlogs` for `No space left on device`, and preserve several GiB of extra staging headroom for `XcodeDistPipeline` beyond the archive itself.
- If root is still tight after these, run app cache cleaners before proposing raw `rm -rf`:

```bash
uv cache clean
pip cache purge
yarn cache clean
npm cache clean --force
```

## Step 3: Rust Build Artifact Cleanup

Do not start with a blind `find ~ -name target` or with hard-coded roots that may miss worktrees. Inventory explicit `target/` directories first using the bundled helper and the machine-specific root list in `references/rust-target-roots.txt`.

Inventory the biggest candidates:

```bash
python /srv/dotfiles/dotfiles/agents/skills/disk-space-cleanup/scripts/rust_target_dirs.py list --min-size 500M --limit 30
```

Focus on stale targets only:

```bash
python /srv/dotfiles/dotfiles/agents/skills/disk-space-cleanup/scripts/rust_target_dirs.py list --min-size 1G --older-than 14 --output tsv
```

Use `cargo-sweep` when the repo is still active and you want age/toolchain-aware cleanup inside a workspace:

```bash
nix run nixpkgs#cargo-sweep -- sweep -d -r -t 30 <workspace-root>
nix run nixpkgs#cargo-sweep -- sweep -r -t 30 <workspace-root>
nix run nixpkgs#cargo-sweep -- sweep -d -r -i <workspace-root>
nix run nixpkgs#cargo-sweep -- sweep -r -i <workspace-root>
```

Use direct `target/` deletion when inventory shows a discrete stale directory, especially for inactive repos or project-local worktrees. The helper only deletes explicit paths named `target` that are beneath configured roots and a Cargo project:

```bash
python /srv/dotfiles/dotfiles/agents/skills/disk-space-cleanup/scripts/rust_target_dirs.py delete /abs/path/to/target
python /srv/dotfiles/dotfiles/agents/skills/disk-space-cleanup/scripts/rust_target_dirs.py delete /abs/path/to/target --yes
```

Recommended sequence:

1. Run `rust_target_dirs.py list` to see the largest `target/` directories across `~/Projects`, `~/org`, `/srv/dotfiles`, and other configured roots.
2. For active repos, prefer `cargo-sweep` from the workspace root.
3. For inactive repos, abandoned branches, and `.worktrees/*/target`, prefer guarded direct deletion of the explicit `target/` directory.
4. Re-run the list command after each deletion round to show reclaimed space.

Machine-specific note:

- Project-local `.worktrees/*/target` directories are common cleanup wins on this machine and are easy to miss with the old hard-coded workflow.
- `cargo-sweep` is installed through the NixOS `code.nix` package set, but stale manually-installed binaries under `~/.cargo/bin` can shadow `/run/current-system/sw/bin/cargo-sweep`. If `cargo sweep` fails with a missing loader or `No such file or directory`, run `type -a cargo-sweep` and remove the stale `~/.cargo/bin/cargo-sweep` entry.
- `cargo-sweep sweep -i/--installed` can fail when `rustup toolchain list` contains stale toolchains whose `rustc` no longer exists. On this machine, `1.68.2-x86_64-unknown-linux-gnu` caused `failed to determine fingerprint ... 'rustc': No such file or directory`.
- `/home/imalison/Projects/codex/codex-rs/target` can be dominated by current-looking `target/debug/incremental` data that `cargo-sweep sweep -a` and `--maxsize` report as not removable. If it is stale and space pressure is high, use the guarded `rust_target_dirs.py delete ... --yes` workflow for that explicit target directory.
- `/home/imalison/Projects/hypr-workspace-history/target` is a small non-Cargo false positive; the guarded delete workflow correctly rejects it because there is no Cargo project above the directory.
- `nixos/cargo-sweep.nix` defines a user timer for every user, `cargo-sweep-rust-targets.timer`, that every 6 hours runs `cargo-sweep sweep -r --hidden --time 2` across `$HOME/Projects`, `$HOME/org`, `/srv/dotfiles`, and `$HOME/.cargo/build`, then deletes centralized cargo build dirs (`~/.cargo/build/<xx>/<hash>`) untouched for 2 days. `CARGO_BUILD_BUILD_DIR` (set system-wide in `nix-shared/system/essential.nix`) redirects cargo intermediate artifacts to `~/.cargo/build/`, so per-project `target/` dirs only hold final artifacts.
- `cargo-sweep` does not discover centralized `$HOME/.cargo/build/<xx>/<hash>` directories as Cargo workspaces; the timer's wrapper handles those separately. During acute pressure, a one-time 12-hour cutoff over those hash directories, skipping any directory with open files, reclaimed 19.57 GiB on 2026-07-20. Keep the persistent timer at its two-day safety window unless the user explicitly requests a configuration change.
- Later on 2026-07-20, repeating that acute-pressure workflow after verifying no active Cargo/Rust processes or open files reclaimed another 65.94 GiB from 32 centralized build shards and moved `/` from 98% used (18 GiB free) to 91% used (84 GiB free). Check freshness recursively within each hash directory, recheck immediately before deletion, and leave the persistent two-day timer unchanged.
- On 2026-07-20, `/` reached 100% with only 1.5 GiB free while all centralized build shards had recursive activity within 12 hours. After verifying no active Cargo/Rust processes or open files, a one-time six-hour cutoff removed the oldest 20 shards (last touched about 9.9 hours earlier) and reclaimed 29.24 GB of filesystem space, leaving 30 GB free. Reserve this shorter cutoff for acute pressure, validate the exact `<xx>/<hash>` paths, and leave the persistent two-day timer unchanged.

## Step 4: Investigation with `ncdu` and `du`

Avoid mounted or remote filesystems when profiling space. Load ignore patterns from `references/ignore-paths.md`.

Prefer the local `safe_ncdu` wrapper over raw `ncdu` invocations. It writes compressed exports under the predictable, shared `/srv/disk-space-assessments/` location (`users`-group readable, not a per-user `~/.cache` dir), records the exclude list beside each export, excludes mounted descendants of the scan root, and keeps a `latest-<root>.json.zst` symlink (plus `.excludes`/`.meta` sidecars) pointing at the most recent run for each root so later analysis and later sessions — and other accounts in the `users` group — never have to guess a path or rescan:

```bash
safe_ncdu /home/imalison
sudo -n env HOME=/home/imalison safe_ncdu /
safe_ncdu /nix/store
safe_ncdu top /srv/disk-space-assessments/latest-root.json.zst 30 /home/imalison
safe_ncdu open /srv/disk-space-assessments/latest-root.json.zst
```

Only fall back to raw, non-persistent `ncdu -x "$HOME"` / `sudo ncdu -x /` for a quick interactive look when `safe_ncdu` itself is broken — never use it as the recorded evidence for an assessment. If `ncdu` is missing entirely, use `nix run nixpkgs#ncdu -- -x "$HOME"` the same way, as a one-off, not as a substitute for a `safe_ncdu` snapshot.

For quick, non-blocking triage on very large trees, prefer bounded probes:

```bash
timeout 30s du -xh --max-depth=1 "$HOME/.cache" 2>/dev/null | sort -h
timeout 30s du -xh --max-depth=1 "$HOME/.local/share" 2>/dev/null | sort -h
```

Machine-specific heavy hitters seen in practice:

- 2026-07-24 `ryzen-shine` online partition reclaim: GPT partition numbers
  were not in physical order. `/boot` (`nvme0n1p3`) and root
  (`nvme0n1p4`) physically preceded the obsolete Microsoft-reserved
  (`p1`) and `/shared` NTFS (`p2`) partitions. After preserving an
  `sfdisk` text dump and raw GPT sectors, unmounting `/shared`, and
  validating the exact replacement table with `sfdisk --no-act`, writing
  the table required `sfdisk --no-reread` because root was mounted.
  `partx --delete --nr 1:2 /dev/nvme0n1` removed the deleted, unmounted
  partition devices from the running kernel, and
  `partx --update --nr 4 /dev/nvme0n1` exposed the enlarged root boundary.
  `resize2fs /dev/nvme0n1p4` then grew ext4 online. Always revalidate the
  disk ID, partition starts, PARTUUIDs, mount state, and exact physical
  order before reusing this pattern.
- 2026-07-22 zero-free-space failure mode: with `/` reporting zero bytes
  available, `apply_patch` truncated an untracked shell function to zero bytes
  before reporting its write failure. Stop all source-file writes as soon as
  the filesystem reaches 100%, reclaim headroom first, and re-check the exact
  byte size of any file involved in the failed write before continuing.
- 2026-07-10 `railbird-sf` incident: K3s reported `DiskPressure` even with tens of GiB free because its container `imagefs` shares `/`, kubelet image GC used the default 85% high-water mark, and the K3s config overrode only `nodefs` eviction thresholds. `crictl imagefsinfo` showed only ~677M of images, so image GC could not reclaim its requested ~149G and repeatedly evicted application pods. Set matching `imagefs.available` values alongside `nodefs.available` in `eviction-hard`, `eviction-soft`, and `eviction-soft-grace-period`; verify via K3s eviction-manager logs rather than trusting `df` alone.
- `~/.cache/uv` can exceed 20G and is reclaimable with `uv cache clean`.
- If `uv cache clean` reports that the cache is currently in use, do not add `--force`; leave it for a later idle cleanup so an active environment or install is not disrupted.
- On 2026-07-21 this host had no `uv` executable on PATH; after confirming no open references or writes in the last hour, directly removing the idle `~/.cache/uv` directory reclaimed 8.2G.
- 2026-07-23 Railbird Mobile Yarn Berry/Nix rebuild: regenerating a large `fetchYarnBerryDeps` offline cache and then materializing `node_modules` exhausted the filesystem with only 299 MiB free. Removing 803 MiB of task-created conversion scratch and running unprivileged `nix store gc` reclaimed 15.4 GiB; the final `node_modules` derivation then succeeded with about 18 GiB free. Preserve roughly 18 GiB of headroom for this dependency build.
- `~/.cache/yarn` and `~/.cache/cabal` can together exceed 4G on this machine; after checking for open files and recent writes, clearing these dependency caches is a safe reclaiming step (dependencies will be redownloaded or rebuilt).
- `~/.cache/pypoetry` can exceed 7G across artifacts, repository cache, and virtualenvs; inspect first, then use Poetry cache commands or targeted virtualenv removal.
- `~/.cache/google-chrome` can exceed 8G across multiple Chrome profiles; close Chrome before clearing profile cache directories.
- `~/.cache/spotify` can exceed 10G; treat as optional app-cache cleanup.
- `~/.gradle` can exceed 8G, mostly under `caches/`; prefer Gradle-aware cleanup and expect dependency redownloads.
- `~/.local/share/picom/debug.log` can grow past 15G when verbose picom debugging is enabled or crashes leave a stale log behind; if `picom` is not running, deleting or truncating the log is a high-yield low-risk win.
- `~/.local/share/Trash` can exceed several GB; empty only with user approval.
- On the macOS Codex host, `~/.cache/keepbook-cargo-target` can grow to several GiB as a Rust target cache and is safe to remove when not actively building keepbook.
- On the macOS Codex host, `nix run github:ccusage/ccusage/v20.0.17` attempted a source build of 213 Rust derivations (about 1.5 GiB unpacked) and exhausted the nearly full APFS container. For ccusage-fleet remotes without `npx`, prefer `nix shell nixpkgs#nodejs --command npx --yes ccusage@<version> ...`; the npm package uses a prebuilt native binary. After aborting the Rust build, unprivileged `nix store gc` removed 670 dead paths (estimated at 4.5 GiB) and increased APFS free space from 207 MiB to 6.2 GiB.
- On the macOS Codex host, large low-risk cache wins have included `~/Library/Caches/Homebrew` via `brew cleanup --prune=all -s`, Yarn/npm/CocoaPods caches, `~/Library/Caches/com.spotify.client`, `~/Library/Caches/ms-playwright`, and stale app updater caches such as `~/Library/Caches/com.anthropic.claudefordesktop.ShipIt`.
- 2026-07-20 macOS cache cleanup: `yarn cache clean` reclaimed the 3.1 GiB `~/Library/Caches/Yarn/v6` cache. Removing stale Sparkle payloads under `~/Library/Caches/com.openai.codex/org.sparkle-project.Sparkle/Installation` (1.9 GiB) and `~/Library/Caches/com.anthropic.claudefordesktop.ShipIt/update.MKJITXP` (751 MiB) increased available Data-volume space from 4.1 GiB to 11 GiB.
- 2026-07-20 macOS Railbird iOS build: a clean signed simulator build used about 5 GiB of task-specific DerivedData and failed at `GenerateDSYMFile` when less than 1 GiB remained. Removing only that task's DerivedData before rebuilding, then running `sudo -n nix-collect-garbage -d`, provided enough headroom; preserve at least 6 GiB free for a clean build.
- 2026-07-21 macOS Railbird TestFlight archive: clearing the regenerable `~/Library/Caches/Google` cache reclaimed about 1.1 GiB while Xcode was archiving. Chrome may retain a live cache directory, so partial-removal "Directory not empty" messages are expected; remeasure reclaimed space rather than treating them as a cleanup failure.
- 2026-07-21 macOS Railbird worktree release: symlinking `node_modules` to the primary checkout caused Xcode duplicate React-Fabric copy outputs. A worktree-local hard-link copy (`cp -al <primary>/node_modules node_modules`) kept CocoaPods paths local while adding minimal data volume.
- On the macOS Codex host, allowing `direnv` in a fresh `railbird-mobile/.worktrees/*` worktree can materialize the full Android SDK dev shell even for workflow-only edits. When free space is already low, use the existing root worktree's tools or direct formatters first; a failed shell realization followed by `sudo -n nix-collect-garbage -d` reclaimed about 6.3 GiB on 2026-07-12.
- 2026-07-13 macOS Railbird worktree cleanup: a fresh Android dev-shell realization repeatedly filled the disk while unpacking NDK r23b. After the failed build exited, 463 paths were dead; `sudo -n nix-collect-garbage -d` reclaimed 12.0 GiB. Avoid retrying that worktree shell and reuse the main worktree's dependencies/toolchain when the task only needs formatting, linting, or TypeScript checks.
- 2026-07-09 macOS runner cleanup: `sudo -n nix-collect-garbage -d` removed 11,455 unreferenced store paths and reclaimed 24.4 GiB, increasing shared APFS container free space from about 3 GiB to 30 GiB. The preflight estimate from the Nix DB reported only 15.6 GiB of dead-path `narSize`, so actual reclaimed filesystem space can be materially larger.
- 2026-07-09 Xcode runner update: `mas outdated` correctly detected Xcode 16.3 -> 26.6, but `mas upgrade 497799835` stalled when no active App Store purchase account was present (`needsAuthentication because appleID is nil` in unified logs). Verify App Store sign-in before relying on unattended `mas` upgrades.
- On the macOS Codex host, `/private/var/folders/*/*/X/*.code_sign_clone` can accumulate stale app framework copies, especially Chrome and Codex clones. Remove only stale clone directories after checking age; avoid blind deletion of the whole `/private/var/folders` tree.
- On the macOS Codex host, `~/Library/Developer/CoreSimulator/Caches/dyld` can exceed several GiB. Clean it only after confirming simulator shutdown is acceptable, because CoreSimulator services may be running.
- On the macOS Codex host, `xcrun simctl runtime delete <UUID>` can remove an installed simulator runtime while its source MobileAsset remains under `/System/Library/AssetsV2/com_apple_MobileAsset_iOSSimulatorRuntime`. In a 2026-07-13 iOS 18.4 cleanup, the runtime and its mounted Cryptex image disappeared but the 8.3 GiB source asset remained; do not raw-delete system-managed `AssetsV2` content, and measure actual APFS free-space change rather than assuming the runtime's displayed size is fully reclaimed.
- `/var/lib/private/gitea-runner` can exceed 50G and is not visible to an unprivileged `ncdu /` scan; use `sudo -n env HOME=/home/imalison safe_ncdu /` when `/var` looks undercounted.
  - Validated cleanup pattern: stop `gitea-runner-nix.service`, remove cache/work directories under `/var/lib/private/gitea-runner` (`.cache`, `.gradle`, `action-cache-dir`, `workspace`, stale nested `gitea-runner`, and nested `nix/.cache`/`nix/.local`), recreate `action-cache-dir`, `workspace`, and `.cache` owned by `gitea-runner:gitea-runner`, then restart the service.
  - Preserve registration/config-like files such as `/var/lib/private/gitea-runner/nix/.runner`, `/var/lib/private/gitea-runner/nix/.labels`, `/var/lib/private/gitea-runner/.docker/config.json`, and SSH/Kube material.
- On the macOS runner, state is split between `/private/var/lib/gitea-runner` and `/Users/gitea-runner`; the daemon label is `org.nixos.gitea-runner-nix`. Stop it with `sudo -n launchctl bootout system /Library/LaunchDaemons/org.nixos.gitea-runner-nix.plist`, clear only regenerable action clones/caches/temp data, recreate required directories as `gitea-runner:staff`, then restore it with `launchctl bootstrap` and verify it is running.
  - The iOS workflow may mount `/Volumes/Extreme SSD/railbird-mobile-ci/build.sparseimage` inside an action worktree. A failed cleanup can delete the sparse-image pathname while leaving its disk image attached. If `hdiutil info` shows that missing path under `mounting user: gitea-runner`, unmount the exact stale worktree mount, identify the parent disk device from the same `hdiutil info` image block, and detach that device with `sudo -n hdiutil detach /dev/diskN`. Never infer or detach a disk from its number alone.
  - Preserve `.runner`, configuration, SSH material, keychains, and provisioning profiles. Validated regenerable targets include `action-cache-dir`, runner `.cache` and `tmp` contents, Cargo registry cache, CocoaPods repos, and runner-owned Xcode caches/DerivedData when no job is active.
  - `/Volumes/Extreme SSD` is ExFAT with `noowners`, so it is not a safe direct home or worktree filesystem for Git, Nix, CocoaPods, or Xcode. Use a capped APFS sparse bundle stored on the SSD, keep registration and credential state internal, and make runner startup fail closed rather than falling back to the internal disk when the image is unavailable.
  - 2026-07-22 external SSD audit: stale Cargo `target` directories and JavaScript `node_modules` under `/Volumes/Extreme SSD/Projects` occupied about 60 GiB. These are the first candidates for creating runner-image headroom, but delete them only after approval and after checking for active builds.
  - 2026-07-22 validated migration: deleting seven approved `target`/`node_modules` trees reclaimed 59.1 GiB and moved the external SSD from 46 GiB to 105 GiB free. A 64 GiB APFS sparse bundle at `/Volumes/Extreme SSD/railbird-runner/runner.sparsebundle` mounted successfully at `/private/var/lib/gitea-runner-external`; its initial physical size was 37 MiB. The live runner used the external `work` directory plus external Cargo, Rustup, Gradle, CocoaPods, Yarn, npm, Go, pip, uv, XDG, runtime, and temp caches, while registration and credential state stayed internal.
  - 2026-07-22 internal cleanup: clearing inactive user build/package caches, deleting the superseded nix-darwin generation, and running `nix-collect-garbage` moved the internal APFS pool from about 3.4 GiB to 17 GiB free; the final Nix GC alone reported 8.6 GiB reclaimed. The active configuration schedules daily GC and optimisation at 04:15 and reserves free space so future builds trigger cleanup before the disk reaches zero.
  - 2026-07-22 T3 Code rebuild: compressing the pnpm dependency store exhausted the APFS pool even after a failed build had recovered 5.3 GiB free. `sudo -n nix-collect-garbage -d`, `brew cleanup --prune=all -s`, and `npm cache clean --force` reclaimed another 4.3 GiB. A subsequent rebuild that needed both the active and replacement T3 closures was unblocked by `pnpm store prune`, which removed 133,104 files / 1,795 packages and reclaimed 5.32 GiB. Preserve roughly 10 GiB of build headroom for this derivation.
  - 2026-08-02 runner-image cleanup: with no active job or open files, stale `nix-shell.*` scratch under `/private/var/lib/gitea-runner-external/cache/tmp` occupied 8.8 GiB. Boot out `org.nixos.gitea-runner-nix`, terminate any orphaned `mount-gitea-runner-storage` or job `log stream` processes that still reference the mount, clear only the validated temp contents, detach the exact device identified by the `runner.sparsebundle` block in `hdiutil info`, and run `hdiutil compact` before bootstrapping the runner again. Compaction reclaimed 18.3 GB of physical SSD space; verify both the mount and runner PID afterward.
  - 2026-08-02 external Paseo cleanup: `/Volumes/Extreme SSD/Projects/paseo/node_modules` consumed 70 GiB on ExFAT because its 1 MiB allocation units massively amplified a dependency tree of small files. After confirming the running Paseo app and daemon came from installed Nix paths and that no process or open file referenced the checkout, deleting that exact regenerable directory reclaimed 70 GiB. Long ExFAT deletions may outlive an `exec` yield while continuing under the session manager; locate and monitor the original `rm` PID to completion instead of starting a competing deletion.
- 2026-08-05 `railbird-sf` remote cleanup (`/` at 100%, 109 MiB free → 86%, 124 GiB free): the three big reclaimable pools were podman image storage, agent worktrees, and Nix-store roots those worktrees pinned. `sudo -n podman system prune -a -f` reclaimed 31.5 GB (three 10.5 GB registry-tagged `railbird-full` images plus stale `org-agenda-api` tags — all re-pullable from `us-central1-docker.pkg.dev`; running kokoro/org-agenda-api containers kept their images). The validated worktree sweep over `~imalison/Projects/{paseo,subtr-actor,rocket-sense,t3code}/.worktrees` removed ~60 clean worktrees (~85 GiB apparent; paseo `rebase-pr-*` worktrees are ~3 GiB each from node_modules) and skipped 13 with real uncommitted work. Rerunning `nix-collect-garbage -d` AFTER the worktree sweep freed 10.4 GiB vs 0.4 GiB before it — removed worktrees' `.direnv` profiles were the roots, so always GC again after a worktree purge. Left intact: 144 GiB of live k3s PVCs under `/var/lib/rancher/k3s/storage` (rocket-sense postgres + storage — application data, never deletable), gitea-runner `.gradle` (33 GiB, an Android CI job was actively running — Gradle/Kotlin daemons visible in `ps`), and `/tmp/paseo-*` agent caches. `journalctl --vacuum-size=500M` freed 0B (all 4 GiB was active journals). New failure pattern: `git worktree remove --force` can FAIL and leave an orphan when the checkout contains root-owned files (container-run `__pycache__`); the fix is `sudo rm -rf` of the verified-clean leftover, then `git worktree prune`.
- On `mac-demarco-mini`, `sudo -n` requires a password for imalison and root SSH is denied, so privileged cleanup (root `nix-collect-garbage`, other users' caches, simulator runtime deletion) needs an interactive session; unprivileged `nix store gc` freed only 250 MiB. Remaining large admin-only candidates seen 2026-08-05: `AssetsV2` Apple Intelligence models ~21 GiB (system-managed, do not raw-delete), iOS simulator runtimes 16 GiB (18.4 + 26.5), kat's `.yarn` cache 6.3 GiB.
- On macOS, `safe_ncdu scan` currently assumes Linux `findmnt` and cannot make a fresh snapshot. Use `ncdu -0 -x -c -o <snapshot>.json.zst /System/Volumes/Data`, then use `safe_ncdu top` with `zstdcat` available to query it.
- `~/Projects/*/target` directories can dominate home usage. Recent example candidates included stale `target/` directories under `scrobble-scrubber`, `http-client-vcr`, `http-client`, `subtr-actor`, `http-types`, `subtr-actor-py`, `sdk`, and `async-h1`.
- 2026-05-26 cleanup: deleting explicit Cargo-backed targets under `~/Projects/{keepbook,subtr-actor,rlru,rocket-sense,boxcars,rumno}` plus stale `subtr-actor/.worktrees/*/target` reclaimed about 65G by helper sizing and moved `/` from 100% used to 89% used. A final all-depth scan left no `~/Projects` Rust `target/` directories over 500M.
- 2026-05-26 cleanup: when `cargo test` is actively running in `~/Projects/subtr-actor`, leave `subtr-actor/target` alone and delete only inactive Cargo-backed targets. Deleting `keepbook`, `rlru`, `rocket-sense`, `rumno`, and stale `subtr-actor/.worktrees/*/target` reclaimed about 24.5G by helper sizing.
- 2026-05-26 cleanup: `~/Projects/nixpkgs/.worktrees/*/result` symlinks pinned several GiB of Nix closures, and clean registered nixpkgs worktrees were about 460M each. Removing stale `result` symlinks, running GC, and removing clean worktrees while preserving dirty ones moved `/` from 100% used to about 90% used.
- 2026-05-27 cleanup: under `~/Projects`, `hypr-workspace-history/target` can be a Rust-style build cache even though the guarded helper rejects it because no `Cargo.toml` is present; inspect and remove that explicit cache manually if present. Preserve `~/Projects/Hyprland/src/layout/target`, which is source code, not a build artifact.
- 2026-07-18 agent-worktree purge (`railbird/.agents/worktrees`, `railbird-mobile/.worktrees`+`.claude/worktrees`, `subtr-actor/.worktrees`, `rocket-sense/.claude/worktrees`): validated safety workflow for bulk `git worktree remove` across many agent-generated worktrees:
  - **Check for an active session first, before checking git state.** Scan `/proc/*/cwd` for every candidate worktree path; a live Claude/Codex agent process with cwd inside one (e.g. `railbird-mobile/.claude/worktrees/pr-1840-ci-comments-7df0a9` in this run) means skip it outright — removing the worktree out from under a running agent is a correctness hazard regardless of git state.
  - `git worktree remove` (even `--force`) never deletes the branch or its commits, only the linked checkout — so a clean worktree on an *unpushed* named branch is still safe to remove; the commits remain reachable in the main repo's refs afterward. Only **uncommitted working-tree changes** are actually at risk of being lost.
  - For a clean worktree in **detached HEAD**, check `git branch --contains <commit> -a` before removing — if the commit is already an ancestor of `master` or another branch (pushed or local), it's safe; if not, it's only reflog-recoverable and riskier.
  - A single-line `M <submodule-path>` diff (e.g. `M gql`, `M vendor/subtr-actor`) is routine submodule-pointer drift from a stale checkout, not real work — treat it as safe to discard, unlike a real source-file diff.
  - Some worktree directories can be **orphaned**: still present on disk but no longer in `git worktree list --porcelain` (their `.git` gitdir file points at a `.git/worktrees/<id>` entry git has already forgotten, so `git status` fails with `fatal: not a git repository`). These can't be cleaned with `git worktree remove`; `rm -rf` the directory directly, then `git worktree prune`.
  - Result: 4/5 `railbird/.agents/worktrees` removed (28G), 6/7 `railbird-mobile` worktrees removed (skipped the active session and a `/tmp` worktree with unexplained untracked credential-like files: `access_tokens.db`, `credentials.db`, `legacy_credentials/`), 2/3 `subtr-actor/.worktrees` removed, 13/15 `rocket-sense/.claude/worktrees` removed (2 were already-orphaned). Net effect on this run: `/` went from 94% used (54G free) to 82% used (155G free), combined with rancher/gradle/tmp/node_modules cleanup below.
- 2026-07-18 cleanup: `/var/lib/rancher` (4.7G) was orphaned state from a fully uninstalled `rancher-desktop`/`containerd`/`k3s` (all `systemctl is-enabled` returned `not-found`) — safe to `sudo rm -rf` outright, no service to stop first.
- 2026-07-18 cleanup: root `/tmp` (not `$HOME/tmp`) had accumulated 9,450 `nix-shell.*` ephemeral TMPDIR dirs (4.6G, almost all empty) plus ~500 named agent/session scratch dirs (`codex-*`, `t3code-*`, `*-audit`, `*-validation-*`, installer `.dmg`s) totaling ~12G. Before bulk-deleting `/tmp`, cross-check `git worktree list` output for entries rooted in `/tmp` (a `railbird-mobile-rebase` worktree lived at `/tmp/railbird-mobile-rebase` in this run) and check `/proc/*/cwd` / `lsof +D /tmp` for live processes (found an active `rumno` service writing `rumno.pid`/`.out`/`.err`, and 730 small live `com.google.Chrome.*` sandbox dirs from running Chrome/Electron apps) — exclude those from the sweep. Some extracted scratch dirs (from `.asar` unpacking, tarball extraction) have read-only directory bits (`dr-xr-xr-x`, mtime epoch-0) that make plain `rm -rf` fail with `Permission denied`; `chmod -R u+rwX <dir>` before retrying `rm -rf` fixes it.
- 2026-07-20 acute unblock: when manual shard deletion is unavailable, `systemctl --user start cargo-sweep-rust-targets.service` is a sanctioned first move — one run swept project `target/` dirs and freed 43G (`/` 100% -> 95%) in under a minute, without touching the 2-day `~/.cargo/build` window.
- 2026-07-19 cleanup: inactive `/tmp/rmk-*`, `/tmp/glove80-*`, and `/tmp/codex-asar.*` build/extraction directories accumulated about 85G in one day. Before deleting these explicit top-level patterns, scan `/proc/*/{cwd,root,exe,fd/*}` for references and check the candidates for nested `.git` entries; with neither present, removing them reduced `/tmp` from 63G to 4.2G and restored 85G of filesystem headroom.
- 2026-07-22 T3 Code review-worktree unblock: many detached `.worktrees/pr-*` checkouts each materialized about 2.7G of `node_modules` while repeated PR review cycles were running. After checking `/proc/*/cwd` and selecting only completed, inactive worktrees, `git -C <worktree> clean -fdx -- node_modules` on five explicit targets restored about 1.9G of physical space. Preserve source and foreign/active worktrees, and prefer relinking the repository's existing dependencies over creating another independent install.
- 2026-07-24 T3 Code stack cleanup: deleting all 174 `node_modules` directories under `/home/imalison/src/t3code` (including workspace-local link farms) removed about 30.7G of apparent data but reclaimed only 1.7G of filesystem space because the large pnpm trees shared content/hard links. Enumerate exact paths first, verify `/proc/*/{cwd,maps,fd/*}` has no `node_modules` users, and use `git -C <worktree> clean -fdx -- <relative-node_modules-path>` for each validated path. The Nix package/system rebuild remained reproducible without repository-local dependencies.
- 2026-07-29 abandoned-git-tempfile pattern (highest-yield find of that run): `~/Projects/nixpkgs/.git/objects` was 40G against only 6.36 GiB of real packs. `git count-objects -vH` reported `garbage: 3288 / size-garbage: 33.05 GiB` — abandoned `.git/objects/pack/tmp_pack_*` files left behind by interrupted fetches/repacks over months. Always run `git count-objects -vH` on any repo whose `.git` dwarfs its worktree; git names the waste itself. Check `ps` for a live `git` fetch/gc and the newest `tmp_pack_*` mtime before clearing, since an in-flight fetch legitimately owns one. Some `tmp_pack_*` files are mode `-r--------`, so `rm` needs `-f` or a prior `chmod`. A scan of every other repo under `~/Projects` found this pattern essentially unique to nixpkgs (t3code had a single 0.1G stray), so scan broadly but expect one offender. Validated removal: `find .git/objects/pack -maxdepth 1 -name 'tmp_pack_*' -delete` reclaimed **33.8 GiB** in seconds and left the repo intact (`garbage: 0`, all 9,581,748 objects still `in-pack`, `git rev-parse HEAD` and `git status` fine). This is far cheaper than `git gc`, which would repack 9.5M objects to remove data git already classifies as garbage — prefer the targeted delete.
- 2026-07-29 counterexample worth remembering: a large `.git` is not automatically waste. `~/Projects/railbird/.git` is 6.7G but reports `garbage: 0` with 6.42 GiB of legitimate packs across only 54,745 objects — i.e. large binary blobs committed to history, nothing to reclaim. Run `git count-objects -vH` to tell the two cases apart instead of assuming; the `in-pack` vs `size-pack` ratio is the tell (tiny object count + huge pack = real blobs, not garbage).
- 2026-07-29 `/tmp` caveat that overrides the "ephemeral scratch" assumption: descriptively-named `/tmp` dirs can be full git checkouts holding the only copy of real work. `t3-integ`, `t3-integ2`, and `t3-4506-compat` each contained a nested `.git` with **staged and untracked source changes and no remote at all** (`git remote get-url origin` empty), so deletion would have been unrecoverable. Always run `find <dir> -maxdepth 3 -name .git` on each `/tmp` candidate and then `git status --porcelain` plus `git remote get-url origin` on every hit — the registered-worktree scan (`git worktree list --porcelain | grep ^worktree /tmp`) does NOT catch these, because standalone clones are not worktrees of anything.
- 2026-07-29 validated safe `/tmp` subset on this machine: `metro-cache`, `node-compile-cache`, a stray `tmp.*/app-release.apk`, and 4078 `nix-shell.*` dirs (no `.git` in any) together reclaimed 1.5 GiB. The `nix-shell.*` dirs are individually tiny but a handful reach ~100M each.
- 2026-07-29 hard-link accounting reminder: `safe_ncdu top` reported railbird-mobile at 49.3 GiB and t3code at 38.4 GiB where `du -x` said 27G and 15G. The gap is pnpm/node_modules hard links. Trust `du -x` (or measured `df` deltas) for *reclaimable* space and treat ncdu's apparent sizes as an upper bound only.
- 2026-07-29 agent-worktree accumulation recheck: `.worktrees` dominated every large JS/Python repo — railbird-mobile 22G/5, t3code 14G/41, org-agenda-api 5.8G/1, railbird 2.3G/26. Also on this machine: `railbird/.venv` 7.1G, `railbird/.git` 6.7G, `influence-sensitivity/.lake` 6.9G + `.elan` 2.6G (Lean toolchain/build, regenerable). Preserve `subtr-actor-mechanics/ml` (9.3G) and `data` (1.2G) — those are ML datasets/checkpoints, not build output.
- 2026-07-29 worktrees live OUTSIDE the repo more often than expected, and `du` on the repo will not find them. Always cross-check `git worktree list --porcelain` against on-disk `.worktrees/`, because the registry is the only thing that reveals external checkouts. Found this run: 8 registered t3code worktrees under `~/.t3/worktrees/t3code/` (5.4G, the T3 Code desktop app's own state dir, 11 dirs on disk so 3 orphans), and two registered railbird worktrees at sibling paths `~/Projects/railbird-agents-architecture` and `~/Projects/railbird-alert-tuning`. Assign these to the agent that already owns the parent repo — never a second writer, or the two race on `.git/worktrees` metadata.
- 2026-07-29 `railbird/.claude` is a symlink to `.agents`, so `.claude/worktrees/` and `.agents/worktrees/` are the SAME directory (same inode). Check with `ls -ld` before reporting them as two separate cleanup targets or double-counting their size.
- 2026-07-29 detached-HEAD reachability: `git branch --contains -a` is NOT sufficient — it misses tags. Use `git for-each-ref --contains <commit>` as well. In t3code this distinguished `thread-picker-build` (reachable only via `refs/tags/t3code-thread-picker/*`, so safe to remove) from six worktrees whose commits no ref contained at all (reflog-only — correctly skipped).
- 2026-07-29 orphan verification without git: when an orphan's gitdir pointer targets a parent repo that no longer exists at all (here `~/dotfiles/dotfiles/emacs.d/straight/repos/org-agenda-api/.git`, a deleted straight.el package clone), `git status` cannot help. The validated technique is to content-hash every file in the orphan and check blob presence in a surviving object store: 54/54, 34/35, and 29/29 matched, with the only misses being generated artifacts (`.elc` bytecode whose `.el` source was present, and a gitignored `expo-env.d.ts`). For a nested submodule whose blobs correctly do NOT live in the superproject, trace `.gitmodules` to the canonical repo and verify the pinned gitlink commit exists and is reachable there. This is much stronger evidence than an mtime heuristic.
- 2026-07-29 `git worktree remove` can fail with `fatal: working trees containing submodules cannot be moved or removed` even on a completely clean worktree. `--force` is correct there — it is overriding a submodule-mechanics objection, not discarding uncommitted work. Do not read that particular `--force` as risky, but do confirm `git status --porcelain` was empty first, and verify branches still resolve afterward.
- 2026-07-29 measured yields, one agent per repo (df deltas are cross-contaminated when agents run concurrently — always have each report an attributable `du -sk` taken immediately BEFORE deleting): t3code 26 of 33 worktrees removed (7 skipped: 1 real diff, 6 reflog-only detached), railbird-mobile 5 of 5, railbird 22 of 24 (2 skipped for genuine uncommitted source work), org-agenda-api 4 of 4 (5.73 GiB attributable, of which 100% was `mova/node_modules` 3.9G + `mova/android` 1.9G). Expect roughly 1 in 8 worktrees to hold real unpushed work — the skip rate is not zero, so never bulk-remove without the per-worktree status check.
- 2026-07-29 `~/.t3` (T3 Code desktop app) is 9.0G: `worktrees/` 5.4G and `userdata/logs/` 2.8G, of which `logs/provider/` is 2.6G across 332 per-session `<uuid>.log` files. Only ~45 files (377M) were older than 7 days. The app rotates `desktop.trace.ndjson.N` / `server.trace.ndjson.N` / `server-child.log.N` at 10M each. Check `ps` for a running T3 Code node process and scan `/proc/*/fd/*` before touching any of it; leave `userdata/secrets`, `attachments`, and `chats` alone entirely.

## Step 5: `/nix/store` Deep Dive

When `/nix/store` is still large after GC, inspect root causes instead of deleting random paths.

Useful commands:

```bash
nix path-info -Sh /nix/store/* 2>/dev/null | sort -h | tail -n 50
nix-store --gc --print-roots
```

Avoid `du -sh /nix/store` as a first diagnostic; it can be very slow on large stores.

For repeated GHC/Rust toolchain copies:

```bash
nix path-info -Sh /nix/store/* 2>/dev/null | rg '(ghc|rustc|rust-std|cargo)'
nix-store --gc --print-roots | rg '(ghc|rust)'
```

Resolve why a path is retained:

```bash
/srv/dotfiles/dotfiles/lib/functions/find_store_path_gc_roots /nix/store/<store-path>
nix why-depends <consumer-store-path> <dependency-store-path>
```

Common retention pattern on this machine:

- Many `.direnv/flake-profile-*` symlinks under `~/Projects` and worktrees keep `nix-shell-env`/`ghc-shell-*` roots alive.
- On the macOS host, `.direnv` symlinks on mounted external volumes still register roots for the internal `/nix/store`. A 2026-07-13 audit found seven direnv profiles across the active home and `/Volumes/Extreme SSD` retaining 17.3 GiB of store paths unique to direnv roots; clearing the external direnvs reclaims internal Nix space, not meaningful space on the external volume.
- 2026-07-13 macOS direnv cleanup: preserving only the main `~/railbird-mobile/.direnv` while removing six other profiles plus `scrobble-scrubber` result roots made 6,262 paths dead; `nix-collect-garbage -d` reported 8.3 GiB actually freed versus a 12.9 GiB preflight `narSize` estimate.
- Old taffybar constellation repos under `~/Projects` can pin large Haskell closures through `.direnv` and `result` symlinks. Deleting `gtk-sni-tray`, `status-notifier-item`, `dbus-menu`, `dbus-hslogger`, and `gtk-strut` and then rerunning `nix-collect-garbage -d` reclaimed about 11G of store data in one validated run.
- `find_store_path_gc_roots` is especially useful for proving GHC retention: many large `ghc-9.10.3-with-packages` paths are unique per project, while the base `ghc-9.10.3` and docs paths are shared.
- NixOS system generations and a repo-root `nixos/result` symlink can pin multiple Android Studio and Android SDK versions. Check `/nix/var/nix/profiles/system-*-link`, `/run/current-system`, `/run/booted-system`, and `/srv/dotfiles/nixos/result` before assuming Android paths are pinned by project shells.
- `~/Projects/railbird-mobile/.direnv/flake-profile-*` can pin large Android SDK system images. Removing stale direnv profiles there is a more targeted first step than deleting Android store paths directly.
- 2026-05-27 Railbird GHC audit: the Railbird backend flake did not explicitly reference Haskell, but its dev shell had derivation-time GHC edges through `inputs.secrets.devShells.${system}.default -> agenix -> shellcheck -> ShellCheck -> ghc` and through `shell-packages.nix`'s `rdma-core -> pandoc-cli -> ghc`. Railbird Mobile had similar non-app-code GHC edges through `inputs.secrets`/`agenix` and `nixGLIntel -> shellcheck`. The `railbird/gql` and `railbird-mobile/src/gql` shells did not show GHC edges in their derivation graphs, only Rust/Cargo build tooling from packages such as `just`.
- For a repeatable `/nix/store` `ncdu` snapshot without driving the TUI, use `safe_ncdu` rather than a one-off `/tmp` export, so the snapshot lands at the predictable `/srv/disk-space-assessments/latest-nix_store.json.zst` path instead of a location later sessions won't know to check:

```bash
safe_ncdu /nix/store
safe_ncdu top /srv/disk-space-assessments/latest-nix_store.json.zst 30
```

- `nix-store --gc --print-dead` plus the Nix SQLite database is a fast way to estimate immediate GC wins before deleting anything:

```bash
nix-store --gc --print-dead > /tmp/nix-dead-paths.txt
printf '%s\n' '.mode list' '.separator |' 'create temp table dead(path text);' \
  '.import /tmp/nix-dead-paths.txt dead' \
  'select count(*), sum(narSize) from ValidPaths join dead using(path);' \
  | nix shell nixpkgs#sqlite --command sqlite3 /nix/var/nix/db/db.sqlite
```

- Quantify before acting:

```bash
find ~/Projects -type l -path '*/.direnv/flake-profile-*' | wc -l
find ~/Projects -type d -name .direnv | wc -l
nix-store --gc --print-roots | rg '/\\.direnv/flake-profile-' | awk -F' -> ' '{print $1 \"|\" $2}' \
  | while IFS='|' read -r root target; do \
      nix-store -qR \"$target\" | rg '^/nix/store/.+-ghc-[0-9]'; \
    done | sort | uniq -c | sort -nr | head
```

- If counts are high and the projects are inactive, propose targeted `.direnv` cleanup for user confirmation.
- 2026-08-14 `jimi-hendnix` (multi-user railbird box) — single largest validated win in this skill's history: **251.8 GiB freed by one `nix-collect-garbage -d`**, moving `/` from 90% used (94G free) to 44% (499G free). Three cleanup classes fed that single GC, and none of them freed meaningful space until the GC ran:
  - **Dead nixtheplanet macOS VM, ~167 GiB.** Three plain symlinks (`/home/imalison/macos-ventura-base-image.qcow2`, `/home/kat/macos-ventura-base-image.qcow2`, `/var/lib/private/nixtheplanet-macos-ventura/macos-ventura-base-image.qcow2`) each pinned a ~40 GiB `mac_hdd_ng.qcow2`. Because this host sets `keep-derivations = true` and `keep-outputs = true`, they also transitively pinned four `InstallAssistant.iso`/`.pkg` build inputs (45 GiB) and `BaseSystem.img` (3 GiB). Those build inputs have **zero referrers** — `nix-store --query --referrers-closure` returns only the path itself — so only `nix-store --query --roots <path>` reveals the retention. Use `--query --roots`, not referrers, whenever a huge store path looks unrooted but is not in `--print-dead`. `services.macos-ventura.enable` was already `false` and the 19 GiB runtime disk was last touched 2024-11-01.
  - **13 stale `.direnv` dirs across six users' homes, ~56 GiB collectively direnv-only**, all 600–800 days old. On disk they were only 76–248 KiB each — the size is entirely what they pin. The four per-user `railbird` checkouts each had a ~14.8 GiB closure but ~3 MiB marginal-unique, so they only paid off removed together.
  - **`/var/lib/private/gitea-runner`, 32 GiB**, removed outright along with the service (see below).
- 2026-08-14 `nix-store --gc --print-dead` returned **zero** dead paths on the first run because a concurrent nix process held ~15k temp GC roots (`nix-store --gc --print-roots` was dominated by `{temp:NNNN}` entries). After that process died and its `temproots` file went stale, the same command found 345 dead paths. If `--print-dead` says zero on a store you have reason to believe is dirty, check the roots listing for `{temp:...}` domination and re-run.

## Removing a Service Entirely (not just its caches)

Validated 2026-08-14 on `jimi-hendnix` for gitea-runner. When the user wants a service gone rather than trimmed, disable it in Nix and rebuild *before* deleting state, so nothing recreates the directory:

1. Confirm no job is mid-flight: `systemctl status <unit>` should show the bare daemon with no child job processes, and `pgrep -a -u <service-user>` should list only the daemon.
2. Flip the flag in the host config (`nixos/machines/<host>.nix`) to `false` rather than deleting the line — this host already expresses `services.macos-ventura.enable = false` and `myModules.railbird-k3s.enable = false` the same way.
3. `just switch` from `/srv/dotfiles/nixos`. A successful run prints `removing user 'gitea-runner'`.
4. Only then `rm -rf` the state tree.
5. Note the server-side consequence: the runner's registration (`.runner` token) is destroyed, so the runner will show as offline in the Gitea instance and must be re-registered if ever reinstated.

### `just switch` gotchas hit during that run

- `safe_switch` retains a completed tmux pane for inspection, but the next invocation removes that completed session before starting a new switch. If a switch seems stale, check `tmux -L nixos-switch list-panes -t switch -F '#{pane_pid} #{pane_dead}'`; `pane_dead=1` means the old run finished and should be replaced automatically. Always verify with `readlink /run/current-system` plus a `systemctl is-enabled` check rather than trusting log output alone.
- `/boot` on this host is deliberately kept `ro` by another workflow (the journal shows a `mount -o remount,rw /boot` immediately followed by `remount,ro`). `switch-to-configuration` then fails with `OSError: [Errno 30] Read-only file system: '/boot/loader/entries/...'` and `Failed to install bootloader` **after** the config has already built. Before assuming corruption, check `dmesg` for `fat-fs`/device errors against the actual `/boot` device — in this run the I/O errors were all on `sdd`/`sdc3` (removable rescue media), and boot-time `systemd-fsck` reported `/dev/nvme1n1p1` clean. Remount rw, re-run the switch, then restore `ro` to leave the machine as found.

## Safety Rules

- Do not delete user files directly unless explicitly requested.
- Prefer cleanup tools that understand ownership/metadata (`nix`, `docker`, `podman`, `cargo-sweep`) over `rm -rf`.
- For Rust build artifacts, deleting an explicit directory literally named `target` is acceptable when it is discovered by the bundled helper; Cargo will rebuild it.
- Present a concise “proposed actions” list before high-impact deletes.
- If uncertain whether data is needed, stop at investigation and ask.

## Learning Loop (Required)

Treat this skill as a living playbook.

After each disk cleanup task:

1. Add newly discovered mountpoints or directories to ignore in `references/ignore-paths.md`.
2. Add newly discovered Rust repo roots in `references/rust-target-roots.txt`.
3. Add validated command patterns or caveats discovered during the run to this `SKILL.md`.
4. Keep instructions practical and machine-specific; remove stale guidance.

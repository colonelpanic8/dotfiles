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
- For Rust build artifacts, do not repeatedly ask for confirmation before deleting explicit directories literally named `target` after `rust_target_dirs.py delete` validates them. Cargo targets are rebuildable artifacts; when the user asks to clean Rust target directories, validate with the helper, delete with `--yes`, and report the reclaimed space.
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
- `nixos/imalison.nix` defines a daily user timer, `cargo-sweep-rust-targets.timer`, that runs `cargo-sweep sweep -r --hidden --maxsize 15GB` across `/home/imalison/Projects`, `/home/imalison/org`, and `/srv/dotfiles`.

## Step 4: Investigation with `ncdu` and `du`

Avoid mounted or remote filesystems when profiling space. Load ignore patterns from `references/ignore-paths.md`.

Use one-filesystem scans to avoid crossing mounts:

```bash
ncdu -x "$HOME"
sudo ncdu -x /
```

When excluding known noisy mountpoints:

```bash
ncdu -x --exclude "$HOME/keybase" "$HOME"
sudo ncdu -x --exclude /keybase --exclude /var/lib/railbird /
```

If `ncdu` is missing, use:

```bash
nix run nixpkgs#ncdu -- -x "$HOME"
```

For reusable, mount-safe snapshots on this machine, prefer the local wrapper:

```bash
safe_ncdu /
sudo -n env HOME=/home/imalison safe_ncdu /
safe_ncdu /nix/store
safe_ncdu top ~/.cache/ncdu/latest-root.json.zst 30 /home/imalison
safe_ncdu open ~/.cache/ncdu/latest-root.json.zst
```

`safe_ncdu` writes compressed ncdu exports under `~/.cache/ncdu`, records the exclude list beside the export, excludes mounted descendants of the scan root, and supports follow-up `top` queries without rescanning.

For quick, non-blocking triage on very large trees, prefer bounded probes:

```bash
timeout 30s du -xh --max-depth=1 "$HOME/.cache" 2>/dev/null | sort -h
timeout 30s du -xh --max-depth=1 "$HOME/.local/share" 2>/dev/null | sort -h
```

Machine-specific heavy hitters seen in practice:

- `~/.cache/uv` can exceed 20G and is reclaimable with `uv cache clean`.
- `~/.cache/pypoetry` can exceed 7G across artifacts, repository cache, and virtualenvs; inspect first, then use Poetry cache commands or targeted virtualenv removal.
- `~/.cache/google-chrome` can exceed 8G across multiple Chrome profiles; close Chrome before clearing profile cache directories.
- `~/.cache/spotify` can exceed 10G; treat as optional app-cache cleanup.
- `~/.gradle` can exceed 8G, mostly under `caches/`; prefer Gradle-aware cleanup and expect dependency redownloads.
- `~/.local/share/picom/debug.log` can grow past 15G when verbose picom debugging is enabled or crashes leave a stale log behind; if `picom` is not running, deleting or truncating the log is a high-yield low-risk win.
- `~/.local/share/Trash` can exceed several GB; empty only with user approval.
- `/var/lib/private/gitea-runner` can exceed 50G and is not visible to an unprivileged `ncdu /` scan; use `sudo -n env HOME=/home/imalison safe_ncdu /` when `/var` looks undercounted.
  - Validated cleanup pattern: stop `gitea-runner-nix.service`, remove cache/work directories under `/var/lib/private/gitea-runner` (`.cache`, `.gradle`, `action-cache-dir`, `workspace`, stale nested `gitea-runner`, and nested `nix/.cache`/`nix/.local`), recreate `action-cache-dir`, `workspace`, and `.cache` owned by `gitea-runner:gitea-runner`, then restart the service.
  - Preserve registration/config-like files such as `/var/lib/private/gitea-runner/nix/.runner`, `/var/lib/private/gitea-runner/nix/.labels`, `/var/lib/private/gitea-runner/.docker/config.json`, and SSH/Kube material.
- `~/Projects/*/target` directories can dominate home usage. Recent example candidates included stale `target/` directories under `scrobble-scrubber`, `http-client-vcr`, `http-client`, `subtr-actor`, `http-types`, `subtr-actor-py`, `sdk`, and `async-h1`.
- 2026-05-26 cleanup: deleting explicit Cargo-backed targets under `~/Projects/{keepbook,subtr-actor,rlru,rocket-sense,boxcars,rumno}` plus stale `subtr-actor/.worktrees/*/target` reclaimed about 65G by helper sizing and moved `/` from 100% used to 89% used. A final all-depth scan left no `~/Projects` Rust `target/` directories over 500M.
- 2026-05-26 cleanup: when `cargo test` is actively running in `~/Projects/subtr-actor`, leave `subtr-actor/target` alone and delete only inactive Cargo-backed targets. Deleting `keepbook`, `rlru`, `rocket-sense`, `rumno`, and stale `subtr-actor/.worktrees/*/target` reclaimed about 24.5G by helper sizing.
- 2026-05-26 cleanup: `~/Projects/nixpkgs/.worktrees/*/result` symlinks pinned several GiB of Nix closures, and clean registered nixpkgs worktrees were about 460M each. Removing stale `result` symlinks, running GC, and removing clean worktrees while preserving dirty ones moved `/` from 100% used to about 90% used.
- 2026-05-27 cleanup: under `~/Projects`, `hypr-workspace-history/target` can be a Rust-style build cache even though the guarded helper rejects it because no `Cargo.toml` is present; inspect and remove that explicit cache manually if present. Preserve `~/Projects/Hyprland/src/layout/target`, which is source code, not a build artifact.
- 2026-06-18 cleanup: deleting helper-validated Rust targets under `.worktrees/*/target` and `.claude/worktrees/*/target`, plus stale `~/Projects/lastfm-edit/target`, removed 24 target directories totaling 67.1G by helper sizing and moved `/` from 99% used to 90% used. Remaining large targets were top-level project caches under `keepbook`, `rlru`, `subtr-actor`, `rocket-sense`, `rocket-sense-pr-73-ci`, `rocket-sense-subtr-viewer`, `rocket-sense-controlled-plays`, and `boxcars`.
- 2026-06-23 cleanup: deleting helper-validated Rust targets over 100M under `~/Projects` removed 21 target directories totaling 59.6G by helper sizing; `rocket-sense/.worktrees/missed-event-capture/target` was recreated once during verification and deleted again after no active Cargo/Rust process was found. Clearing rebuildable Cargo registry/git caches and removing stale `~/.rustup/toolchains/1.68.2-x86_64-unknown-linux-gnu` moved `/` from 99% used with 17G free to 91% used with 81G free. In this shell, `python`, `python3`, `cargo`, `rustup`, `ps`, and `cargo-sweep` were not on `PATH`; using `nix run nixpkgs#python3 -- ...` worked for the helper, and `nix shell nixpkgs#procps nixpkgs#ripgrep --command bash -lc 'ps ...'` worked for process checks.
- 2026-06-28 cleanup: deleting helper-validated Rust targets over 500M under `~/Projects` removed 25 target directories totaling 102.6G by helper sizing and moved `/` from 98% used with 23G free to 86% used with 126G free. The largest wins were top-level `rlru`, `subtr-actor`, `rocket-sense`, `keepbook`, and `boxcars` targets plus many `rocket-sense` and `subtr-actor` `.worktrees/*/target` and `.claude/worktrees/*/target` directories. A follow-up scan over 100M found no remaining helper-validated Rust targets.
- 2026-07-03 cleanup (from within a sandboxed agent shell): `sudo -n` was unavailable ("no new privileges" flag), so `sudo nix-collect-garbage -d`, `docker`/`podman` prune were all out. Sudo-free wins still worked: plain `nix-collect-garbage -d` talked to the daemon and freed 17.0 GiB (2777 store paths), and helper-validated Rust target deletions under `~/Projects` (`keepbook`, `rlru`, and several `rocket-sense/.claude/worktrees/*/target` + `.worktrees/*/target`) reclaimed ~27G, moving `/home` from 100% used with 86M free to 95% with 47G free. Left `~/Projects/lastfm-edit/target` (17.4G) intact deliberately: it was in active use by the running session/build and a concurrent agent, and enough space had already been freed. Lesson: when only ~80M is free, disk-write failures surface as nondeterministic Rust build errors ("could not compile … due to 1 previous error" with no diagnostic text, `disk I/O error` on `~/.cache/nix/fetcher-cache-v4.sqlite`, and openssl-sys build-script failures) rather than obvious "No space left" messages.

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
- Old taffybar constellation repos under `~/Projects` can pin large Haskell closures through `.direnv` and `result` symlinks. Deleting `gtk-sni-tray`, `status-notifier-item`, `dbus-menu`, `dbus-hslogger`, and `gtk-strut` and then rerunning `nix-collect-garbage -d` reclaimed about 11G of store data in one validated run.
- `find_store_path_gc_roots` is especially useful for proving GHC retention: many large `ghc-9.10.3-with-packages` paths are unique per project, while the base `ghc-9.10.3` and docs paths are shared.
- NixOS system generations and a repo-root `nixos/result` symlink can pin multiple Android Studio and Android SDK versions. Check `/nix/var/nix/profiles/system-*-link`, `/run/current-system`, `/run/booted-system`, and `/srv/dotfiles/nixos/result` before assuming Android paths are pinned by project shells.
- `~/Projects/railbird-mobile/.direnv/flake-profile-*` can pin large Android SDK system images. Removing stale direnv profiles there is a more targeted first step than deleting Android store paths directly.
- 2026-05-27 Railbird GHC audit: the Railbird backend flake did not explicitly reference Haskell, but its dev shell had derivation-time GHC edges through `inputs.secrets.devShells.${system}.default -> agenix -> shellcheck -> ShellCheck -> ghc` and through `shell-packages.nix`'s `rdma-core -> pandoc-cli -> ghc`. Railbird Mobile had similar non-app-code GHC edges through `inputs.secrets`/`agenix` and `nixGLIntel -> shellcheck`. The `railbird/gql` and `railbird-mobile/src/gql` shells did not show GHC edges in their derivation graphs, only Rust/Cargo build tooling from packages such as `just`.
- For a repeatable `/nix/store` `ncdu` snapshot without driving the TUI, export and inspect it:

```bash
ncdu -0 -x -c -o /tmp/nix-store.ncdu.json.zst /nix/store
zstdcat /tmp/nix-store.ncdu.json.zst | jq 'def sumd: if type=="array" then ((.[0].dsize // 0) + ([.[1:][] | sumd] | add // 0)) elif type=="object" then (.dsize // 0) else 0 end; .[3] | sumd'
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

## Safety Rules

- Do not delete user files directly unless explicitly requested.
- Prefer cleanup tools that understand ownership/metadata (`nix`, `docker`, `podman`, `cargo-sweep`) over `rm -rf`.
- For Rust build artifacts, deleting an explicit directory literally named `target` is acceptable when it is discovered and validated by the bundled helper; Cargo will rebuild it. Do not double-check with the user after helper validation when the active request is Rust target cleanup.
- Present a concise “proposed actions” list before high-impact deletes.
- If uncertain whether data is needed, stop at investigation and ask.

## Learning Loop (Required)

Treat this skill as a living playbook.

After each disk cleanup task:

1. Add newly discovered mountpoints or directories to ignore in `references/ignore-paths.md`.
2. Add newly discovered Rust repo roots in `references/rust-target-roots.txt`.
3. Add validated command patterns or caveats discovered during the run to this `SKILL.md`.
4. Keep instructions practical and machine-specific; remove stale guidance.

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
python /home/imalison/dotfiles/dotfiles/agents/skills/disk-space-cleanup/scripts/rust_target_dirs.py list --min-size 500M --limit 30
```

Focus on stale targets only:

```bash
python /home/imalison/dotfiles/dotfiles/agents/skills/disk-space-cleanup/scripts/rust_target_dirs.py list --min-size 1G --older-than 14 --output tsv
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
python /home/imalison/dotfiles/dotfiles/agents/skills/disk-space-cleanup/scripts/rust_target_dirs.py delete /abs/path/to/target
python /home/imalison/dotfiles/dotfiles/agents/skills/disk-space-cleanup/scripts/rust_target_dirs.py delete /abs/path/to/target --yes
```

Recommended sequence:

1. Run `rust_target_dirs.py list` to see the largest `target/` directories across `~/Projects`, `~/org`, `~/dotfiles`, and other configured roots.
2. For active repos, prefer `cargo-sweep` from the workspace root.
3. For inactive repos, abandoned branches, and `.worktrees/*/target`, prefer guarded direct deletion of the explicit `target/` directory.
4. Re-run the list command after each deletion round to show reclaimed space.

Machine-specific note:

- Project-local `.worktrees/*/target` directories are common cleanup wins on this machine and are easy to miss with the old hard-coded workflow.

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
/home/imalison/dotfiles/dotfiles/lib/functions/find_store_path_gc_roots /nix/store/<store-path>
nix why-depends <consumer-store-path> <dependency-store-path>
```

Common retention pattern on this machine:

- Many `.direnv/flake-profile-*` symlinks under `~/Projects` and worktrees keep `nix-shell-env`/`ghc-shell-*` roots alive.
- Old taffybar constellation repos under `~/Projects` can pin large Haskell closures through `.direnv` and `result` symlinks. Deleting `gtk-sni-tray`, `status-notifier-item`, `dbus-menu`, `dbus-hslogger`, and `gtk-strut` and then rerunning `nix-collect-garbage -d` reclaimed about 11G of store data in one validated run.
- `find_store_path_gc_roots` is especially useful for proving GHC retention: many large `ghc-9.10.3-with-packages` paths are unique per project, while the base `ghc-9.10.3` and docs paths are shared.
- NixOS system generations and a repo-root `nixos/result` symlink can pin multiple Android Studio and Android SDK versions. Check `/nix/var/nix/profiles/system-*-link`, `/run/current-system`, `/run/booted-system`, and `~/dotfiles/nixos/result` before assuming Android paths are pinned by project shells.
- `~/Projects/railbird-mobile/.direnv/flake-profile-*` can pin large Android SDK system images. Removing stale direnv profiles there is a more targeted first step than deleting Android store paths directly.
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

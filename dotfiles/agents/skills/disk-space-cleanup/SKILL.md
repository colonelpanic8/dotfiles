---
name: disk-space-cleanup
description: Investigate and safely reclaim disk space on this machine, especially on NixOS systems with heavy Nix, Rust/Haskell, Docker, and Podman usage. Use when disk is low, builds fail with no-space errors, /nix/store appears unexpectedly large, or the user asks for easy cleanup wins without deleting important data.
---

# Disk Space Cleanup

Reclaim disk space with a safety-first workflow: investigate first, run obvious low-risk cleanup wins, then do targeted analysis for larger opportunities.

## Execution Default

- Start with non-destructive investigation and quick sizing.
- Prioritize easy wins first (`nix-collect-garbage`, container prune, Cargo artifacts).
- Propose destructive actions with expected impact before running them.
- Run destructive actions only after confirmation, unless the user explicitly requests immediate execution of obvious wins.
- Capture new reusable findings by updating this skill before finishing.

## Workflow

1. Establish current pressure and biggest filesystems
2. Run easy cleanup wins
3. Sweep Rust build artifacts in common project roots
4. Investigate remaining heavy directories with `ncdu`/`du`
5. Investigate `/nix/store` roots when large toolchains still persist
6. Summarize reclaimed space and next candidate actions
7. Record new machine-specific ignore paths or cleanup patterns in this skill

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

## Step 3: Rust Build Artifact Cleanup

Target common roots first: `~/Projects` and `~/code`.

Use `cargo-sweep` in dry-run mode before deleting:

```bash
nix run nixpkgs#cargo-sweep -- sweep -d -r -t 30 ~/Projects ~/code
```

Then perform deletion:

```bash
nix run nixpkgs#cargo-sweep -- sweep -r -t 30 ~/Projects ~/code
```

Alternative for toolchain churn cleanup:

```bash
nix run nixpkgs#cargo-sweep -- sweep -r -i ~/Projects ~/code
```

Recommended sequence:

1. Run `-t 30` first for age-based stale builds.
2. Run a dry-run with `-i` next.
3. Apply `-i` when dry-run shows significant reclaimable space.

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
- Quantify before acting:

```bash
find ~/Projects -type l -path '*/.direnv/flake-profile-*' | wc -l
find ~/Projects -type d -name .direnv | wc -l
```

- If counts are high and the projects are inactive, propose targeted `.direnv` cleanup for user confirmation.

## Safety Rules

- Do not delete user files directly unless explicitly requested.
- Prefer cleanup tools that understand ownership/metadata (`nix`, `docker`, `podman`, `cargo-sweep`) over `rm -rf`.
- Present a concise “proposed actions” list before high-impact deletes.
- If uncertain whether data is needed, stop at investigation and ask.

## Learning Loop (Required)

Treat this skill as a living playbook.

After each disk cleanup task:

1. Add newly discovered mountpoints or directories to ignore in `references/ignore-paths.md`.
2. Add validated command patterns or caveats discovered during the run to this `SKILL.md`.
3. Keep instructions practical and machine-specific; remove stale guidance.

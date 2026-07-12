---
name: disk-space-cleanup
description: Safely reclaim disk space through reviewed, automated cleanup actions on this machine, especially for Nix, Rust build artifacts, containers, caches, and service state. Use when an assessment has identified cleanup candidates, the user asks to free space or automate remediation, or low-space recovery requires measured deletion of rebuildable data.
---

# Disk Space Cleanup

Act on evidence from `disk-space-assessment`. Automate guarded remediation, measure each action, and preserve user data and active work.

Bundled resources:

- `scripts/rust_target_dirs.py`: inventory and guarded deletion of explicit Cargo `target/` directories
- `references/rust-target-roots.txt`: machine-specific Rust scan roots
- `references/cleanup-patterns.md`: validated machine-specific remediation patterns

## Required Input and Handoff

Prefer a recent assessment containing baseline `df` output, reusable `ncdu` artifact paths, ranked candidates, and active-process caveats. If no reusable snapshot exists, use `disk-space-assessment` first. Even in an emergency, create a timestamped `safe_ncdu` snapshot of the affected filesystem or focused heavy root before deleting data whenever the scan can complete safely.

Record before/after free space for every cleanup round. After material cleanup, create a new reusable snapshot through `disk-space-assessment` so the result can be compared without losing the pre-cleanup evidence.

## Execution Policy

- Present proposed actions with expected impact and risk before high-impact deletion.
- Run destructive actions only after confirmation unless the user explicitly requests immediate cleanup of obvious rebuildable artifacts.
- Prefer ownership-aware cleanup commands over raw recursive deletion.
- Check for active builds, services, mounts, and open files before deleting their state.
- Execute one category at a time and remeasure; stop when the user's space target is met.
- Never treat overlapping Nix closure sizes as additive reclaim estimates.

For Rust artifacts, helper-validated explicit directories literally named `target` are rebuildable. When the user asks to clean Rust targets, inventory them, validate with the helper, delete with `--yes`, and report reclaimed space without repeatedly asking for confirmation.

## Workflow

1. Read the assessment and record current free space.
2. Rank candidates by reclaim, reversibility, user impact, and confidence.
3. Check active use and run dry-run modes where available.
4. Execute the smallest sufficient cleanup round.
5. Remeasure free space and verify affected services or builds.
6. Produce a post-cleanup reusable snapshot after material changes.
7. Report actions, actual reclaim, residual candidates, and artifact paths.

## Baseline and Activity Checks

```bash
df -h /
df -h /home
df -h /nix
ps aux | rg '(cargo|rustc|nix|docker|podman|gitea-runner)' || true
```

Use `lsof`, service status, repository state, and worktree age when a candidate could be active. A directory that looks stale by mtime may still belong to a running build or concurrent agent.

## Low-Risk Automated Wins

Use dry-run or non-interactive privilege checks first:

```bash
nix-collect-garbage -d
sudo -n nix-collect-garbage -d
sudo -n docker system prune -a
sudo -n podman system prune -a
```

Notes:

- Plain `nix-collect-garbage -d` may work through the daemon when `sudo` is unavailable.
- Add container `--volumes` only with explicit approval to delete unused volumes.
- Recheck `df` after each command.
- Fail fast on unavailable privilege; do not hang on a password prompt.

Use application-aware cache cleaners when the assessment identifies those caches:

```bash
uv cache clean
pip cache purge
yarn cache clean
npm cache clean --force
```

Expect dependency redownloads. Close applications before clearing their profile caches.

## Rust Build Artifacts

Inventory across configured project and worktree roots:

```bash
python /srv/dotfiles/dotfiles/agents/skills/disk-space-cleanup/scripts/rust_target_dirs.py list --min-size 500M --limit 30
python /srv/dotfiles/dotfiles/agents/skills/disk-space-cleanup/scripts/rust_target_dirs.py list --min-size 1G --older-than 14 --output tsv
```

For active workspaces, prefer age/toolchain-aware cleanup:

```bash
nix run nixpkgs#cargo-sweep -- sweep -d -r -t 30 <workspace-root>
nix run nixpkgs#cargo-sweep -- sweep -r -t 30 <workspace-root>
```

For inactive repos and stale worktrees, use guarded direct deletion:

```bash
python /srv/dotfiles/dotfiles/agents/skills/disk-space-cleanup/scripts/rust_target_dirs.py delete /abs/path/to/target
python /srv/dotfiles/dotfiles/agents/skills/disk-space-cleanup/scripts/rust_target_dirs.py delete /abs/path/to/target --yes
```

The helper rejects symlinks, paths outside configured roots, non-`target` names, and directories without a Cargo project above them. Re-run inventory after each round. Preserve any target involved in an active Cargo/Rust process.

If `python` is unavailable, run the helper with `nix run nixpkgs#python3 -- ...`. If a stale `~/.cargo/bin/cargo-sweep` shadows the NixOS binary, inspect `type -a cargo-sweep` before use.

## Nix Remediation

Do not delete `/nix/store` paths directly. Act on retaining roots identified by the assessment:

- Remove stale project `.direnv` directories or `flake-profile-*` roots only after verifying the project/worktree is inactive.
- Remove stale `result*` symlinks when the referenced build output no longer needs to remain rooted.
- Remove obsolete system generations through Nix tooling.
- Run garbage collection after roots are removed and measure actual free-space change.
- Run `nix-store --optimise` when duplicate physical files are a demonstrated opportunity; it can be I/O intensive.

Use `nix_store_audit --top 30` before and after a root-removal campaign. Treat its uniquely collectible estimates as planning evidence, not a promise of exact filesystem reclaim.

## Service and Cache Remediation

Read `references/cleanup-patterns.md` before touching private service state, runner caches, logs, or ambiguous project artifacts. Preserve configuration, registration, credentials, SSH material, and active workspaces.

Prefer service-aware sequences: stop the service, clean only validated cache/work paths, recreate required directories with correct ownership, restart, and verify health.

- 2026-07-10 `railbird-sf` incident: K3s reported `DiskPressure` even with tens of GiB free because its container `imagefs` shares `/`, kubelet image GC used the default 85% high-water mark, and the K3s config overrode only `nodefs` eviction thresholds. `crictl imagefsinfo` showed only ~677M of images, so image GC could not reclaim its requested ~149G and repeatedly evicted application pods. Set matching `imagefs.available` values alongside `nodefs.available` in `eviction-hard`, `eviction-soft`, and `eviction-soft-grace-period`; verify via K3s eviction-manager logs rather than trusting `df` alone.

## Safety Rules

- Do not delete user documents, source code, credentials, volumes, or unknown data without explicit approval.
- Never delete a path solely because its name resembles a cache or build directory.
- Do not clean targets used by running builds or concurrent agents.
- Preserve dirty worktrees and registration/configuration files.
- Stop when evidence is insufficient and return to `disk-space-assessment`.

## Learning Loop

After each cleanup:

1. Add stable, validated remediation knowledge to `references/cleanup-patterns.md`.
2. Add new Rust repository roots to `references/rust-target-roots.txt`.
3. Update assessment-side diagnostic patterns separately; do not turn this skill into a usage-history log.
4. Remove stale guidance and keep automation guarded and reproducible.

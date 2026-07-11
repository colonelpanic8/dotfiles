---
name: disk-space-assessment
description: Measure and explain disk usage without deleting data, producing reusable compressed ncdu snapshots and an evidence-backed cleanup handoff. Use when disk is low, a filesystem is unexpectedly large, builds fail from space pressure, the user asks what is consuming space, or deeper iterative disk analysis is needed before cleanup.
---

# Disk Space Assessment

Build a reproducible picture of disk usage. Stop at findings and proposed actions; use `disk-space-cleanup` when the user authorizes remediation.

Read `references/ignore-paths.md` before scanning. Read `references/observed-heavy-hitters.md` when triaging this machine or interpreting familiar paths.

## Required Artifact Contract

Always create a reusable, timestamped `ncdu` export for every filesystem or major root assessed. Never make an interactive `ncdu` session, transient `/tmp` export, or `du` output the only record of an assessment.

Use the local wrapper:

```bash
safe_ncdu /home
safe_ncdu /nix/store
sudo -n env HOME=/home/imalison safe_ncdu /
```

`safe_ncdu` writes these durable artifacts under `~/.cache/ncdu/`:

- `safe-ncdu-<root>-<timestamp>.json.zst`: compressed importable ncdu data
- `<snapshot>.excludes`: exact exclusions used
- `<snapshot>.meta`: scan root, timestamps, hostname, ncdu version, status, and artifact paths
- `latest-<root>.json.zst` plus matching sidecar symlinks

Keep timestamped files intact for iterative analysis. Report their absolute paths in the handoff. If a scan cannot finish, retain and report the failed manifest, mark the coverage gap, and use bounded probes as supplemental evidence.

## Workflow

1. Record filesystem pressure and topology.
2. Choose scan roots that cover the pressured filesystem without crossing mounts.
3. Create reusable `safe_ncdu` snapshots before any cleanup.
4. Analyze snapshots repeatedly with `top` and `open`; do not rescan for every question.
5. Attribute special stores such as Nix separately.
6. Produce findings, confidence, cleanup candidates, and artifact paths.

## 1. Establish the Baseline

```bash
df -h /
df -h /home
df -h /nix
findmnt -rn -o TARGET,FSTYPE,SOURCE,OPTIONS
```

Record used/free space and whether `/home`, `/nix`, or other large paths are separate mounts. Note active builds or services that may cause measurements to move during the assessment.

## 2. Select Scan Roots

Prefer one-filesystem coverage:

- Scan `/` for root accounting.
- Scan separately mounted `/home` and `/nix/store` independently.
- Add a focused root such as `~/Projects` when the first snapshot identifies it as dominant.
- Use privileged root scans when unprivileged results undercount private service state.

Inspect exclusions before a long scan:

```bash
safe_ncdu excludes /
```

Update `references/ignore-paths.md` and the implementation of `safe_ncdu` together when a newly discovered remote, recursive, or special mount needs a permanent exclusion.

## 3. Create the Snapshots

Run scans early enough that cleanup does not destroy the evidence:

```bash
safe_ncdu /home
safe_ncdu /nix/store
sudo -n env HOME=/home/imalison safe_ncdu /
```

If `safe_ncdu` is unavailable, source or run `/srv/dotfiles/dotfiles/lib/functions/safe_ncdu`. If `ncdu` itself is missing, use Nix temporarily rather than substituting a non-reusable interactive scan.

Do not store the privileged scan in root's home. Set `HOME=/home/imalison` so all artifacts remain together and are available to later sessions.

## 4. Analyze Iteratively

Query the same export at multiple depths:

```bash
safe_ncdu top ~/.cache/ncdu/latest-root.json.zst 30
safe_ncdu top ~/.cache/ncdu/latest-root.json.zst 30 /home/imalison
safe_ncdu top ~/.cache/ncdu/latest-home.json.zst 30 /imalison/Projects
safe_ncdu open ~/.cache/ncdu/latest-root.json.zst
```

Use `du` only for bounded confirmation, live-change checks, or paths missing from the snapshot:

```bash
timeout 30s du -xh --max-depth=1 "$HOME/.cache" 2>/dev/null | sort -h
timeout 30s du -xh --max-depth=1 "$HOME/.local/share" 2>/dev/null | sort -h
```

Distinguish physical allocation from logical/apparent size, especially for sparse files, hardlinked Nix store data, copy-on-write storage, and container layers. Do not sum overlapping Nix closure sizes as reclaim estimates.

## 5. Attribute Nix Store Usage

When `/nix/store` is large, first quantify whether garbage collection can help:

```bash
nix_store_audit --top 30
nix-store --gc --print-dead
nix-store --gc --print-roots
```

Use `/srv/dotfiles/dotfiles/lib/functions/find_store_path_gc_roots` and `nix why-depends` to explain why a large path is retained. Inspect `.direnv/flake-profile-*`, `result*` symlinks, system generations, and current/booted system closures. Prefer `nix_store_audit` over an initial `du -sh /nix/store`, which is slow and does not explain retention.

## Assessment Handoff

Return:

- Baseline filesystem usage and scan time.
- Absolute paths to every snapshot, `.excludes`, and `.meta` artifact.
- Coverage and privilege limitations.
- Largest consumers with evidence from snapshot queries or store attribution.
- Ranked cleanup candidates, expected reclaim range, risk, and whether data is rebuildable.
- Volatile paths or active processes that cleanup must preserve.

Do not delete, prune, garbage-collect, truncate, or mutate data in this skill.

## Learning Loop

After each assessment:

1. Add new permanent scan exclusions to `references/ignore-paths.md` and `safe_ncdu`.
2. Add stable machine-specific diagnostic knowledge to `references/observed-heavy-hitters.md`.
3. Remove stale observations instead of accumulating an unbounded chronological log.

# Auditing `.direnv` Nix GC Roots

Audit direnv roots as a retention graph, not as a list of symlink sizes.

## Required Audit

Run:

```bash
python /srv/dotfiles/dotfiles/agents/skills/disk-space-assessment/scripts/direnv_gc_roots_audit.py --top 30
```

The script writes a timestamped JSON artifact under `~/.cache/ncdu/` and updates `latest-direnv-gc-roots.json`. Preserve the timestamped artifact with the related ncdu snapshots.

## Interpret the Measures

- **Raw roots**: every `.direnv` entry reported by `nix-store --gc --print-roots`; several may point to the same store target.
- **Unique targets**: deduplicated store paths directly referenced by those roots.
- **Project closure**: union of the transitive closures of one project's direnv targets.
- **Collectively direnv-only**: direnv closure paths absent from the union of every observed non-direnv root closure. Removing all direnv roots could make these collectible.
- **Outside non-direnv roots**: one project's closure after subtracting non-direnv-retained paths. This remains an upper bound because other direnv projects may share it.
- **Marginal unique**: paths retained by exactly one direnv project and no non-direnv root. This is the best logical NAR-size estimate of what removing only that project's `.direnv` could make collectible.

Rank projects by marginal unique size, then inspect age, worktree state, active shells/builds, and project importance. Large closure size with near-zero marginal unique size indicates heavy sharing and little immediate benefit from removing that project alone.

## Validate Candidates

For a candidate project:

```bash
find <project>/.direnv -maxdepth 1 -type l -printf '%TY-%Tm-%Td %TH:%TM %p -> %l\n' | sort
git -C <project> status --short
git worktree list --porcelain
ps aux | rg '<project>|direnv|nix develop|nix-shell'
```

Resolve a surprising retained store path with:

```bash
/srv/dotfiles/dotfiles/lib/functions/find_store_path_gc_roots /nix/store/<path>
nix why-depends <profile-or-shell-target> /nix/store/<path>
```

## Limitations

- NAR sizes are logical database sizes, not guaranteed physical bytes reclaimed.
- The graph is a point-in-time view; shells and agents can add or replace roots during the run.
- Nix paths may also be retained by roots created immediately after enumeration.
- Run the audit immediately before and after any cleanup campaign and use actual `df` change as the final measure.

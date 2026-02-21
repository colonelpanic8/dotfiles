# Ignore Paths for Disk Investigation

Use this file to track mountpoints or directories that should be excluded from `ncdu`/`du` scans because they are remote, special-purpose, or noisy.

## Known Ignores

- `$HOME/keybase`
- `/keybase`
- `/var/lib/railbird`
- `/run/user/*/doc` (FUSE portal mount; machine-specific example observed: `/run/user/1004/doc`)

## Discovery Commands

List mounted filesystems and spot special mounts:

```bash
findmnt -rn -o TARGET,FSTYPE,SOURCE
```

Target likely remote/special mounts:

```bash
findmnt -rn -o TARGET,FSTYPE,SOURCE | rg '(keybase|fuse|rclone|s3|railbird)'
```

## Maintenance Rule

When a disk cleanup run encounters a mount or path that should be ignored in future runs, add it here immediately with a short note.

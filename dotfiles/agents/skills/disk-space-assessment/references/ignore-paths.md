# Ignore Paths for Disk Assessment

Exclude these paths from `ncdu`/`du` scans because they are remote, recursive, separately mounted, or noisy.

## Known Ignores

- `$HOME/keybase`
- `$HOME/.cache/keybase`
- `$HOME/.local/share/keybase`
- `$HOME/.config/keybase`
- `/keybase`
- `/var/lib/railbird`
- `/run/user/*/doc` (FUSE portal mount; observed as `/run/user/1004/doc`)

Discover candidates with:

```bash
findmnt -rn -o TARGET,FSTYPE,SOURCE | rg '(keybase|fuse|rclone|s3|railbird)'
```

When adding an ignore, update `/srv/dotfiles/dotfiles/lib/functions/safe_ncdu` in the same change so recorded snapshots actually use it.

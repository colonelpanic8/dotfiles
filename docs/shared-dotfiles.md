# Shared Dotfiles Worktree

This repo is intended to live at `/srv/dotfiles` on shared NixOS machines.
Home Manager links user dotfiles to that shared checkout instead of to
`$HOME/dotfiles`, so the links work consistently for every managed user.

Set it up from any existing checkout:

```sh
just setup-shared-dotfiles
```

The setup command:

- copies the current checkout to `/srv/dotfiles` when needed
- makes the checkout readable by everyone
- makes it writable by the `wheel` group
- sets directory setgid/default ACLs so new files stay group-writable
- configures Git for group sharing
- creates `/etc/nixos -> /srv/dotfiles/nixos` when `/etc/nixos` is absent or already a symlink

Use a different target or group when needed:

```sh
just setup-shared-dotfiles --target /srv/dotfiles --group wheel
```

If a machine has a real `/etc/nixos` directory and you want to replace it with
the shared checkout symlink:

```sh
just setup-shared-dotfiles --force-etc-nixos
```

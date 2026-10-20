- Never create, enter, or use a Git worktree for this repository. Work only in
  the primary checkout (usually at `/srv/dotfiles`).
- Run NixOS activation only as `just switch` from `/srv/dotfiles/nixos`
- Do not add or modify anything in `nixos/imalison.nix` unless the user
  explicitly asks for the change to be made there. Put packages and
  configuration intended for every user in the appropriate shared NixOS or Home
  Manager module instead.
- For rescue media, family installation, remote support, or recovery, read the
  [family installer operating guide](https://github.com/colonelpanic8/dotfiles/blob/master/nixos/family-installer/README.md).
  Updated rescue media includes its pinned copy at
  `/etc/nixos-family-installer/README.md`, with agent instructions alongside it.

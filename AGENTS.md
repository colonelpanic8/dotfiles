# Agent Notes (dotfiles)

## Never use Git worktrees

- Never create, enter, or use a Git worktree for this repository. Work only in
  the primary checkout at `/srv/dotfiles`.
- Do not use Codex/Claude task modes or helpers that create a worktree for this
  repository.
- Do not place worktrees for nested repositories anywhere under
  `/srv/dotfiles`.
- Run NixOS activation only as `just switch` from `/srv/dotfiles/nixos` (or the
  `/etc/nixos` symlink to it). Never activate from another checkout and never
  point `DOTFILES_WORKTREE` at a temporary path.
- This repository uses Home Manager out-of-store links. Activating from a
  disposable checkout makes live dotfiles point into that checkout and breaks
  them when it is removed.

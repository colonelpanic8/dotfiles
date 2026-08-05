This repository is a single git repo rooted at `/srv/dotfiles` on NixOS
machines. This `nixos/` directory is the NixOS flake, but most "user command"
scripts and shell functions live outside of it.

## Where To Put Things

- Shell functions (autoloaded by zsh): `../dotfiles/lib/functions/`
  - These are added to `fpath` in `nixos/environment.nix`, then autoloaded.
- User-executable scripts on PATH: `../dotfiles/lib/bin/`
  - This directory is added to PATH in `nixos/environment.nix` via `${libDir}/bin`.

Avoid dropping scripts in `~/bin` or `~/.local/bin` unless the user explicitly asks.

## NixOS Rebuild Workflow

- Run `just switch` from `/etc/nixos` or `/srv/dotfiles/nixos` (not `nixos-rebuild` directly).
- Host configs live under `machines/`.
- Activate only from the primary `/srv/dotfiles` checkout. `just switch` refuses
  a non-primary one, and overriding `DOTFILES_WORKTREE` to defeat that is a
  mistake: Home Manager's out-of-store symlinks would stay pointed at the
  temporary checkout after it is removed, leaving `~` full of dangling links.
- A rebuild may restart `paseo.service`, killing every Paseo-hosted agent and
  terminal — including you, if you are one. `just switch` detects that it is
  running inside paseo's cgroup and re-executes itself detached via
  `safe_switch`, a tmux session outside the cgroup. Follow or retrieve that run
  with `tmux -L nixos-switch attach -t switch` or
  `tail -f ~/.local/state/nixos-switch/switch.log`.

## Rofi/Tmux Integration Pointers

- Existing rofi scripts live in `../dotfiles/lib/bin/` (e.g. `rofi_command.sh`).
- Keybind locations:
  - Hyprland: `../dotfiles/config/hypr/hyprland/binds.lua`
  - XMonad: `../dotfiles/config/xmonad/xmonad.hs`

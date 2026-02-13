# Agent Notes (dotfiles/nixos)

This repository is a single git repo rooted at `~/dotfiles`. This `nixos/` directory is the NixOS flake, but most "user command" scripts and shell functions live outside of it.

## Where To Put Things

- Shell functions (autoloaded by zsh): `../dotfiles/lib/functions/`
  - These are added to `fpath` in `nixos/environment.nix`, then autoloaded.
- User-executable scripts on PATH: `../dotfiles/lib/bin/`
  - This directory is added to PATH in `nixos/environment.nix` via `${libDir}/bin`.

Avoid dropping scripts in `~/bin` or `~/.local/bin` unless the user explicitly asks.

## NixOS Rebuild Workflow

- Run `just switch` from `~/dotfiles/nixos` (not `nixos-rebuild` directly).
- Host configs live under `machines/`.

## Rofi/Tmux Integration Pointers

- Existing rofi scripts live in `../dotfiles/lib/bin/` (e.g. `rofi_command.sh`).
- Keybind locations:
  - Hyprland: `../dotfiles/config/hypr/hyprland.conf`
  - XMonad: `../dotfiles/config/xmonad/xmonad.hs`


---
name: taffybar-nixos-flake-chain
description: Use when doing NixOS rebuilds involving taffybar, or when flake.lock updates are needed after changing taffybar ecosystem packages. Also use when debugging stale taffybar versions after `just switch`.
---

# Taffybar NixOS Flake Chain

How the taffybar ecosystem packages are consumed by the NixOS configuration through a chain of nested flakes, and what flake.lock updates may be needed when something changes.

See also: `taffybar-ecosystem-release` for the package dependency graph, release workflow, and Hackage publishing.

## The Flake Chain

The NixOS system build pulls in taffybar through the personal
`imalison-taffybar` config flake. The top-level NixOS flake should not declare
or override a direct `taffybar` input; the config flake owns its taffybar
version.

```
nixos/flake.nix                              (top - `just switch` reads this)
│ └── imalison-taffybar  path:../dotfiles/config/taffybar
│
dotfiles/config/taffybar/flake.nix           (middle - imalison-taffybar config)
│ ├── taffybar         path:.../taffybar/taffybar
│ └── gtk-sni-tray, gtk-strut, etc.          (GitHub inputs)
│
dotfiles/config/taffybar/taffybar/flake.nix  (bottom - taffybar library)
│ └── gtk-sni-tray, gtk-strut, etc.          (flake = false GitHub inputs)
```

The NixOS layer may make `imalison-taffybar` follow shared inputs such as
`nixpkgs`, `flake-utils`, and `xmonad`, but it should not set
`imalison-taffybar.inputs.taffybar.follows`.

## Why Bottom-Up Updates Matter

`path:` inputs snapshot the target flake **including its flake.lock** at lock time. If you only run `nix flake update` at the top (nixos) layer, the middle and bottom layers keep whatever was previously locked in their own flake.lock files.

So when propagating a change to a system rebuild, you generally need to update flake.lock files from the bottom up — the bottom layer first so the middle layer picks up fresh locks when it re-resolves, then the middle so the top picks up fresh locks.

```bash
# Bottom (if an ecosystem dep changed):
cd ~/.config/taffybar/taffybar && nix flake update <pkg>

# Middle:
cd ~/.config/taffybar && nix flake update <pkg> taffybar

# Top:
cd ~/dotfiles/nixos && nix flake update imalison-taffybar
```

Not every change requires touching all three layers. Think about which flake.lock files actually contain stale references:

- Changed **taffybar itself** — it's owned by the config flake, so start at the middle (`nix flake update taffybar`) then update `imalison-taffybar` at the top.
- Changed a **leaf ecosystem package** (e.g. gtk-strut) — start at the bottom since taffybar's flake.lock references it, then cascade up.
- The nixos flake can still have unrelated direct inputs such as `kanshi-sni`. Do not add a top-level `taffybar` input just to control the config flake's taffybar source.

## Rebuilding

```bash
cd ~/dotfiles/nixos && just switch
```

If taffybar seems stale after a rebuild, check whether the flake.lock at each layer actually points at the expected revision — a missed cascade step is the usual cause.

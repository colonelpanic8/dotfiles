---
name: taffybar-nixos-flake-chain
description: Use when doing NixOS rebuilds involving taffybar, or when flake.lock updates are needed after changing taffybar ecosystem packages. Also use when debugging stale taffybar versions after `just switch`.
---

# Taffybar NixOS Flake Chain

How the taffybar ecosystem packages are consumed by the NixOS configuration through a chain of nested flakes, and what flake.lock updates may be needed when something changes.

See also: `taffybar-ecosystem-release` for the package dependency graph, release workflow, and Hackage publishing.

## The Three-Layer Flake Chain

The NixOS system build pulls in taffybar through three nested flake.nix files:

```
nixos/flake.nix                              (top — `just switch` reads this)
│ ├── taffybar         path:.../taffybar/taffybar
│ ├── imalison-taffybar  path:../dotfiles/config/taffybar
│ └── gtk-sni-tray, gtk-strut, etc.          (GitHub inputs)
│
dotfiles/config/taffybar/flake.nix           (middle — imalison-taffybar config)
│ ├── taffybar         path:.../taffybar/taffybar
│ └── gtk-sni-tray, gtk-strut, etc.          (GitHub inputs)
│
dotfiles/config/taffybar/taffybar/flake.nix  (bottom — taffybar library)
│ └── gtk-sni-tray, gtk-strut, etc.          (flake = false GitHub inputs)
```

All three flakes declare their own top-level inputs for the ecosystem packages and use `follows` to keep versions consistent within each layer.

## Why Bottom-Up Updates Matter

`path:` inputs snapshot the target flake **including its flake.lock** at lock time. If you only run `nix flake update` at the top (nixos) layer, the middle and bottom layers keep whatever was previously locked in their own flake.lock files.

So when propagating a change to a system rebuild, you generally need to update flake.lock files from the bottom up — the bottom layer first so the middle layer picks up fresh locks when it re-resolves, then the middle so the top picks up fresh locks.

```bash
# Bottom (if an ecosystem dep changed):
cd ~/.config/taffybar/taffybar && nix flake update <pkg>

# Middle:
cd ~/.config/taffybar && nix flake update <pkg> taffybar

# Top:
cd ~/dotfiles/nixos && nix flake update <pkg> imalison-taffybar taffybar
```

Not every change requires touching all three layers. Think about which flake.lock files actually contain stale references:

- Changed **taffybar itself** — it's the bottom layer, so start at the middle (`nix flake update taffybar`) then the top.
- Changed a **leaf ecosystem package** (e.g. gtk-strut) — start at the bottom since taffybar's flake.lock references it, then cascade up.
- The nixos flake also has **direct GitHub inputs** for ecosystem packages with `follows` overrides. Updating those at the top level may be sufficient if nothing changed in the middle/bottom flake.lock files themselves.

## Rebuilding

```bash
cd ~/dotfiles/nixos && just switch
```

If taffybar seems stale after a rebuild, check whether the flake.lock at each layer actually points at the expected revision — a missed cascade step is the usual cause.

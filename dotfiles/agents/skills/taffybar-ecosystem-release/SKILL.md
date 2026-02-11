---
name: taffybar-ecosystem-release
description: Use when releasing, version-bumping, or propagating changes across taffybar GitHub org packages (taffybar, gtk-sni-tray, gtk-strut, status-notifier-item, dbus-menu, dbus-hslogger)
---

# Taffybar Ecosystem Release

Release and propagate changes across the taffybar Haskell package ecosystem.

## Package Dependency Graph

```
taffybar
├── gtk-sni-tray
│   ├── dbus-menu
│   ├── gtk-strut
│   └── status-notifier-item
├── dbus-menu
├── gtk-strut
├── status-notifier-item
└── dbus-hslogger
```

**Leaf packages** (no ecosystem deps): `gtk-strut`, `status-notifier-item`, `dbus-hslogger`, `dbus-menu`
**Mid-level**: `gtk-sni-tray` (depends on dbus-menu, gtk-strut, status-notifier-item)
**Top-level**: `taffybar` (depends on all above)

## Repositories & Local Checkouts

| Package | GitHub | Local Checkout |
|---------|--------|---------------|
| taffybar | taffybar/taffybar | `~/.config/taffybar/taffybar/` |
| gtk-sni-tray | taffybar/gtk-sni-tray | `~/Projects/gtk-sni-tray/` |
| gtk-strut | taffybar/gtk-strut | `~/Projects/gtk-strut/` |
| status-notifier-item | taffybar/status-notifier-item | `~/Projects/status-notifier-item/` |
| dbus-menu | taffybar/dbus-menu | `~/Projects/dbus-menu/` |
| dbus-hslogger | IvanMalison/dbus-hslogger | `~/Projects/dbus-hslogger/` |

## Releasing a Package

Always release leaf packages before their dependents. Changes propagate **upward** through the graph.

### 1. Release the Changed Package

Use the `hackage-release` skill for the full Hackage publish workflow. In the local checkout:

1. Bump version in `.cabal` file (PVP: A.B.C.D)
2. Update ChangeLog.md
3. `cabal build && cabal check`
4. `cabal sdist`
5. Commit, tag `vX.Y.Z.W`, push with tags
6. Publish to Hackage
7. Publish docs

### 2. Update Dependents' Version Bounds

For each package higher in the graph that depends on what you just released, update the dependency bound in its `.cabal` file. For example, if you bumped `gtk-strut` to 0.1.5.0:

- In `gtk-sni-tray.cabal`: update `gtk-strut >= 0.1.5 && < 0.2`
- In `taffybar.cabal`: update `gtk-strut >= 0.1.5 && < 0.2`

Then release those packages too if needed (repeat from step 1).

### 3. Update Flake Inputs

Each package's `flake.nix` references its ecosystem dependencies as inputs (typically `flake = false` pointing at GitHub). After pushing changes, update the flake.lock in any repo that directly references the changed package:

```bash
cd ~/Projects/gtk-sni-tray       # if it depends on what changed
nix flake update gtk-strut
```

```bash
cd ~/.config/taffybar/taffybar   # taffybar references all ecosystem pkgs
nix flake update gtk-strut
```

### Full Ecosystem Release Order

1. `gtk-strut`, `status-notifier-item`, `dbus-hslogger`, `dbus-menu` (leaves — parallel OK)
2. `gtk-sni-tray` (update bounds for any leaf changes first)
3. `taffybar` (update bounds for all changes)

## NixOS Configuration Integration

The personal NixOS config consumes these packages through a chain of flakes. When doing a system rebuild (`just switch` in `~/dotfiles/nixos`), be aware that there are multiple flake.lock files that may need updating depending on what changed.

```
nixos/flake.nix                              (top — `just switch` reads this)
│ ├── taffybar         path:.../taffybar/taffybar
│ ├── imalison-taffybar  path:../dotfiles/config/taffybar
│ └── gtk-sni-tray, gtk-strut, etc.          (GitHub inputs)
│
dotfiles/config/taffybar/flake.nix           (middle — imalison-taffybar)
│ ├── taffybar         path:.../taffybar/taffybar
│ └── gtk-sni-tray, gtk-strut, etc.          (GitHub inputs)
│
dotfiles/config/taffybar/taffybar/flake.nix  (bottom — taffybar library)
│ └── gtk-sni-tray, gtk-strut, etc.          (flake = false GitHub inputs)
```

All three flakes declare their own top-level inputs for the ecosystem packages and use `follows` to keep versions consistent. The key thing to understand: `path:` inputs snapshot the target flake **including its flake.lock** at lock time. So if you update only the top-level nixos flake.lock, the middle and bottom layers will still use whatever was previously locked in *their* flake.lock files.

This means when propagating a change to a system rebuild, you generally need to update flake.lock files **bottom-up** — the bottom layer first so the middle layer picks up fresh locks, then the middle so the top picks up fresh locks:

```bash
# Bottom (if an ecosystem dep changed):
cd ~/.config/taffybar/taffybar && nix flake update <pkg>

# Middle:
cd ~/.config/taffybar && nix flake update <pkg> taffybar

# Top:
cd ~/dotfiles/nixos && nix flake update <pkg> imalison-taffybar taffybar
```

Not every change requires touching all three layers. If you only changed taffybar itself, the bottom layer doesn't need updating since it *is* the bottom. If the nixos flake already uses `follows` to override an input from GitHub directly, updating just that top-level input may be sufficient. Think about which flake.lock files actually contain stale references and update those.

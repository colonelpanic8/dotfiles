---
name: taffybar-ecosystem-release
description: Use when releasing, version-bumping, or propagating changes across taffybar GitHub org packages (taffybar, gtk-sni-tray, gtk-strut, status-notifier-item, dbus-menu, dbus-hslogger)
---

# Taffybar Ecosystem Release

Release and propagate changes across the taffybar Haskell package ecosystem.

See also: `taffybar-nixos-flake-chain` for how these packages are consumed by the NixOS configuration and what flake.lock updates may be needed after a release.

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

**Manual doc upload required for GTK-dependent packages:** Hackage cannot build documentation for packages that depend on GTK/GI libraries (the build servers lack the system dependencies). This affects `taffybar`, `gtk-sni-tray`, `gtk-strut`, and `dbus-menu`. For these packages you must build haddocks locally and upload them yourself — see the `hackage-release` skill for the `cabal haddock --haddock-for-hackage` and `cabal upload --documentation` commands. Only `status-notifier-item` and `dbus-hslogger` (pure DBus/Haskell deps) can have their docs built by Hackage automatically.

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

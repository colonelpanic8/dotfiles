---
name: taffybar-ecosystem-release
description: Use when releasing, version-bumping, or propagating changes across taffybar GitHub org packages (taffybar, gtk-sni-tray, gtk-strut, status-notifier-item, dbus-menu, dbus-hslogger)
---

# Taffybar Ecosystem Release

Release and propagate changes across the taffybar Haskell package ecosystem.

## Package Dependency Graph

```
imalison-taffybar  (user config — ~/.config/taffybar/)
└── taffybar  (main library — ~/.config/taffybar/taffybar/)
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
**Top-level lib**: `taffybar` (depends on all above)
**User config**: `imalison-taffybar` (depends on taffybar + gtk-sni-tray + gtk-strut)

## Repositories & Local Checkouts

| Package | GitHub | Local Checkout |
|---------|--------|---------------|
| taffybar | taffybar/taffybar | `~/.config/taffybar/taffybar/` |
| gtk-sni-tray | taffybar/gtk-sni-tray | `~/Projects/gtk-sni-tray/` |
| gtk-strut | taffybar/gtk-strut | `~/Projects/gtk-strut/` |
| status-notifier-item | taffybar/status-notifier-item | `~/Projects/status-notifier-item/` |
| dbus-menu | taffybar/dbus-menu | `~/Projects/dbus-menu/` |
| dbus-hslogger | IvanMalison/dbus-hslogger | `~/Projects/dbus-hslogger/` |
| imalison-taffybar | (dotfiles repo) | `~/.config/taffybar/` |

## Release Order (Bottom-Up)

Always release leaf packages first, then dependents. Changes propagate **upward**.

### 1. Release the Changed Leaf Package

Use the `hackage-release` skill for the Hackage publish workflow. In the local checkout (`~/Projects/<pkg>/`):

1. Bump version in `.cabal` file (PVP: A.B.C.D)
2. Update ChangeLog.md
3. `cabal build && cabal check`
4. `cabal sdist`
5. Commit, tag `vX.Y.Z.W`, push with tags
6. Publish to Hackage (see hackage-release skill for credentials)
7. Publish docs

### 2. Update Dependents' Version Bounds

For each package that depends on the one you just released, update the dependency bound in its `.cabal` file. For example, if you bumped `gtk-strut` to 0.1.5.0:

- In `gtk-sni-tray.cabal`: update `gtk-strut >= 0.1.5 && < 0.2`
- In `taffybar.cabal`: update `gtk-strut >= 0.1.5 && < 0.2`

Then release those packages if needed (repeat from step 1).

### 3. Update Nix Flake Inputs

After pushing changes to GitHub, update flake.lock files that reference the changed packages:

**taffybar's flake** (`~/.config/taffybar/taffybar/`):
```bash
cd ~/.config/taffybar/taffybar
nix flake update gtk-strut  # or whichever input changed
```

**imalison-taffybar's flake** (`~/.config/taffybar/`):
```bash
cd ~/.config/taffybar
nix flake update gtk-strut  # update the direct input
nix flake update taffybar   # if taffybar itself changed
```

### 4. Rebuild and Test

```bash
cd ~/.config/taffybar
nix develop --command cabal build
```

Or for a full NixOS rebuild:
```bash
cd ~/dotfiles/nixos && just switch
```

## Flake Input Architecture

**taffybar's flake.nix** pulls all ecosystem deps as `flake = false` inputs from GitHub and builds them via `callCabal2nix` through a Nix overlay system.

**imalison-taffybar's flake.nix** re-declares the same inputs as proper flake inputs (where available) and uses `inputs.X.follows` to ensure taffybar uses the same versions:

```
gtk-sni-tray.inputs.gtk-strut.follows = "gtk-strut"
gtk-sni-tray.inputs.status-notifier-item.follows = "status-notifier-item"
gtk-sni-tray.inputs.dbus-menu.follows = "dbus-menu"
taffybar.inputs.gtk-sni-tray.follows = "gtk-sni-tray"
...
```

This means updating a leaf input in the imalison config flake automatically threads it through to taffybar and gtk-sni-tray.

## Common Scenarios

### Changed a leaf package (e.g. status-notifier-item)

1. Make changes in `~/Projects/status-notifier-item/`
2. Release to Hackage if publishing
3. Push to GitHub
4. `cd ~/.config/taffybar && nix flake update status-notifier-item`
5. Rebuild

### Changed taffybar itself

1. Make changes in `~/.config/taffybar/taffybar/`
2. Push to GitHub (or just rebuild — imalison config uses `path:` input)
3. `cd ~/.config/taffybar && nix flake update taffybar`
4. Rebuild

### Changed gtk-sni-tray (mid-level)

1. Make changes in `~/Projects/gtk-sni-tray/`
2. If a leaf dep also changed, update bounds in `gtk-sni-tray.cabal`
3. Push to GitHub
4. `cd ~/.config/taffybar && nix flake update gtk-sni-tray`
5. Rebuild

### Full ecosystem release (all packages)

Release in this exact order:
1. `gtk-strut`, `status-notifier-item`, `dbus-hslogger`, `dbus-menu` (leaves, parallel OK)
2. `gtk-sni-tray` (update bounds for any leaf changes first)
3. `taffybar` (update bounds for all changes)
4. Update all flake inputs in imalison config
5. Rebuild

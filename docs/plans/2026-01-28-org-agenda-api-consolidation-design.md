# Org-Agenda-API Consolidation Design

## Overview

Consolidate org-agenda-api container builds and fly.io deployment into the dotfiles repository. This eliminates the separate `colonelpanic-org-agenda-api` repo and provides:

- Container outputs available to NixOS machines directly
- Fly.io deployment from the same repo
- Fewer repos to maintain
- Cachix integration for faster builds

## Directory Structure

```
/home/imalison/dotfiles/
├── nixos/
│   ├── flake.nix              # Main flake, adds container output
│   ├── org-agenda-api.nix     # Existing tangling module (stays here)
│   └── ...
├── org-agenda-api/
│   ├── container.nix          # Container build logic (mkContainer, etc.)
│   ├── configs/
│   │   ├── colonelpanic/
│   │   │   ├── custom-config.el
│   │   │   └── overrides.el (optional)
│   │   └── kat/
│   │       └── custom-config.el
│   ├── fly/
│   │   ├── fly.toml
│   │   ├── deploy.sh
│   │   └── config-{instance}.env
│   └── secrets/
│       ├── secrets.nix        # agenix declarations
│       └── *.age              # encrypted secrets
└── dotfiles/emacs.d/
    └── org-config.org         # Source of truth for org config
```

## Flake Integration

The main dotfiles flake at `/home/imalison/dotfiles/nixos/flake.nix` exposes container outputs:

```nix
outputs = inputs @ { self, nixpkgs, flake-utils, ... }:
  {
    nixosConfigurations = { ... };  # existing
  } // flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      containerLib = import ../org-agenda-api/container.nix {
        inherit pkgs system;
        tangledConfig = (import ./org-agenda-api.nix {
          inherit pkgs system;
          inputs = inputs;
        }).org-agenda-custom-config;
      };
    in {
      packages = {
        container-colonelpanic = containerLib.mkInstanceContainer "colonelpanic";
        container-kat = containerLib.mkInstanceContainer "kat";
      };
    }
  );
```

Build with: `nix build .#container-colonelpanic`

## Custom Elisp & Tangling

Single source of truth: `org-config.org` tangles to elisp files loaded by containers.

**What stays in custom-config.el (container-specific glue):**
- Path overrides (`/data/org` instead of `~/org`)
- Stubs for unavailable packages (`org-bullets-mode` no-op)
- Customize-to-setq format conversion
- Template conversion for org-agenda-api format
- Instance-specific settings

**Audit:** During implementation, verify no actual org logic is duplicated in custom-config.el.

## Cachix Integration

### Phase 1: Use upstream cache as substituter

Add to dotfiles flake's `nixConfig`:

```nix
nixConfig = {
  extra-substituters = [
    "https://org-agenda-api.cachix.org"
  ];
  extra-trusted-public-keys = [
    "org-agenda-api.cachix.org-1:PUBLIC_KEY_HERE"
  ];
};
```

Benefits:
- `container-base` (~500MB+ dependencies) fetched from cache
- Rebuilds only process the small custom config layer

### Phase 2 (future): Push custom builds

Set up GitHub Action or local push for colonelpanic-specific container builds.

## Fly.io Deployment

**What moves:**
- `fly.toml` → `dotfiles/org-agenda-api/fly/fly.toml`
- `deploy.sh` → `dotfiles/org-agenda-api/fly/deploy.sh`
- `configs/*/config.env` → `dotfiles/org-agenda-api/fly/config-{instance}.env`
- Agenix secrets → `dotfiles/org-agenda-api/secrets/`

**Deploy script changes:**
- Build path: `nix build "../nixos#container-${INSTANCE}"`
- Secrets path adjusts to new location
- Otherwise same logic

## Implementation Phases

### Phase 1: Pull latest & verify current state
- Pull latest changes in org-agenda-api and colonelpanic-org-agenda-api
- Build container, verify it works
- Fix any issues before restructuring

### Phase 2: Create dotfiles structure
- Create `/home/imalison/dotfiles/org-agenda-api/` directory
- Move container.nix logic (adapted from current colonelpanic-org-agenda-api flake)
- Move instance configs (colonelpanic/, kat/)
- Move fly.io deployment files
- Move agenix secrets

### Phase 3: Integrate with dotfiles flake
- Update `/home/imalison/dotfiles/nixos/flake.nix` to expose container outputs
- Add cachix substituter configuration
- Test build from dotfiles: `nix build .#container-colonelpanic`

### Phase 4: Verify deployment
- Test deploy.sh from new location
- Verify fly.io deployment works
- Run the container locally on a NixOS machine

### Phase 5: Audit & cleanup
- Review custom-config.el for any duplicated org logic
- Archive colonelpanic-org-agenda-api repo
- Update any references/documentation

## Repos Affected

- **dotfiles** - Receives container build + fly.io deployment
- **colonelpanic-org-agenda-api** - Becomes obsolete after migration
- **org-agenda-api** (upstream) - No changes, used as flake input

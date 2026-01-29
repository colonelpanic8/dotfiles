---
name: bump-overlay-versions
description: Use when user asks to bump, update, or upgrade claude-code or codex versions in overlay.nix, or when checking if newer versions are available
---

# Bump Overlay Versions

## Overview

Updates claude-code and/or codex to latest versions in `~/dotfiles/nixos/overlay.nix`. Nix requires correct hashes which must be discovered through failed builds.

## Quick Reference

| Package | Check Latest | Hash Fields |
|---------|--------------|-------------|
| claude-code | `npm view @anthropic-ai/claude-code version` | `hash`, `npmDepsHash` (keep empty) |
| codex | `curl -s "https://api.github.com/repos/openai/codex/releases/latest" \| jq -r '.tag_name'` | `hash`, `cargoHash` |

## Workflow

### 1. Check Current vs Latest Versions

```bash
# Current versions are in overlay.nix claudeCodeVersion and codexVersion blocks
# Check latest:
npm view @anthropic-ai/claude-code version
curl -s "https://api.github.com/repos/openai/codex/releases/latest" | jq -r '.tag_name'
# codex tag format: rust-v0.92.0 -> version is 0.92.0
```

### 2. Update Version and Clear Hashes

In `~/dotfiles/nixos/overlay.nix`:

**For claude-code:**
```nix
claudeCodeVersion = {
  version = "X.Y.Z";        # Update to new version
  hash = "";                # Clear - will get from build error
  npmDepsHash = "";         # Keep empty (no npm deps)
};
```

**For codex:**
```nix
codexVersion = {
  version = "X.Y.Z";        # Update to new version (without rust-v prefix)
  hash = "";                # Clear - will get from build error
  cargoHash = "";           # Clear - will get from build error
};
```

### 3. Build to Get Hashes

```bash
just switch
```

The build will fail with hash mismatch errors showing the correct hash. Copy the `got:` hash value.

**For codex:** You'll need to run twice - once for `hash` (source), once for `cargoHash` (cargo deps).

### 4. Update with Correct Hashes

Replace empty strings with the hashes from build errors, then run `just switch` again.

## Enable/Disable Overrides

The overlay uses boolean flags - set to `false` to use nixpkgs versions instead:

```nix
enableCodexOverride = true;       # Set false to use nixpkgs codex
enableClaudeCodeOverride = true;  # Set false to use nixpkgs claude-code
```

**Never delete the override code** - just toggle the flags.

## File Location

`~/dotfiles/nixos/overlay.nix`
